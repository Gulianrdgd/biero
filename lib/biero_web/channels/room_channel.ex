defmodule BieroWeb.RoomChannel do
  use BieroWeb, :channel
  require Ecto.Query
  alias Biero.{User, Team, Repo, ChannelWatcher, Encryption}
  require Logger

  def join("biero:admin", params, socket) do
    username = params["username"]
    token = params["token"]
    case is_nil(username) or is_nil(token) do
      false -> case User |> Ecto.Query.where(username: ^username) |> Repo.exists? do
                 true -> case Phoenix.Token.verify(BieroWeb.Endpoint, "user auth", token, max_age: 86400) do
                           {:ok, username} -> {:ok = ChannelWatcher.monitor(:chat, self(), {__MODULE__, :leave, [username]}), socket}
                           _ -> {:error, "NO"}
                         end
                 false -> {:error, "NO"}
               end
      true -> {:error, "NO"}
    end
  end

  def join("biero:front", _params, socket) do
    {:ok, socket}
  end

  def handle_in("shout", payload, socket) do
    #    payload {"body" => "message", "name" => "username"}
    #    topic is vissible in socket
    case payload["body"] do
      "?getTable" ->
        "biero:" <> room = socket.topic
        if room == "admin" and Encryption.checkToken(payload["username"], payload["token"]) do
          case payload["table"] do
            "Teams" -> payload = %{"body" => "?newTable", "table" => Team.getTeams(), "type" =>"Teams"}
                       broadcast socket, "shout", payload
            "Users" -> payload = %{"body" => "?newTable", "table" => User.getUsers(), "type" => "Users"}
                       broadcast socket, "shout", payload
            _       -> payload = %{"body" => "?err"}
                       broadcast socket, "shout", payload
          end
        end
        if room == "front" do
          payload = %{"body" => "?newTable", "table" => Team.getTeams(), "type" =>"Teams"}
          broadcast socket, "shout", payload
        end
        {:noreply, socket}
      "?sendChanges" ->
        "biero:" <> room = socket.topic
        if (room == "admin" and Encryption.checkToken(payload["username"], payload["token"])) do
          case [length(payload["newUsers"]), length(payload["newTeams"])] do
            [0, 0] ->
                      {:noreply, socket}
            [_, 0] -> payload = %{"body" => "?newTable", "table" => User.setUser(payload["newUsers"], payload["token"]), "type" => "Users"}
                      broadcast socket, "shout", payload
                      {:noreply, socket}
            [0, _] -> payload = %{"body" => "?newTable", "table" => Team.setTeam(payload["newTeams"]), "type" => "Team"}
                      BieroWeb.Endpoint.broadcast("biero:front", "shout", payload)
                      broadcast socket, "shout", payload
                      {:noreply, socket}
            _ ->      payload2 = %{"body" => "?newTable", "tableTeam" => Team.setTeam(payload["newTeams"]), "tableUser" => User.setUser(payload["newUsers"], payload["token"]), "type" => "Both"}
                      broadcast socket, "shout", payload2
                      payloadFront = %{"body" => "?newTable", "table" => Team.setTeam(payload["newTeams"]), "type" => "Team"}
                      BieroWeb.Endpoint.broadcast("biero:front", "shout", payloadFront)
                      {:noreply, socket}
          end
        end
      _ ->
        "biero:" <> room = socket.topic
        payload = Map.merge(payload, %{"room" => room})
        broadcast socket, "shout", payload
        {:noreply, socket}
    end
  end


  def leave(_user_id) do
    Logger.info("Leaving!")
  end
end

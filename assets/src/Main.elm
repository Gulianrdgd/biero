port module Main exposing (..)

import Browser
import Html exposing (Html, div)
import Html.Attributes exposing (class)
import Json.Decode exposing (decodeString, keyValuePairs, string)
import List exposing (head, reverse)


-- MAIN

main : Program { url : String } RoundInfo Msg
main =
  Browser.element { init = \{ url } -> ( init url, Cmd.none ), update = update, subscriptions = subscriptions, view = view }


-- MODEL
port sendMessage : String -> Cmd msg
port messageReceiver : (String -> msg) -> Sub msg


type alias RoundInfo =
  { username : String
  , token : String
  }


init : String -> RoundInfo
init url =
  RoundInfo "" ""

subscriptions : RoundInfo -> Sub Msg
subscriptions model =
    messageReceiver Recv


type Msg
  = Recv String


update : Msg -> RoundInfo -> ( RoundInfo, Cmd Msg )
update msg model =
  case msg of
    _ -> (model, Cmd.none)

view : RoundInfo -> Html Msg
view model =
    div [class "container fade"] [
    ]


decode : String  -> List(String, String)
decode json = case decodeString (keyValuePairs string) json of
                Ok x -> x
                Err _ -> case decodeString (Json.Decode.list (Json.Decode.list (string))) json of
                            Ok val -> List.map jsonArr val
                            Err _ ->  [("no", "no")]


jsonArr : List String -> (String, String)
jsonArr lst = case head lst of
                Just user -> case head (reverse lst) of
                        Just val -> (user, val)
                        _ -> ("no", "no")
                _ -> ("no", "no")

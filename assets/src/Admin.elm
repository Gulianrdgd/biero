port module Admin exposing (..)

import Browser
import Dict exposing (Dict, get)
import Html exposing (Attribute, Html, a, aside, button, div, img, input, li, nav, p, tbody, text, ul)
import Html.Attributes exposing (class, href, placeholder, src, style)
import Html.Attributes.Aria exposing (role)
import Html.Events exposing (onClick, onInput)
import Json.Decode exposing (decodeString, dict, errorToString, field, keyValuePairs, string)
import Json.Encode as Encode exposing (list)
import List exposing (map)
import List.Extra exposing (find)
import String exposing (fromInt, toInt)
import Table exposing (Column, unsortable)

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
  , tableData : List(Dict String String)
  , selected : Selected
  , action : Action
  , tableState : Table.State
  , query : String
  , changes : Change
  }

type Selected =
    Users
    | Teams
    | None

selectedToString : Selected -> String
selectedToString x = case x of
                        Users -> "Users"
                        Teams -> "Teams"
                        None -> "None"

type Action =
    Create
    | Edit
    | Delete
    | Null

type alias User =
  { username : String
  , isAdmin : String
  , team : String
  }

type alias Team =
  { name : String
  , users : String
  , etappe : Int
  }

type alias Change =
    { teamList : List Team
    , userList : List User
    }

init : String -> RoundInfo
init url =
  RoundInfo "" "" [] None Null (Table.initialSort "Year") "" (Change [] [])

subscriptions : RoundInfo -> Sub Msg
subscriptions model =
    messageReceiver Recv



configUser : Table.Config User Msg
configUser =
  Table.config
    { toId = .username
    , toMsg = SetTableState
    , columns =
        [ Table.stringColumn "Username" .username
        , Table.stringColumn "isAdmin" .isAdmin
        , Table.stringColumn "Teamname" .team
        ]
    }

configTeam : Table.Config Team Msg
configTeam =
  Table.config
    { toId = .name
    , toMsg = SetTableState
    , columns =
        [ Table.stringColumn "Teamname" .name
        , Table.stringColumn "Users" .users
        , Table.intColumn "etappe" .etappe
        , addColumn "" .name
        ]
    }

type Msg
  = Recv String
  | Selected Selected Action
  | SetQuery String
  | SetTableState Table.State
  | ChangeTeam String String String
  | SendChanges Change

update : Msg -> RoundInfo -> ( RoundInfo, Cmd Msg )
update msg model =
  case msg of
    Selected s x -> ({ model | selected = s, action = x, tableData = []}, sendMessage (Encode.encode 0 (Encode.object[ ("message",Encode.string "?getTable")
                                                                                                                             , ( "username", Encode.string model.username )
                                                                                                                             , ( "token", Encode.string model.token)
                                                                                                                             , ( "table", Encode.string (selectedToString s))
                                                                                                                             ])))
    Recv s -> case decode s of
              [("?username", user)] -> ({ model | username = user}, Cmd.none)
              [("?token", token)] -> ({ model | token = token}, Cmd.none)
              [("no", "no")] -> ({ model | tableData = getArrString s}, Cmd.none)
              _ -> (model, Cmd.none)
    SetQuery newQuery ->
      ({ model | query = newQuery }, Cmd.none)

    SetTableState newState ->
      ({ model | tableState = newState }, Cmd.none)
    ChangeTeam user op newUsers ->
        ({ model | changes = changeTeam model.changes user op newUsers}, Cmd.none)
    SendChanges change ->
        ({model | changes = (Change [] [])}, sendMessage (Encode.encode 0 (Encode.object[   ("message",Encode.string "?getTable")
                                                                                          , ( "username", Encode.string model.username )
                                                                                          , ( "token", Encode.string model.token)
                                                                                          , ( "newTeams", Encode.list teamToObject change.teamList)
                                                                                          , ( "newUsers", Encode.list userToObject change.userList)
                                                                                          ])))


view : RoundInfo -> Html Msg
view model =
    div [style "height" "100%"][
    nav [class "navbar has-background-primary"] [
      div [class "navbar-brand"] [
        a [class "navbar-item", href "https://vdhorst.dev"] [
          img [src "https://bulma.io/images/bulma-logo.png", class "logo"] []
          ],
        a [role "button", class "navbar-burger"] []
      ],
      div [class "navbar-menu"][
        div [class "navbar-start"][]
        ]],
    div [class "container fade columns"] [
        div [class "column is-one-fifth"] [
        aside [class "menu"][
          p [class "menu-label has-text-white"][text "Teams"],
          ul [class "menu-list"] [
            li [] [
               a [class "has-text-white", onClick (Selected Teams Create)] [text "create"]
            ],
            li [] [
               a [class "has-text-white", onClick (Selected Teams Edit)] [text "edit"]
            ],
            li [] [
               a [class "has-text-white", onClick (Selected Teams Delete)] [text "remove"]
            ]
          ],
            p [class "menu-label has-text-white"][text "Users"],
            ul [class "menu-list"] [
              li [] [
                 a [class "has-text-white", onClick (Selected Users Create)] [text "create"]
              ],
              li [] [
                 a [class "has-text-white", onClick (Selected Users Delete)] [text "remove"]
              ]
            ]
        ]
        ],
        div [class "column has-background-white"] [
            createTable model model.tableData
        ]
    ]
    ]

createTable : RoundInfo -> List(Dict String String) -> Html Msg
createTable model x = let
                          lowerQuery =
                            String.toLower model.query

                          acceptableUsers =
                            List.filter (String.contains lowerQuery << String.toLower << .username) (tableUsers model.tableData)

                          acceptableTeams =
                            List.filter (String.contains lowerQuery << String.toLower << .name) (tableTeams model.tableData)
                        in div [class "container", style "width" "80%"] [
                    case model.selected of
                        Teams -> case model.action of
                                    Create -> div [][]
                                    _ ->
                                        div []
                                              [
                                              div [class "columns"] [
                                                  div [class "column"] [
                                                    input [ placeholder "Search by Name", onInput SetQuery ] []
                                                  ],
                                                  div [class "column"] [
                                                    button [] [text "Apply changes"]
                                                  ]
                                              ]
                                              , Table.view configTeam model.tableState acceptableTeams
                                              ]

                        Users -> case model.action of
                                    Create -> div [][]
                                    _ ->
                                        div []
                                              [
                                              div [class "columns"] [
                                                  div [class "column"] [
                                                    input [ placeholder "Search by Name", onInput SetQuery ] []
                                                  ],
                                                  div [class "column"] [
                                                    button [ onClick (SendChanges model.changes)] [text "Apply changes"]
                                                  ]
                                              ]
                                              , Table.view configUser model.tableState acceptableUsers
                                              ]

                        None -> div [] []
                    ,
                    case model.tableData of
                        [] ->  div [] [text "loading ..."]
                        _ -> tbody [] []

                ]


decode : String  -> List(String, String)
decode json = case decodeString (keyValuePairs string) json of
                Ok x -> x
                Err _ -> [("no", "no")]


getArrString : String -> List (Dict String String)
getArrString str = case decodeString (field "table" (Json.Decode.list (dict string))) str of
                    Ok x -> x
                    Err err -> [Dict.fromList [("err", errorToString err)]]


tableUsers : List(Dict String String) -> List User
tableUsers x = map (\y -> User (getStringFromDict y "username") (getStringFromDict y "hasAdmin") (getStringFromDict y "username")) x


tableTeams : List(Dict String String) -> List Team
tableTeams x = map (\y -> Team (getStringFromDict y "name") (getStringFromDict y "users") (stringToInt (getStringFromDict y "etappe"))) x

stringToInt : String -> Int
stringToInt s = case toInt s of
                 Just x -> x
                 Nothing -> 0

addColumn : String -> (data -> String) -> Column data msg
addColumn name _ =
    Table.veryCustomColumn
        { name = name
        , viewData = \_ -> (viewEtappe name)
        , sorter = unsortable
        }

viewEtappe : String -> Table.HtmlDetails msg
viewEtappe name =
    Table.HtmlDetails []
        [
         button [] [text "+"],
         button [] [text "-"]
        ]


getStringFromDict : (Dict String String) -> String -> String
getStringFromDict dic key = case get key dic of
                               Just x -> x
                               Nothing -> ""


teamToObject : Team -> Encode.Value
teamToObject t = Encode.object[ ("team", Encode.string t.name)
                                , ("users", Encode.string t.users)
                                , ("etappe", Encode.int t.etappe)]


userToObject : User -> Encode.Value
userToObject u = Encode.object[ ("username", Encode.string u.username)
                              , ("isAdmin", Encode.string u.isAdmin)
                              , ("team", Encode.string u.team)]

changeTeam : Change -> String -> String -> String -> Change
changeTeam changes team op newUsers = case find (\el -> el.name == team) changes.teamList of
                                Just _ -> Change (map (\val -> if val.name == team then
                                                                            case op of
                                                                               "+" -> Team val.name (val.users ++ newUsers) (val.etappe + 1)
                                                                               "-" -> Team val.name (val.users ++ newUsers) (val.etappe - 1)
                                                                               "none" -> Team val.name (val.users ++ newUsers) val.etappe
                                                                               _ -> val
                                                       else val
                                                                               ) changes.teamList) changes.userList
                                Nothing -> case op of
                                            "+" -> Change (changes.teamList ++ [Team team newUsers 1]) changes.userList
                                            "-" -> Change (changes.teamList ++ [Team team newUsers -1]) changes.userList
                                            _ -> changes
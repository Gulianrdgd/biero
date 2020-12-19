port module Admin exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Dict exposing (Dict, get)
import Html exposing (Attribute, Html, a, aside, button, div, img, input, li, nav, p, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, href, placeholder, src, style, value)
import Html.Attributes.Aria exposing (role)
import Html.Events exposing (keyCode, on, onClick, onInput)
import Json.Decode exposing (decodeString, dict, errorToString, field, keyValuePairs, string)
import Json.Encode as Encode
import List exposing (length, map)
import List.Extra exposing (find)
import String exposing (fromInt, toInt)

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
  , query : String
  , changes : Change
  , changedFieldTemp : String
  , selectedRow : String
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
  RoundInfo "" "" [] None Null "" (Change [] []) "" ""

subscriptions : RoundInfo -> Sub Msg
subscriptions model =
    messageReceiver Recv


type Msg
  = Recv String
  | Selected Selected Action
  | SetQuery String
  | SendChanges Change
  | SetChange Change
  | FocusOut String
  | Input String
  | SelectedRow String String

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
    SetChange change ->
        ({model | changes = change} , Cmd.none)
    SendChanges change ->
        ({model | changes = (Change [] [])}, sendMessage (Encode.encode 0 (Encode.object[   ("message", Encode.string "?sendChanges")
                                                                                          , ( "username", Encode.string model.username )
                                                                                          , ( "token", Encode.string model.token)
                                                                                          , ( "newTeams", Encode.list teamToObject change.teamList)
                                                                                          , ( "newUsers", Encode.list userToObject change.userList)
                                                                                          ])))
    Input s ->
        ({model | changedFieldTemp = s}, Cmd.none)
    FocusOut name ->
            ({ model | changes = changeTeam model.changes name "none" model.changedFieldTemp, changedFieldTemp = "", selectedRow = ""}, Cmd.none)
    SelectedRow name users ->
        ({model | selectedRow = name, changedFieldTemp = users}, Cmd.none)

view : RoundInfo -> Html Msg
view model =
    div [class "container fade columns", style "height" "100vh"] [
        div [class "column is-one-fifth", style "background" "purple"] [
            div [] [
                img [src "/images/logo.png", class "logo"] []
            ],
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
                                                createButton model
                                              , createTableTeams model acceptableTeams
                                              ]

                        Users -> case model.action of
                                    Create -> div [][]
                                    _ ->
                                        div []
                                              [
                                              createButton model
                                              ,createTableUser acceptableUsers
                                              ]

                        None -> div [] []
                    ,
                    case model.tableData of
                        [] ->  div [] [text "loading ..."]
                        _ -> tbody [] []

                ]

createButton : RoundInfo -> Html Msg
createButton model = div [class "columns"] [
                    div [class "column"] [
                        input [ placeholder "Search by Name", onInput SetQuery ] []
                    ],
                div [class "column"] [
                    button [class "button", onClick (SendChanges model.changes)] [
                     p [] [text "Apply changes "],
                     p [class "has-text-success", style "margin-left" "10px"] [text (fromInt((length model.changes.teamList) + (length model.changes.userList)))]
                     ]
                ]
                ]

createTableUser : List User -> Html Msg
createTableUser lst = table [class "table"] [
                                thead [] [
                                    tr [] [
                                        td [] [text "Username"],
                                        td [] [text "IsAdmin"],
                                        td [] [text "Team"]
                                    ]
                                ], (lst) |> map (\x -> tr [] [
                                                            td [] [text x.username],
                                                            td [] [text x.isAdmin],
                                                            td [] [text x.team]
                                                        ]) |> tbody []
                            ]


createTableTeams : RoundInfo -> List Team -> Html Msg
createTableTeams model lst = table [class "table"] [
                                thead [] [
                                    tr [] [
                                        td [] [text "Teamname"],
                                        td [] [text "Users"],
                                        td [] [text "Etappe"],
                                        td [] [text "Change Etappe"]
                                    ]
                                ], (lst) |> map (\x -> tr [] [
                                                            td [] [text x.name],
                                                            td [] [
                                                                if model.selectedRow == x.name then
                                                                    input [value (getUsers (model.changedFieldTemp) (getChangeString x.name model)), onInput Input, onFocusOut (FocusOut x.name)] []
                                                                else
                                                                   p [onClick (SelectedRow x.name x.users)] [text (getUsers x.users (getChangeString x.name model))]
                                                               ],
                                                            td [] [text (fromInt (x.etappe + (getChangeInt x.name model)))],
                                                            td [] [
                                                                button [class "button", onClick (SetChange (changeTeam model.changes x.name "+" x.users))] [text "+"],
                                                                button [class "button", onClick (SetChange (changeTeam model.changes x.name "-" x.users))] [text "-"]
                                                            ]
                                                        ]) |> tbody []
                            ]


decode : String  -> List(String, String)
decode json = case decodeString (keyValuePairs string) json of
                Ok x -> x
                Err _ -> [("no", "no")]


getUsers : String -> String -> String
getUsers str1 str2 = case (str1, str2) of
                        ("", "") -> "None"
                        ("", x) -> x
                        (x, "") -> x
                        (_, y) -> y


getChangeString : String -> RoundInfo -> String
getChangeString name model = case find (\x -> x.name == name) model.changes.teamList of
                                    Just y -> y.users
                                    Nothing -> ""


getChangeInt : String -> RoundInfo -> Int
getChangeInt name model = case find (\x -> x.name == name) model.changes.teamList of
                                  Just y -> y.etappe
                                  Nothing -> 0


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


getStringFromDict : (Dict String String) -> String -> String
getStringFromDict dic key = case get key dic of
                               Just x -> x
                               Nothing -> "No"


teamToObject : Team -> Encode.Value
teamToObject t = Encode.object[ ("team", Encode.string t.name)
                                , ("users", Encode.string t.users)
                                , ("etappe", Encode.int t.etappe)]


userToObject : User -> Encode.Value
userToObject u = Encode.object[ ("username", Encode.string u.username)
                              , ("isAdmin", Encode.string u.isAdmin)
                              , ("team", Encode.string u.team)]


onFocusOut : msg -> Attribute msg
onFocusOut message =
  on "focusout" (Json.Decode.succeed message)

onFocusIn : msg -> Attribute msg
onFocusIn message =
  on "focusin" (Json.Decode.succeed message)


changeTeam : Change -> String -> String -> String -> Change
changeTeam changes team op newUsers = case find (\el -> el.name == team) changes.teamList of
                                Just _ -> Change (map (\val -> if val.name == team then
                                                                            case op of
                                                                               "+" -> Team val.name newUsers (val.etappe + 1)
                                                                               "-" -> Team val.name newUsers (val.etappe - 1)
                                                                               "none" -> Team val.name newUsers val.etappe
                                                                               _ -> val
                                                       else val
                                                                               ) changes.teamList) changes.userList
                                Nothing -> case op of
                                            "+" -> Change (changes.teamList ++ [Team team newUsers 1]) changes.userList
                                            "-" -> Change (changes.teamList ++ [Team team newUsers -1]) changes.userList
                                            "none" -> Change (changes.teamList ++ [Team team newUsers 0]) changes.userList
                                            _ -> changes
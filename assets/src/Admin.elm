port module Admin exposing (..)

import Browser
import Color exposing (Color, toRgba)
import Dict exposing (Dict, get)
import Hex
import Html exposing (Attribute, Html, a, aside, button, div, h1, h5, img, input, label, li, p, small, table, tbody, td, text, thead, tr, ul)
import Html.Attributes exposing (attribute, class, placeholder, src, style, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode exposing (decodeString, dict, errorToString, field, keyValuePairs, string)
import Json.Encode as Encode
import List exposing (filter, head, length, map, reverse, tail)
import List.Extra exposing (find)
import String exposing (fromInt, split, toInt)
import Toasty
import ColorPicker

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
  , changedFieldTempPass : String
  , changedFieldTempPass2 : String
  , changeNameTemp : String
  , selectedRow : String
  , toasties : Toasty.Stack String
  , colorPicker : ColorPicker.State
  , colour : Color
  , selectedWheel : String
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
    | Null

type alias User =
  { username : String
  , isAdmin : String
  , delete : Bool
  , password : String
  , password2 : String
  }

type alias Team =
  { name : String
  , users : String
  , etappe : Int
  , delete : Bool
  , color : String
  }

type alias Change =
    { teamList : List Team
    , userList : List User
    }

myConfig : Toasty.Config msg
myConfig =
    Toasty.config
        |> Toasty.transitionOutDuration 700
        |> Toasty.delay 8000
        |> Toasty.containerAttrs containerAttrs

containerAttrs =
    [ style "max-width" "300px"
    , style "position" "fixed"
    , style "right" "0"
    , style "top" "0"
    , style "list-style-type" "none"
    ]

init : String -> RoundInfo
init url =
  RoundInfo "" "" [] None Null "" (Change [] []) "" "" "" "" "" Toasty.initialState ColorPicker.empty (Color.rgb 255 0 0) ""

subscriptions : RoundInfo -> Sub Msg
subscriptions model =
    messageReceiver Recv


type Msg
  = Recv String
  | Selected Selected Action
  | SetQuery String
  | SendChanges Change
  | SetChange Change
  | FocusOut String String
  | FocusOutCreate String
  | Input String String
  | SelectedRow String String
  | DeleteFromChanges String String
  | ToastyMsg (Toasty.Msg String)
  | ColorPickerMsg ColorPicker.Msg
  | GetColorWheel String

update : Msg -> RoundInfo -> ( RoundInfo, Cmd Msg )
update msg model =
  case msg of
    Selected s x -> ({ model | selected = s, action = x, tableData = [],  changedFieldTemp = "", selectedRow = "", changeNameTemp = ""}, sendMessage (Encode.encode 0 (Encode.object[ ("message",Encode.string "?getTable")
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
        ({model | changes = change, selectedWheel=""} , Cmd.none)
    SendChanges change ->
        ({model | changes = (Change [] []), changedFieldTemp = "", changeNameTemp = ""}, sendMessage (Encode.encode 0 (Encode.object[   ("message", Encode.string "?sendChanges")
                                                                                          , ( "username", Encode.string model.username )
                                                                                          , ( "token", Encode.string model.token)
                                                                                          , ( "newTeams", Encode.list teamToObject change.teamList)
                                                                                          , ( "newUsers", Encode.list userToObject change.userList)
                                                                                          ]))) |> Toasty.addToast myConfig ToastyMsg "Change sent!"
    Input kind s -> case kind of
                    "createName" ->
                         ({model | changeNameTemp = s}, Cmd.none)
                    "changePass" ->
                         ({model | changedFieldTempPass = s}, Cmd.none)
                    "changePass2" ->
                         ({model | changedFieldTempPass2 = s}, Cmd.none)
                    _ ->
                        ({model | changedFieldTemp = s}, Cmd.none)
    FocusOut name action ->
            case action of
                "teamsEdit" ->
                    ({ model | changes = changeTeam model.changes name "none" model.changedFieldTemp "" False, selectedRow = "", changedFieldTemp = "", changeNameTemp = ""}, Cmd.none)
                "userCreatePas" ->
                    ({ model | changes = changeUser model.changes name "none" model.changedFieldTempPass "" "changePass", selectedRow = "", changedFieldTemp = "", changeNameTemp = ""}, Cmd.none)
                "userCreatePas2" ->
                    ({ model | changes = changeUser model.changes name "none" model.changedFieldTempPass model.changedFieldTempPass2 "changePass", selectedRow = "", changedFieldTemp = "", changeNameTemp = "", changedFieldTempPass = "", changedFieldTempPass2 = ""}, Cmd.none)
                _ -> (model, Cmd.none)
    FocusOutCreate name ->
              ({ model | changes = changeTeam model.changes name "none" model.changedFieldTemp "none" False, selectedRow = ""}, Cmd.none)
    SelectedRow name users ->
        case getChangeString name model "users" of
            "" ->
                ({model | selectedRow = name, changedFieldTemp = users}, Cmd.none)
            x ->
                ({model | selectedRow = name, changedFieldTemp = x}, Cmd.none)
    ToastyMsg subMsg ->
         Toasty.update myConfig ToastyMsg subMsg model
    DeleteFromChanges kind name ->
        ({model | changes = case kind of
                                "Teams" -> changeTeam model.changes name "none" "" "" True
                                _ -> model.changes
               }, Cmd.none)
    ColorPickerMsg ms ->
                let
                    ( m, colour ) =
                        ColorPicker.update ms model.colour model.colorPicker
                in
                    ({ model
                        | colorPicker = m
                        , colour = colour |> Maybe.withDefault model.colour
                    }, Cmd.none)
    GetColorWheel val -> ({model | selectedWheel = val }, Cmd.none)


view : RoundInfo -> Html Msg
view model =
    div [class "container bg-white", style "max-width" "1750px"] [
    Toasty.view myConfig renderToast ToastyMsg model.toasties,
    div [class "row", style "height" "100vh"] [
        div [class "col-3 overflow-hidden bg-dark", style "max-width" "20%"] [
            div [] [
                img [src "/images/logo.png", class "logo"] []
            ],
        aside [class ""][
          p [class "text-white mt-3 fs-4"][text "Teams"],
          ul [class "nav nav-tabs flex-column "] [
            li [class "nav-item"] [
               a [class "nav-link fs-5", onClick (Selected Teams Create)] [text "create"]
            ],
            li [class "nav-item"] [
               a [class "nav-link fs-5", onClick (Selected Teams Edit)] [text "edit"]
            ]
          ],
            p [class "text-white mt-3 fs-4"][text "Users"],
            ul [class "nav nav-tabs flex-column", style "text-decoration" "none"] [
              li [class "nav-item rounded"] [
                 a [class "nav-link fs-5", onClick (Selected Users Create)] [text "Create"]
              ],
              li [class "nav-item rounded"] [
                 a [class "nav-link fs-5", onClick (Selected Users Edit)] [text "Edit"]
              ]
            ]
        ]
        ],
        div [class "col mt-4"] [
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
                                    Create -> div [class ""][
                                                        div [class "centered align-middle", style "margin-top" "10%"] [
                                                        h1 [class "mb-3"] [text "Create a new team"],
                                                          div [class "form-group"] [
                                                            label [] [text "TeamName"],
                                                            input [value model.changeNameTemp, onInput (Input "createName"), onFocusOut (FocusOutCreate model.changeNameTemp), class "form-control", placeholder "Enter team name"][]
                                                          ],
                                                          div [class "form-group"][
                                                            label [][text "Players"],
                                                            input [value model.changedFieldTemp, onInput (Input "createPlayers"), onFocusOut (FocusOutCreate model.changeNameTemp), class "form-control", placeholder "players"][],
                                                            small [class "form-text text-muted"] [text "Please enter names with comma separation"]
                                                          ],
                                                          button [class "btn btn-primary mb-auto", onClick (SendChanges model.changes)] [text "Submit"]
                                                        ], div [class "mt-5"] [
                                                            h1 [class "mb-3"] [text "Current teams"],
                                                            createTableTeamsCreate acceptableTeams]
                                                    ]
                                    _ ->
                                        div [class ""]
                                              [
                                                div [class "centered align-middle", style "margin-top" "10%"] [
                                                    h1 [class "mb-3"] [text "Edit the teams"],
                                                    div [class "mt-5 mb-5"] [createButton model],
                                                    div [] [createTableTeams model acceptableTeams]
                                              ]]

                        Users -> case model.action of
                                    Create -> div [][]
                                    _ ->
                                        div [class "mt-5"]
                                              [
                                              div [class "mb-5"] [createButton model]
                                              ,div [] [createTableUser model acceptableUsers]
                                              ]
                        None -> div [] []
                    ,
                    case model.tableData of
                        [] ->  div [] [text "Welcome to the dashboard page"]
                        _ -> tbody [] []

                ]

createButton : RoundInfo -> Html Msg
createButton model = div [class "row"] [
                    div [class "col"] [
                        input [ placeholder "Search by Name", onInput SetQuery ] []
                    ],
                div [class "col", style "text-align" "right"] [
                    button [class "btn btn-dark", onClick (SendChanges model.changes)] [
                    div [class "row center"] [
                     p [class "col-auto mb-0"] [text "Apply changes"],
                     p [class "col text-success mb-0"] [text (fromInt((length model.changes.teamList) + (length model.changes.userList)))]
                     ]
                     ]
                ]
                ]

createTableUser : RoundInfo -> List User -> Html Msg
createTableUser model lst = table [class "table"] [
                                thead [] [
                                    tr [] [
                                        td [] [text "Username"],
                                        td [] [text "IsAdmin"],
                                        td [] [text "Password"],
                                        td [] [text "Password again"]
                                    ]
                                ], (lst) |> map (\x -> let change = getChangeUser x.username model in tr [] [
                                                            td [] [text x.username],
                                                            td [] [text x.isAdmin],
                                                            td [(if change.password == change.password2 then class "invalid" else class "")] [input [value model.changedFieldTempPass, onInput (Input "changePass"), onFocusOut (FocusOut x.username "userCreatePas"), attribute "type" "password", class "form-control", placeholder "password"][]],
                                                            td [(if change.password == change.password2 then class "invalid" else class "")] [input [value model.changedFieldTempPass2, onInput (Input "changePass2"), onFocusOut (FocusOut x.username "userCreatePas2"), attribute "type" "password", class "form-control", placeholder "password again"][]]
                                                        ]) |> tbody []
                            ]


createTableTeams : RoundInfo -> List Team -> Html Msg
createTableTeams model lst = table [class "table"] [
                                thead [] [
                                    tr [] [
                                        td [] [text "Teamname"],
                                        td [] [text "Users"],
                                        td [] [text "Etappe"],
                                        td [] [text "Change Etappe"],
                                        td [] [text "Change Color"],
                                        td [] [text "Delete"]
                                    ]
                                ], (lst) |> map (\x -> tr [] [
                                                            if length (filter (\el -> el.name == x.name && el.delete) model.changes.teamList) > 0  then td [style "text-decoration" "underline", style "text-decoration-color" "red"] [text x.name] else td [] [text x.name],
                                                            td [] [
                                                                if model.selectedRow == x.name then
                                                                    input [value model.changedFieldTemp, onInput (Input "createPlayers"), onFocusOut (FocusOut x.name "teamsEdit")] []
                                                                else
                                                                   p [onClick (SelectedRow x.name x.users)] [text (getChangedVal x.users (getChangeString x.name model "users"))]
                                                               ],
                                                            td [] [text (fromInt (x.etappe + (getChangeInt x.name model)))],
                                                            td [] [
                                                                div [class "row "][
                                                                button [class "col-md-3 btn btn-dark", onClick (SetChange (changeTeam model.changes x.name "+" (getChangeString x.name model "users") x.color False))] [text "+"],
                                                                button [class "col-md-3 btn btn-dark", style "margin-left" "0.1rem", onClick (SetChange (changeTeam model.changes x.name "-" (getChangeString x.name model "users") x.color False))] [text "-"]
                                                                ]
                                                            ],
                                                            td [] [
                                                                div [class "row justify-content-start"][
                                                                    div [class "dot col-sm-1", style "background" (getChangedVal x.color (getChangeString x.name model "color")), style "--bs-gutter-x" "1rem"] [],
                                                                    button [class "col-sm-5 btn btn-dark", onClick (GetColorWheel x.name)] [text "Choose color"],
                                                                    if model.selectedWheel == x.name then colorModal model x.name x.users else div[] []
                                                                ]
                                                            ],
                                                            td [] [
                                                                button [attribute "type" "button", class "btn-close", attribute "aria-label" "Close", onClick (DeleteFromChanges "Teams" x.name)][]
                                                            ]
                                                        ]) |> tbody []
                            ]
colorModal : RoundInfo -> String -> String -> Html Msg
colorModal model name users =
                div [class "modal", style "display" "block"][
                  div [class "modal-dialog"][
                    div [class "modal-content center", style "width" "50%"][
                        div [style "margin-top" "1rem"] [
                        ColorPicker.view model.colour model.colorPicker
                         |> Html.map ColorPickerMsg
                         ]
                         , button [class "btn btn-dark", style "margin-top" "1rem", style "margin-bottom" "1rem", onClick (SetChange (changeTeam model.changes name "color" users (colorToHex model.colour) False))] [text "submit"]
                    ]
                  ]
                ]

getPasswords : String -> (String, String)
getPasswords input = let lst = split "," input in case (head lst, head (reverse lst)) of
                                                     (Just x, Just y) -> (x, y)
                                                     (Just x, Nothing) -> (x, "")
                                                     (Nothing, Just y) -> ("", y)
                                                     _ -> ("", "")


createTableTeamsCreate : List Team -> Html Msg
createTableTeamsCreate lst = table [class "table"] [
                                thead [] [
                                    tr [] [
                                        td [] [text "Teamname"],
                                        td [] [text "Users"],
                                        td [] [text "Etappe"],
                                        td [] [text "Color"]
                                    ]
                                ], (lst) |> map (\x -> tr [] [
                                                            td [] [text x.name],
                                                            td [] [text x.users],
                                                            td [] [text (fromInt x.etappe)],
                                                            td [] [div [class "dot dotCreate", style "background" x.color] []]
                                                        ]) |> tbody []
                            ]

decode : String  -> List(String, String)
decode json = case decodeString (keyValuePairs string) json of
                Ok x -> x
                Err _ -> [("no", "no")]


getChangedVal : String -> String -> String
getChangedVal str1 str2 = case (str1, str2) of
                        ("", "") -> "None"
                        ("", x) -> x
                        (x, "") -> x
                        (_, y) -> y


getChangeString : String -> RoundInfo -> String -> String
getChangeString name model attribute = case find (\x -> x.name == name) model.changes.teamList of
                                    Just y -> case attribute of
                                                "users" -> y.users
                                                "color" -> y.color
                                                _ -> "Wrong attribute!"
                                    Nothing -> ""

getChangeStringUser : String -> RoundInfo -> String -> String
getChangeStringUser name model attribute = case find (\x -> x.username == name) model.changes.userList of
                                    Just y -> case attribute of
                                                "password" -> y.password ++ ", " ++ y.password2
                                                _ -> "Wrong attribute!"
                                    Nothing -> ""
getChangeUser : String -> RoundInfo -> User
getChangeUser name model = case find (\x -> x.username == name) model.changes.userList of
                                Just y -> y
                                Nothing -> User "empty" "" False "" ""

getChangeInt : String -> RoundInfo -> Int
getChangeInt name model = case find (\x -> x.name == name) model.changes.teamList of
                                  Just y -> y.etappe
                                  Nothing -> 0


getArrString : String -> List (Dict String String)
getArrString str = case decodeString (field "table" (Json.Decode.list (dict string))) str of
                    Ok x -> x
                    Err err -> [Dict.fromList [("err", errorToString err)]]


tableUsers : List(Dict String String) -> List User
tableUsers x = map (\y -> User (getStringFromDict y "username") (getStringFromDict y "hasAdmin") False "" "") x


tableTeams : List(Dict String String) -> List Team
tableTeams x = map (\y -> Team (getStringFromDict y "name") (getStringFromDict y "users") (stringToInt (getStringFromDict y "etappe")) False (getStringFromDict y "color")) x


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
                                , ("etappe", Encode.int t.etappe)
                                , ("delete", Encode.bool t.delete)
                                , ("color", Encode.string t.color)]


userToObject : User -> Encode.Value
userToObject u = Encode.object[ ("username", Encode.string u.username)
                              , ("isAdmin", Encode.string u.isAdmin)
                              , ("delete", Encode.bool u.delete)]


onFocusOut : msg -> Attribute msg
onFocusOut message =
  on "focusout" (Json.Decode.succeed message)


onFocusIn : msg -> Attribute msg
onFocusIn message =
  on "focusin" (Json.Decode.succeed message)


renderToast : String -> Html Msg
renderToast toast =
    div [class "container mt-3 fade"][
        div [class "card bg-success"] [
                div [class "card-body"] [
                    h5 [class "card-title text-white text-center"] [text toast]
                ]
            ]
        ]


changeTeam : Change -> String -> String -> String -> String -> Bool -> Change
changeTeam changes team op newUsers color delete = case find (\el -> el.name == team) changes.teamList of
                                Just _ -> Change (map (\val -> if val.name == team then
                                                                            case op of
                                                                               "+" -> Team val.name newUsers (val.etappe + 1) (val.delete || delete) val.color
                                                                               "-" -> Team val.name newUsers (val.etappe - 1) (val.delete || delete) val.color
                                                                               "none" -> Team val.name newUsers val.etappe (val.delete || delete) val.color
                                                                               "color" -> Team val.name val.users val.etappe (val.delete || delete) color
                                                                               _ -> val
                                                       else val
                                                                               ) changes.teamList) changes.userList
                                Nothing -> case op of
                                            "+" -> Change (changes.teamList ++ [Team team newUsers 1 delete color]) changes.userList
                                            "-" -> Change (changes.teamList ++ [Team team newUsers -1 delete color]) changes.userList
                                            "none" -> Change (changes.teamList ++ [Team team newUsers 0 delete color]) changes.userList
                                            "color" -> Change (changes.teamList ++ [Team team newUsers 0 delete color]) changes.userList
                                            _ -> changes


changeUser : Change -> String -> String -> String -> String -> String -> Change
changeUser changes username admin pass pass2 op = case find (\el -> el.username == username) changes.userList of
                                                    Just _ -> Change changes.teamList (map (\val -> if val.username == username then
                                                                            case op of
                                                                               "delete" -> User val.username val.isAdmin val.delete val.password val.password2
                                                                               "changePass" -> User val.username val.isAdmin val.delete pass pass2
                                                                               "changeAdmin" -> User val.username admin val.delete val.password val.password2
                                                                               _ -> val
                                                                         else val) changes.userList)
                                                    Nothing -> case op of
                                                                "delete" -> Change changes.teamList (changes.userList ++ [User username admin True pass pass2])
                                                                "changePass" -> Change changes.teamList (changes.userList ++ [User username admin False pass pass2])
                                                                "changeAdmin" -> Change changes.teamList (changes.userList ++ [User username admin False pass pass2])
                                                                _ -> changes

colorToHex : Color -> String
colorToHex col = let {red, green, blue, alpha} = toRgba col in "#" ++ fixedSizeHex (Hex.toString (round (red * 255))) ++ fixedSizeHex (Hex.toString (round (green * 255))) ++ fixedSizeHex (Hex.toString (round (blue * 255)))

fixedSizeHex : String -> String
fixedSizeHex s = case String.length s of
                    1 -> "0" ++ s
                    _ -> s
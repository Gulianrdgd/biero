port module Main exposing (..)

import Admin exposing (getStringFromDict)
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, h5, img, span, table, tbody, td, text, thead, tr)
import Html.Attributes exposing (alt, attribute, class, id, src, style)
import Html.Attributes.Aria exposing (role)
import Json.Decode exposing (decodeString, dict, errorToString, field, keyValuePairs, string)
import List exposing (head, map)
import Loading
    exposing
        ( LoaderType(..)
        , defaultConfig
        , render
        )
import String exposing (fromInt, toInt)


main : Program { url : String } Model Msg
main =
    Browser.element { init = \{ url } -> ( init url, Cmd.none ), update = update, subscriptions = subscriptions, view = view }



-- MODEL


port sendMessage : String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


type alias Model =
    { username : String
    , token : String
    , teamPos : List ( Int, Int )
    , selectedRow : String
    , tableData : List Team
    }


type alias Team =
    { name : String
    , users : String
    , etappe : Int
    , color : String
    , cssColor : String
    , posLeft : String
    , posTop : String
    }


init : String -> Model
init _ =
    Model "" "" [] "" []


subscriptions : Model -> Sub Msg
subscriptions model =
    messageReceiver Recv


type Msg
    = Recv String
    | SelectedRow String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectedRow name ->
            ( { model | selectedRow = name }, Cmd.none )

        Recv message ->
            case decode message of
                [ ( "?username", user ) ] ->
                    ( { model | username = user }, Cmd.none )

                [ ( "?token", token ) ] ->
                    ( { model | token = token }, Cmd.none )

                [ ( "?color", token ) ] ->
                    ( { model | token = token }, Cmd.none )

                [ ( "no", "no" ) ] ->
                    ( { model | tableData = tableTeams (getArrString message) }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "container-fluid", style "padding-left" "0", style "padding-right" "0", style "background" "#e62272" ]
        [ div [ class "row no-gutters", style "height" "100vh" ]
            [ div [ class "col-12 col-xs-8 col-md-8 col-lg-8 col-xl-10" ]
                [ case model.tableData of
                    [] ->
                        div [ class "d-flex justify-content-center" ]
                            [ div [ class "spinner-border ", role "status", style "margin-top" "40vh", style "width" "10rem", style "height" "10rem" ]
                                [ span [ class "visually-hidden" ] [ text "Loading..." ]
                                ]
                            ]

                    x ->
                        case head x of
                            Just z ->
                                div [ style "display" "flex", style "width" "100%", style "height" "100vh"]
                                    [ img [ src "/images/biero.png", style "margin" "auto", style "max-height" "1920", style "width" "100%", style "height" "auto", attribute "draggable" "false", alt "map of the biero" ] []
                                    , map
                                        (\team ->
                                            teamPerson team.cssColor team.posLeft team.posTop
                                        )
                                        model.tableData
                                        |> div []
                                    ]

                            _ ->
                                div [] []
                ]
            , div [ class "col-12 col-xs-4 col-md-4 col-lg-4 col-xl-2", style "display" "block" ]
                [ div [ class "card text-center", style "height" "100vh" ]
                    [ div [ class "card-body" ]
                        [ h5 [ class "card-title" ] [ text "Leaderboard" ]
                        , div [ class "table-responsive" ]
                            [ createLeaderboard model.tableData
                            ]
                        ]
                    ]
                ]
            ]
        ]


teamPerson : String -> String -> String -> Html Msg
teamPerson color posLeft posTop =
    div []
        [ img [ class "player", src "images/body.png", style "position" "absolute", style "top" (posTop ++ "px"), style "left" (posLeft ++ "px"), alt "player body" ] []
        , img [ class "player", src "images/color.png", style "position" "absolute", style "top" (posTop ++ "px"), style "left" (posLeft ++ "px"), style "filter" color, alt "player bicycle in a different color" ] []
        ]


decode : String -> List ( String, String )
decode json =
    case decodeString (keyValuePairs string) json of
        Ok x ->
            x

        Err _ ->
            [ ( "no", "no" ) ]


getArrString : String -> List (Dict String String)
getArrString str =
    case decodeString (field "table" (Json.Decode.list (dict string))) str of
        Ok x ->
            x

        Err err ->
            [ Dict.fromList [ ( "err", errorToString err ) ] ]


tableTeams : List (Dict String String) -> List Team
tableTeams x =
    map (\y -> Team (getStringFromDict y "name") (getStringFromDict y "users") (stringToInt (getStringFromDict y "etappe")) (getStringFromDict y "color") (getStringFromDict y "cssColor") (getStringFromDict y "posLeft") (getStringFromDict y "posTop")) x


createLeaderboard : List Team -> Html Msg
createLeaderboard lst =
    table [ class "table" ]
        [ thead []
            [ tr []
                [ td [] [ text "Color" ]
                , td [] [ text "Team" ]
                , td [] [ text "Etappe" ]
                ]
            ]
        , lst
            |> map
                (\x ->
                    tr []
                        [ td [] [ div [ class "dot dotCreate", style "background" x.color ] [] ]
                        , td [] [ text x.name ]
                        , td [] [ text (fromInt x.etappe) ]
                        ]
                )
            |> tbody []
        ]


stringToInt : String -> Int
stringToInt s =
    case toInt s of
        Just x ->
            x

        Nothing ->
            0

port module Main exposing (..)

import Admin exposing (getStringFromDict)
import Browser
import Dict exposing (Dict)
import Html exposing (Attribute, Html, div, img, p, text)
import Html.Attributes exposing (attribute, class, height, id, src, style, width)
import Http
import Image exposing (Image)
import Json.Decode exposing (decodeString, dict, errorToString, field, keyValuePairs, string)
import List exposing (head, map)
import String exposing (toInt)
import Loading
  exposing
      ( LoaderType(..)
      , defaultConfig
      , render
      )



main : Program { url : String } Model Msg
main =
  Browser.element { init = \{ url } -> ( init url, Cmd.none ), update = update, subscriptions = subscriptions, view = view }


-- MODEL
port sendMessage : String -> Cmd msg
port messageReceiver : (String -> msg) -> Sub msg


type alias Model  =
  { username : String
  , token : String
  , teamPos : List (Int, Int)
  , selectedRow : String
  , tableData : List(Team)
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
init url =
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
         ({model | selectedRow = name}, Cmd.none)
    Recv message ->
        case decode message of
                      [("?username", user)] -> ({ model | username = user}, Cmd.none)
                      [("?token", token)] -> ({ model | token = token}, Cmd.none)
                      [("?color", token)] -> ({ model | token = token}, Cmd.none)
                      [("no", "no")] -> ({ model | tableData = tableTeams (getArrString message)}, Cmd.none)
                      _ -> (model, Cmd.none)


view : Model -> Html Msg
view model =
    div [class "container"] [
        case model.tableData of
            [] ->     div [class "center", style "margin" "auto"]
                          [ Loading.render
                              DoubleBounce -- LoaderType
                              { defaultConfig | color = "#333" } -- Config
                              Loading.On -- LoadingState
                          ]
            x -> case (head x) of
                    Just z -> div [] [
                                      img [src "/images/world.svg", style "position" "absolute", style "top" "0", style "left" "0", style "width" "100%", style "height" "100vh"] [],
                                      img [id "biero", src "/images/biero.svg", style "position" "absolute", style "top" "0", style "left" "0", style "width" "100%", style "height" "100vh"] [],
                                      map (\team ->
                                            teamPerson team.cssColor team.posLeft team.posTop
                                          ) model.tableData |> div []
                                      ]
                    _ -> div [] []
    ]

teamPerson : String -> String -> String -> Html Msg
teamPerson color posLeft posTop = div [] [
                            img [class "player", src "images/body.png", style "position" "absolute", style "top" (posTop ++ "px"), style "left" (posLeft ++ "px")] [],
                            img [class "player", src "images/color.png", style "position" "absolute", style "top" (posTop ++ "px"), style "left" (posLeft ++ "px"), style "filter" color] []
                       ]

decode : String  -> List(String, String)
decode json = case decodeString (keyValuePairs string) json of
                Ok x -> x
                Err _ -> [("no", "no")]

getArrString : String -> List (Dict String String)
getArrString str = case decodeString (field "table" (Json.Decode.list (dict string))) str of
                    Ok x -> x
                    Err err -> [Dict.fromList [("err", errorToString err)]]

tableTeams : List(Dict String String) -> List Team
tableTeams x = map (\y -> Team (getStringFromDict y "name") (getStringFromDict y "users") (stringToInt (getStringFromDict y "etappe")) (getStringFromDict y "color") (getStringFromDict y "cssColor") (getStringFromDict y "posLeft") (getStringFromDict y "posTop")) x

stringToInt : String -> Int
stringToInt s = case toInt s of
                 Just x -> x
                 Nothing -> 0
--svgFiltersToStyles : String -> List(Attribute msg)

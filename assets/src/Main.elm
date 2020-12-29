port module Main exposing (..)

import Browser
import Html exposing (Html, div, p, text)
import Html.Attributes exposing (class)
import Json.Decode exposing (decodeString, keyValuePairs, string)
import List exposing (head, reverse)
import Svg exposing (rect, svg)
import Svg.Attributes exposing (height, rx, ry, viewBox, width, x, y)


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
  , teamPos : List (Int, Int)
  }


init : String -> RoundInfo
init url =
  RoundInfo "" "" []


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
        roundRect,
        p [] [text "Hzo is dit wel zichtbaar????"]
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

roundRect =
    svg
      [ width "520", height "120", viewBox "200 0 120 120" ]
      [ rect [ x "10", y "10", width "100", height "100", rx "15", ry "15" ] [] ]
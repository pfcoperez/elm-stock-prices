module Main exposing (main)

import Browser
import Html exposing (Html, table, tr, th, td, text, button, div, input)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)

main =
  Browser.element
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

-- MODEL

type alias Value =
    {
      symbol : String,
      subscribed: Bool,
      current : Maybe Float,
      volume : Maybe Float,
      timestamp : Maybe Time.Posix
    }

newValue : String -> Value
newValue sym = {
  symbol = sym,
  subscribed = False,
  current = Nothing,
  volume = Nothing,
  timestamp = Nothing
  }

type alias State = 
  { 
    symbolSelector : String,
    apiKey : Maybe String,
    values : List Value
  }

init : () -> (State, Cmd Msg)
init () = (
  { symbolSelector = ""
  , apiKey = Nothing
  , values = []
  }
  , Cmd.none
  ) 

-- UPDATE

type Msg = Subscribe | Unsubscribe String | SelectorChange String

update : Msg -> State -> (State, Cmd Msg)
update msg model = case msg of
  SelectorChange newSelector -> ({ model | symbolSelector = newSelector }, Cmd.none)
  Subscribe -> ({ model | symbolSelector = "", values = model.values ++ [ newValue model.symbolSelector ] }, Cmd.none)
  Unsubscribe symbol -> ({ model | values = List.filter (\value -> value.symbol /= symbol) model.values }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : State -> Sub Msg
subscriptions _ = Sub.none

-- VIEW

type alias TextValueFunction a = (a -> String) -> Maybe a -> String
textValue : TextValueFunction a
textValue toText maybeX = Maybe.withDefault "-" (Maybe.map toText maybeX)

timeToUTCStr : Time.Posix -> String
timeToUTCStr time =
  String.fromInt (toHour utc time)
  ++ ":" ++
  String.fromInt (toMinute utc time)
  ++ ":" ++
  String.fromInt (toSecond utc time)
  ++ " (UTC)"

valueRow : Value -> Html Msg
valueRow { symbol, current, volume, timestamp } =
  tr [] [
    td [] [ text symbol ],
    td [] [ text (textValue String.fromFloat current) ],
    td [] [ text (textValue String.fromFloat volume) ],
    td [] [ text (textValue timeToUTCStr timestamp) ],
    td [] [ button [ onClick (Unsubscribe symbol) ] [ text "Unsubscribe" ] ]
  ]

valuesTableHeader : Html Msg
valuesTableHeader =
  tr [] [
    th [] [ text "Symbol" ],
    th [] [ text "Price" ],
    th [] [ text "Volume" ],
    th [] [ text "Last update" ] 
  ]

valuesTable : List Value -> Html Msg
valuesTable values = table [] ([ valuesTableHeader ] ++ (List.map valueRow values))

addSymbolView : String -> Html Msg
addSymbolView currentSelector =
  div [] [
    text "Subscribe to: ",
    input [ value currentSelector, onInput SelectorChange ] [],
    button [ onClick Subscribe ] [ text "OK" ]
    ]

view : State -> Html Msg
view { values, symbolSelector } = 
  div [] [
    addSymbolView symbolSelector,
    valuesTable values
  ]
port module Main exposing (main)

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

-- Low level WS ports

port sendMessage : String -> Cmd msg
port messageReceiver : (String -> msg) -> Sub msg

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
    values : List Value,
    wsLog: List String
  }

init : () -> (State, Cmd Msg)
init () = (
  { symbolSelector = ""
  , apiKey = Nothing
  , values = []
  , wsLog = [ "Init" ]
  },
  Cmd.none
  ) 

-- Messages for WS port

subscribeJson : String -> String
subscribeJson symbol =
  let
    subscriptionJson = "{\"type\":\"subscribe\",\"symbol\":\"" ++ symbol ++ "\"}"
  in subscriptionJson

-- UPDATE

type Msg
  = Subscribe
  | Unsubscribe String
  | SelectorChange String
  | Receive String

addWsLogEntry : List String -> String -> List String
addWsLogEntry currentLog newEntry = newEntry :: List.take 9 currentLog

update : Msg -> State -> (State, Cmd Msg)
update msg model = case msg of
  SelectorChange newSelector -> ({ model | symbolSelector = newSelector }, Cmd.none)
  Subscribe ->
    let
      subsCommand = sendMessage (subscribeJson model.symbolSelector)
    in ({ model | symbolSelector = "", values = model.values ++ [ newValue model.symbolSelector ] }, subsCommand)
  Unsubscribe symbol -> ({ model | values = List.filter (\value -> value.symbol /= symbol) model.values }, Cmd.none)
  -- Port messages
  Receive value -> ({model | wsLog = addWsLogEntry model.wsLog value }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : State -> Sub Msg
subscriptions _ = Sub.batch [ messageReceiver Receive ]

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

logView : List String -> Html Msg
logView entries = div [] (List.map text entries)

view : State -> Html Msg
view { values, symbolSelector, wsLog } = 
  div [] [
    addSymbolView symbolSelector,
    valuesTable values,
    logView wsLog
  ]
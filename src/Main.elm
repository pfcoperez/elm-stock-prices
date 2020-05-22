port module Main exposing (main)

import Browser
import Html exposing (Html, table, tr, th, td, text, button, div, input, br, h1, h2)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Time exposing (..)
import Json.Decode exposing (float, string, int, decodeValue, map4, field, list)
import Json.Encode
import Dict exposing (Dict)
import Maybe.FlatMap exposing (flatMap)

main =
  Browser.element
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

-- Low level WS ports

port sendMessage : String -> Cmd msg
port connectWithToken : String -> Cmd msg
port messageReceiver : (Json.Encode.Value -> msg) -> Sub msg

-- MODEL

type alias HistoryRecord =
  {
    price : Float,
    timestamp: Time.Posix
  }

type alias Value =
    {
      symbol : String,
      subscribed: Bool,
      current : Maybe Float,
      volume : Maybe Float,
      timestamp : Maybe Time.Posix,
      history : Maybe HistoryRecord
    }

newValue : String -> Value
newValue sym = {
  symbol = sym,
  subscribed = False,
  current = Nothing,
  volume = Nothing,
  timestamp = Nothing,
  history = Nothing
  }

type alias State = 
  {
    token: String,
    symbolSelector : String,
    apiKey : Maybe String,
    values : Dict String Value,
    wsLog: List String
  }

init : () -> (State, Cmd Msg)
init () = (
  { token = ""
  , symbolSelector = ""
  , apiKey = Nothing
  , values = Dict.empty
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

unsubscribeJson : String -> String
unsubscribeJson symbol = "{\"type\":\"unsubscribe\",\"symbol\":\"" ++ symbol ++ "\"}"

-- UPDATE

type Msg
  = Subscribe
  | Unsubscribe String
  | SelectorChange String
  | Receive Json.Encode.Value
  | TokenChange String
  | UseToken
  | Tick Time.Posix

addWsLogEntry : List String -> String -> List String
addWsLogEntry currentLog newEntry = newEntry :: List.take 9 currentLog

update : Msg -> State -> (State, Cmd Msg)
update msg model = case msg of
  SelectorChange newSelector -> ({ model | symbolSelector = newSelector }, Cmd.none)
  Subscribe ->
    let
      subsCommand = sendMessage (subscribeJson model.symbolSelector)
    in ({ model | symbolSelector = "", values = Dict.insert model.symbolSelector (newValue model.symbolSelector) model.values }, subsCommand)
  Unsubscribe symbol -> 
    --let
    --  unsubsCommand = sendMessage (unsubscribeJson model.symbolSelector)
    -- in ({ model | values = List.filter (\value -> value.symbol /= symbol) model.values }, unsubsCommand)
    (model, Cmd.none)
  TokenChange newToken ->
    ({ model | token = newToken }, Cmd.none)
  UseToken ->
    if String.isEmpty model.token then ( { model | wsLog = addWsLogEntry model.wsLog "ERROR: Empty token" }, Cmd.none) -- FIXME: Show a notification pop-up.
    else
      let
        (initialModel, _) = init ()
        initialLog = [ "Connecting using token: " ++ model.token ]
      in ( { initialModel | wsLog = initialLog, token = model.token } , connectWithToken model.token)
  -- Port messages
  Receive json ->
    let jsonAsStr = Json.Encode.encode 0 json
    in case decodeValue valuesDecoder json of
      Ok receivedValues ->
        let
          valuesUpdater : Value -> Dict String Value -> Dict String Value
          valuesUpdater value current =
            let
              maybeHistory = flatMap (\known -> known.history ) (Dict.get value.symbol current)
              newHistory = case maybeHistory of
                  Just h -> Just h
                  Nothing -> Maybe.map2 (\t -> \p -> { price = p, timestamp = t }) value.timestamp value.current
            in Dict.insert value.symbol { value | history = newHistory } current
          updated = List.foldl valuesUpdater model.values receivedValues
        in ({model | wsLog = addWsLogEntry model.wsLog jsonAsStr, values = updated}, Cmd.none)
      Err err ->
        let
          m = case err of
            Json.Decode.Failure problem _ -> problem
            _ -> ""
        in ({model | wsLog = addWsLogEntry model.wsLog ("ERROR: " ++ m ++ " " ++ jsonAsStr)}, Cmd.none)
  Tick currentTime ->
    let
      historyExpiration : String -> Value -> Value
      historyExpiration _ value =
        let
          historyTTL = 1000 -- millis
          currentMs = posixToMillis currentTime
          isExpired timeMs = currentMs - historyTTL > timeMs
        in
          { value |
            history = flatMap (\h -> if isExpired (posixToMillis h.timestamp) then Nothing else Just h ) value.history
          }
    in ({ model | values = Dict.map historyExpiration model.values }, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : State -> Sub Msg
subscriptions _ = Sub.batch [ messageReceiver Receive, Time.every 500 Tick ]

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
valueRow { symbol, current, volume, timestamp, history } =
  let
    transparent = "#ffffff00"
    up = "turquoise"
    down = "red"
    priceColor maybePrev maybeCurrent =
      Maybe.withDefault
        transparent
        (Maybe.map2 (\prev -> \c -> if c < prev then down else if c > prev then up else transparent) maybePrev maybeCurrent)
    color = priceColor (Maybe.map (\h -> h.price) history) current
  in
    tr [] [
      td [] [ text symbol ],
      td [ style "background-color" color ] [ text (textValue String.fromFloat current) ],
      td [] [ text (textValue String.fromFloat volume) ],
      td [] [ text (textValue timeToUTCStr timestamp) ] --,
      -- td [] [ button [ onClick (Unsubscribe symbol) ] [ text "Unsubscribe" ] ]
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
valuesTable values = table [ class "table" ] ([ valuesTableHeader ] ++ (List.map valueRow values))

addSymbolView : String -> Html Msg
addSymbolView currentSelector =
  div [ class "field has-addons" ] [
    div [ class "control" ] [
      input [ value currentSelector, onInput SelectorChange, class "input is-primary" ] []
    ],
    div [ class "control" ] [
      button [ onClick Subscribe, class "button is-info" ] [ text "Add symbol" ]
    ]
    ]

logView : List String -> Html Msg
logView entries = div [ class "footer" ] [
  div [ class "content has-text-centered" ] (List.map (\entry -> div [ ] [text entry, br [] []]) entries)
  ]

tokenView : String -> Html Msg
tokenView currentToken =
  div [ class "field has-addons" ] [
      div [ class "control" ] [
        input [ value currentToken, onInput TokenChange, class "input is-primary" ] []
      ],
      div [ class "control" ] [
        button [ onClick UseToken, class "button is-info" ] [ text "Connect" ]
      ]
    ]

view : State -> Html Msg
view { values, symbolSelector, wsLog, token } =
  div [ class "columns" ] [
    div [ class "column is-one-third" ] [
      h1 [ class "title is-2" ] [ text "Stock prices" ],
      br [] [],
      h2 [ class "subtitle" ] [
        text "API Token",
        br [] [],
        br [] [],
        tokenView token
      ],
      h2 [ class "subtitle" ] [
        text "Subscriptions",
        br [] [],
        br [] [],
        addSymbolView symbolSelector
      ]
    ],
    div [ class "column" ] [
      div [ class "container" ] [
        valuesTable (List.sortBy (\v -> v.symbol) (Dict.values values)),
        logView wsLog
        ]
      ]
  ] 

valueDecoder : Json.Decode.Decoder Value
valueDecoder =
  let
    builder : String -> Float -> Float -> Int -> Value
    builder symbol price volume millis = Value symbol True (Just price) (Just volume) (Just (Time.millisToPosix millis)) Nothing
  in map4 builder
    (field "s" string)
    (field "p" float)
    (field "v" float)
    (field "t" int)

valuesDecoder : Json.Decode.Decoder (List Value)
valuesDecoder = field "data" (Json.Decode.list valueDecoder)
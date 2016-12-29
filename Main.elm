import Html exposing (Html, button, div, text, ul, li)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import String
import Http
import Date
import Task


type alias Log = String


type alias Model = { lines : List Log }


type Msg = NewLog (Result Http.Error String)
         | LineSent (Result Http.Error ())
         | Mouse String
         | Append (Date.Date, String)


main : Program Never Model Msg
main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


init : (Model, Cmd Msg)
init = (Model [], getLog)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Mouse s ->
      (model, (getTimeAndAppend s))
    NewLog (Ok log) ->
      ({model | lines = List.filter (String.isEmpty >> not) (String.lines log)}, Cmd.none)
    NewLog (Err error) ->
      ({ model | lines = (String.lines (toString error))}, Cmd.none)
    LineSent (Ok _) ->
      (model, getLog)
    LineSent (Err error) ->
      ({ model | lines = (String.lines (toString error))}, Cmd.none)
    Append (time, s) ->
      let line = (formatDate time) ++ ": " ++ s
      in
        (model, (sendLine line))


view : Model -> Html Msg
view model =
  div []
    [ ul [] (List.map (\l -> li [ style [("color", "#333"), ("font-size", "40pt")]] [ text l ]) model.lines)
    , button (mouseButtonAttribs "Maus") [ text "Maus" ]
    , button (mouseButtonAttribs "2 Mäuse") [ text "2 Maus" ]
    , button (mouseButtonAttribs "Maus pimpd") [ text "Maus +" ]
    , button (mouseButtonAttribs "2 Mäuse mit pimp") [ text "2 Maus +" ]
    ]


mouseButtonAttribs s =
  [ onClick (Mouse s)
  , style [("float", "left"), ("padding", "20px"), ("background-color", "#D95B43"), ("width", "100vw"), ("font-size", "60pt")]
  ]

getLog : Cmd Msg
getLog =
  Http.getString "http://localhost:4002/"
  |> Http.send NewLog


sendLine : String -> Cmd Msg
sendLine line =
  Http.send LineSent (requestLine line)


requestLine : String -> Http.Request ()
requestLine line =
  Http.request
    { method = "POST"
    , headers = []
    , url = "http://localhost:4002/append?line=" ++ line
    , body = Http.emptyBody
    , expect = Http.expectStringResponse (\_ -> Ok ())
    , timeout = Nothing
    , withCredentials = False
    }


getTimeAndAppend : String -> Cmd Msg
getTimeAndAppend s =
  Task.perform (\t -> Append (t, s)) Date.now


formatDate : Date.Date -> String
formatDate d =
  String.join "." [toString (Date.day d), toString (monthToInt (Date.month d)), toString (Date.year d)]


monthToInt : Date.Month -> Int
monthToInt m =
  case m of
    Date.Jan -> 1
    Date.Feb -> 2
    Date.Mar -> 3
    Date.Apr -> 4
    Date.May -> 5
    Date.Jun -> 6
    Date.Jul -> 7
    Date.Aug -> 8
    Date.Sep -> 9
    Date.Oct -> 10
    Date.Nov -> 11
    Date.Dec -> 12


subscriptions : Model -> Sub msg
subscriptions _ = Sub.none

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.App
import Http
import Task
import Json.Decode

main =
  Html.App.program
    { init = init
    , subscriptions = subscriptions
    , update = update
    , view = view
    }

-- Model

type alias Model =
  { topic : String
  , gifUrl : String
  }

init : (Model, Cmd Msg)
init =
  let
    model = Model "kittens" ""
  in

  (model, getRandomGif model.topic)

-- Update

type Msg
  = GifRequest
  | SetTopic String
  | FetchSucceed String
  | FetchFail Http.Error

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    SetTopic topic ->
      ({ model | topic = topic }, Cmd.none)

    GifRequest ->
      (model, getRandomGif model.topic)

    FetchSucceed url ->
      ({ model | gifUrl = url }, Cmd.none)

    FetchFail error ->
      (model, Cmd.none)

getRandomGif : String -> Cmd Msg
getRandomGif topic =
  let
    url =
      "http://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic
  in
    Task.perform FetchFail FetchSucceed (Http.get decodeGifUrl url)

decodeGifUrl : Json.Decode.Decoder String
decodeGifUrl =
  Json.Decode.at ["data", "image_url"] Json.Decode.string

-- Subscriptions

subscriptions : Model -> Sub a
subscriptions model =
  Sub.none

-- View

view : Model -> Html Msg
view model =
  div []
    [ h2 [] [ text <| if model.topic == "" then "???" else model.topic ]
    , button [ onClick GifRequest ] [ text "More Please!" ]
    , br [] []
    , br [] []
    , input [ onInput SetTopic, value model.topic ] [ text model.topic ]
    , br [] []
    , br [] []
    , img [ src model.gifUrl ] []
    ]

port module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as D
import Random
import Browser.Events exposing (onKeyDown)
import Html
import Html
import Html.Attributes
import Debug exposing (toString)
import Time
import Task
import Regex


-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Browser.Events.onKeyDown keyDecoder
        }



-- PORTS


port sendMessage : String -> Cmd msg



-- MODEL


type alias Model =
    { draft : String
    , allSentences : List String
    , numFound : Int
    , whichWord : Int
    , sentenceStartTime : Maybe Time.Posix
    , wordStartTime : Maybe Time.Posix
    , totalWPM : Float
    , currentWPM : Float
    , dpwpm : Float
    , enemyHp : Int
    , enemyPosition : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { draft = ""
      , allSentences = ["The quick brown fox jumps over the lazy dog."]
      , numFound = 0
      , whichWord = 0
      , sentenceStartTime = Nothing
      , wordStartTime = Nothing
      , totalWPM = 0
      , currentWPM = 0
      , dpwpm = 0.1
      , enemyHp = 100
      , enemyPosition = 5
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Replay
    | NextWord
    | Typed Char
    | StartTime Time.Posix
    | WordEndTime Time.Posix
    | NoOp



-- Use the `sendMessage` port when someone presses ENTER or clicks
-- the "Send" button. Check out index.html to see the corresponding
-- JS where this is piped into a WebSocket.
--


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case Debug.log "msg" msg of
        StartTime time ->
            ( { model | sentenceStartTime = Just time
                        , wordStartTime = Just time
              }
            , Cmd.none
            )

        WordEndTime time ->
            ( { model | wordStartTime = Just time
                        , totalWPM = getWPM (getMysteryWord model |> String.left model.numFound ) model.sentenceStartTime time
                        , currentWPM = getWPM "word" model.wordStartTime time
                        , enemyHp = model.enemyHp - (round <| (getWPM "word" model.wordStartTime time) * model.dpwpm ) 
              }
            , Cmd.none
            )

        Replay ->
            ( model
            , sendMessage (getMysteryWord model)
            )

        NextWord ->
            let newModel = 
                  if List.length model.allSentences > (model.whichWord+1) then
                    {model | whichWord = model.whichWord + 1 }
                  else 
                    {model | whichWord = 0 }
            in
            ( { newModel | draft = "", numFound = 0 }
            , sendMessage (((getMysteryWord newModel) ))
            )

        Typed c ->
            if (model.numFound + 1) > (String.length <| getMysteryWord model) then
                update NextWord { model | draft = "", numFound = 0}
            else if c == getCurrentLetter model then
                ( { model | draft = "", numFound = model.numFound + 1}
                , 
                    if model.numFound == 0 then
                        getTime StartTime
                    else if c == ' ' then
                        getTime WordEndTime
                    else
                        Cmd.none
                )
            else
              ( { model | draft = String.fromChar c, enemyPosition = model.enemyPosition - 1}, Cmd.none)

        NoOp ->
          ( model, Cmd.none)

splitWords : String -> List String
splitWords sentence =
    Regex.split (Maybe.withDefault Regex.never <| Regex.fromString "[\\w]+") sentence

getWPM : String -> Maybe Time.Posix -> Time.Posix -> Float
getWPM sentence maybeStart end =
    case maybeStart of
        Nothing -> 0
        Just start ->
            let startMillis = Time.posixToMillis start
                endMillis = Time.posixToMillis end
                elapsedMillis = Debug.log "elapsedMillis" <| endMillis - startMillis
                elapsedSeconds = Debug.log "elapsedSeconds" <| (toFloat elapsedMillis) / 1000
                elapsedWords = Debug.log "elapsedWords" <| (splitWords sentence |> List.length |> toFloat)
            in
            Debug.log "getWPM" <| 60 * elapsedWords / elapsedSeconds

-- SUBSCRIPTIONS
-- VIEW


view : Model -> Html Msg
view model =
    mysteryWordView model


getMysteryWord : Model -> String
getMysteryWord model =
    case (List.head <| List.drop model.whichWord model.allSentences) of
        Nothing ->
          ""
        Just word ->
          word

mysteryWordView : Model -> Html Msg
mysteryWordView model =
    div [class "columns"] [
      div [ class "column container is-fluid has-text-centered" ]
          [ h1 [ class "title" ] [ text ("Typing Shooter") ]
          , mysteryLetter model.draft (getMysteryWord model) model.numFound 
          , button [class "button is-info", onClick Replay] [text "🔊 Replay"]
          , button [class "button is-primary", onClick NextWord] [text "➡️ Next Word"]
          , br [] []
          , br [] []
          , progressBar model
          , h1 [ class "title" ] [ text ("Total WPM: " ++ (String.fromFloat model.totalWPM )) ]
          , h1 [ class "title" ] [ text ("Current WPM: " ++ (String.fromFloat model.currentWPM)) ]          
          , h1 [ class "title" ] [ text ("DPWPM: " ++ (String.fromFloat model.dpwpm)) ]          
          , h1 [ class "title" ] [ text ("Enemy HP: " ++ (String.fromInt model.enemyHp)) ]          
          , h1 [ class "title" ] [ text ("Enemy Position: " ++ (String.fromInt model.enemyPosition)) ]          
          ]
    ]

getCurrentLetter : Model -> Char
getCurrentLetter model =
  let charList = String.toList (getMysteryWord model) 
  in  
    List.drop model.numFound charList
    |> List.head
    |> Maybe.withDefault ' '

mysteryLetter : String -> String -> Int -> Html Msg
mysteryLetter guess word numberFound =
    div []
        (List.indexedMap
            (\i ->
                \letter ->
                    if i == numberFound && guess /= "" then 
                        div [ class "button is-danger is-large m-2" ] [ text (String.fromChar letter)  ]
                    else if i < numberFound then
                        if letter == ' ' then
                            div [ class "button is-success is-large m-2" ] [ text "🚀" ]
                        else
                            div [ class "button is-success is-large m-2" ] [ text (String.fromChar letter) ]
                    else
                        div [ class "button is-light is-large m-2" ] [ text (String.fromChar letter) ]
            )
            (String.toList word)
        )

wordList : List String -> Html Msg
wordList listOfWords =
    ul [] (List.map (\word -> li [ class "title" ] [ text word ]) listOfWords)

progressBar : Model -> Html Msg
progressBar model = 
  let maxRounds = List.length model.allSentences
      currentRound = model.whichWord
      progressPercent = 100 * (toFloat currentRound) / (toFloat maxRounds)
  in
    if progressPercent < 100 then
      Html.progress [class "progress is-success", Html.Attributes.max "100", Html.Attributes.value (String.fromFloat progressPercent)] [text ""]
    else
      div [class "modal is-active"]
          [ div [class "modal-background"] []
          , div [class "modal-content"] 
                [section [class "hero is-primary"]
                         [ div [class "hero-body"]
                               [p [class "title"] [text "All Done!"]]
                         ]
                ]
          ]

-- DETECT ENTER


ifIsEnter : msg -> D.Decoder msg
ifIsEnter msg =
    D.field "key" D.string
        |> D.andThen
            (\key ->
                if key == "Enter" then
                    D.succeed msg
                else
                    D.fail "some other key"
            )


--

keyDecoder : D.Decoder Msg
keyDecoder =
  D.map toKey (D.field "key" D.string)

toKey : String -> Msg
toKey string =
  case String.uncons string of
    Just (char, "") ->
      Typed char

    _ ->
      NoOp


-- CMD

getTime : (Time.Posix -> Msg) -> Cmd Msg
getTime msg = 
    Task.perform msg Time.now 
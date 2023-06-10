module Main exposing (..)

import Bootstrap.CDN as CDN
import Bootstrap.Grid exposing (container)
import Browser
import Browser.Dom as Dom
import Debug exposing (toString)
import Html exposing (Html, a, b, br, button, div, h1, h2, h3, p, span, text, textarea)
import Html.Attributes exposing (class, cols, disabled, href, id, placeholder, rows, style, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode exposing (field, string)
import List exposing (length)
import Round
import Task
import Time



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { goalParagraph : String
    , currentParagraph : String
    , playerName : String
    , playTimeMs : Int
    , gameStatus : GameStatus
    }


type GameStatus
    = AwaitingQuote
    | NotStarted
    | CountingDown
    | InProgress
    | Completed


init : () -> ( Model, Cmd Msg )
init _ =
    ( { goalParagraph = ""
      , currentParagraph = ""
      , playerName = "Anonymous"
      , playTimeMs = -3000
      , gameStatus = AwaitingQuote
      }
    , getRandomQuote
    )



-- UPDATE


type Msg
    = Change String
    | Tick Time.Posix
    | StartGame
    | EndGame
    | GotQuote (Result Http.Error String)
    | FocusResult (Result Dom.Error ())


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change newText ->
            ( { model
                | currentParagraph = newText
                , gameStatus =
                    if newText == model.goalParagraph then
                        Completed

                    else
                        InProgress
              }
            , Cmd.none
            )

        Tick _ ->
            let
                newPlayTimeMs =
                    model.playTimeMs + 100

                newGameStatus =
                    if newPlayTimeMs >= 0 then
                        InProgress

                    else
                        CountingDown
            in
            ( { model | playTimeMs = newPlayTimeMs, gameStatus = newGameStatus }, Dom.focus "typeBox" |> Task.attempt FocusResult )

        StartGame ->
            ( { model | gameStatus = CountingDown, currentParagraph = "", playTimeMs = -3000 }, Cmd.none )

        EndGame ->
            ( { model | gameStatus = Completed }, Cmd.none )

        GotQuote result ->
            case result of
                Ok quote ->
                    let
                        newQuote =
                            normalizeQuote quote
                    in
                    ( { model | goalParagraph = newQuote, gameStatus = NotStarted }, Cmd.none )

                Err _ ->
                    ( model, getRandomQuote )

        FocusResult result ->
            -- handle success or failure here
            case result of
                Err (Dom.NotFound _) ->
                    ( model, Cmd.none )

                Ok () ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if not (List.member model.gameStatus [ CountingDown, InProgress ]) then
        Sub.none

    else
        Time.every 100 Tick



-- VIEW


view : Model -> Html Msg
view model =
    viewPlayArea model


viewPlayArea : Model -> Html Msg
viewPlayArea model =
    container [ class "text-center p-4" ]
        [ CDN.stylesheet
        , h1 [] [ text "Typing Race" ]
        , h2 [] [ text ("Welcome, " ++ model.playerName ++ "!") ]
        , h3 [] [ text "Type the paragraph below as fast as you can to calculate your WPM. You have to match the capitalization, spacing, and punctuation exactly." ]
        , if model.gameStatus == AwaitingQuote then
            text "Getting quote..."

          else
            viewParagraph model.currentParagraph model.goalParagraph
        , textarea [ id "typeBox", placeholder "Type the paragraph above here!", value model.currentParagraph, onInput Change, disabled (model.gameStatus /= InProgress), rows 6, cols 48 ] []
        , br [] []
        , button [ onClick StartGame, disabled (List.member model.gameStatus [ InProgress, AwaitingQuote ]) ] [ text "Start" ]
        , h3 [] [ viewGameStatus model ]
        , text ("Benchmark WPM: " ++ getBenchmarkWPM model)
        , br [] []
        , text ("Raw WPM: " ++ getRawWPM model)
        , br [] []
        , br [] []
        , viewWPMInfo
        ]


viewParagraph : String -> String -> Html Msg
viewParagraph currentParagraph goalParagraph =
    let
        remainingText =
            String.slice (String.length currentParagraph) (String.length goalParagraph) goalParagraph

        checkChar =
            \char curIndex -> String.fromChar char /= String.slice curIndex (curIndex + 1) goalParagraph

        indexOfWrong =
            findIndex (String.toList currentParagraph) checkChar

        correctWrongBorder =
            if indexOfWrong == -1 then
                String.length currentParagraph

            else
                indexOfWrong

        correctText =
            String.slice 0 correctWrongBorder currentParagraph

        wrongText =
            String.slice correctWrongBorder (String.length currentParagraph) goalParagraph
    in
    p [ class "border m-auto w-50" ]
        [ span [ style "background-color" "lime" ] [ text correctText ]
        , span [ style "background-color" "red" ] [ text wrongText ]
        , span [] [ text remainingText ]
        ]


findIndex : List a -> (a -> Int -> Bool) -> Int
findIndex list fn =
    doFindIndex list fn 0


doFindIndex : List a -> (a -> Int -> Bool) -> Int -> Int
doFindIndex list fn curIndex =
    case List.head list of
        Just elem ->
            if fn elem curIndex then
                curIndex

            else
                case List.tail list of
                    Just tail ->
                        doFindIndex tail fn (curIndex + 1)

                    Nothing ->
                        -1

        Nothing ->
            -1


viewGameStatus : Model -> Html Msg
viewGameStatus model =
    text
        (if model.gameStatus == Completed then
            "You finished! 🏁 It took you "
                ++ toString (toFloat model.playTimeMs / 1000)
                ++ " seconds to complete the race!"

         else if model.gameStatus == CountingDown then
            "Get ready, game will start in " ++ toString (-1 * toFloat model.playTimeMs / 1000) ++ " seconds."

         else if model.gameStatus == InProgress then
            "Currently racing...Time played: " ++ toString (toFloat model.playTimeMs / 1000) ++ " seconds."

         else if model.gameStatus == AwaitingQuote then
            "Fetching quote, the start button will unlock shortly..."

         else
            "Press the start button to begin."
        )


viewWPMInfo : Html Msg
viewWPMInfo =
    div [ class "text-center m-4" ]
        [ h3 [] [ text "What's the difference between Benchmark WPM and Raw WPM?" ]
        , p [ class "text-left m-4" ]
            [ b [] [ text "Raw WPM " ]
            , text "considers full words, using the formula (# of words / time in minutes)"
            , br [] []
            , text "However, some words are longer/shorter than others, so Raw WPM is highly dependent on how \"wordy\" your random paragraph is."
            , br [] []
            , b [] [ text "Benchmark WPM " ]
            , text "uses "
            , a [ href "https://humanbenchmark.com/tests/typing" ] [ text "Human Benchmark" ]
            , text "'s formula, which considers a word to be an average of 5 characters. The formula is ((# of characters / 5) / time in minutes)"
            ]
        ]



-- HELPERS


getRawWPM : Model -> String
getRawWPM model =
    String.words model.currentParagraph
        |> List.filter (\w -> w /= "")
        |> length
        |> (\l -> toFloat l / (toFloat model.playTimeMs / 1000 / 60))
        |> convertNaNToZero
        |> Round.round 2


getBenchmarkWPM : Model -> String
getBenchmarkWPM model =
    String.toList model.currentParagraph
        |> length
        |> (\l -> (toFloat l / 5) / (toFloat model.playTimeMs / 1000 / 60))
        |> convertNaNToZero
        |> Round.round 2


convertNaNToZero : Float -> Float
convertNaNToZero float =
    if isNaN float then
        0

    else
        float


getRandomQuote : Cmd Msg
getRandomQuote =
    Http.get
        { url = "https://elm-lang.org/api/random-quotes"
        , expect = Http.expectJson GotQuote (field "quote" string)
        }


normalizeQuote : String -> String
normalizeQuote quote =
    quote
        |> String.replace "”" "\""
        |> String.replace "“" "\""
        |> String.replace "–" "-"
        |> String.replace "’" "'"
        |> String.replace "‘" "'"
        |> String.replace "—" "-"

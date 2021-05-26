module Main exposing (Agent(..), Model, Msg(..), State(..), agentTrigger, generateAgents, init, main, randomAgent, randomAgents, subscriptions, update, view, viewAgent, viewTime)

import Browser
import Browser.Events as Events
import Html exposing (Html, pre, text)
import Http
import Platform.Cmd as Cmd
import Random
import Time exposing (Posix)



{- todo

   - display as color
   - define relationships
   - create network

-}
-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { agents : List Agent
    , t : Int
    }


type State
    = Active
    | Passive


type Agent
    = Agent State Int


type Msg
    = NoOp
    | GotAgents (List Agent)
    | Tick Posix


randomAgent : Random.Generator Agent
randomAgent =
    Random.int 0 100
        |> Random.map (\i -> Agent Passive i)


randomAgents =
    Random.list 100 randomAgent


agentTrigger : Int -> Agent -> Agent
agentTrigger value (Agent state thresh) =
    case state of
        Active ->
            Agent state thresh

        Passive ->
            if value > thresh then
                Agent Active thresh

            else
                Agent Passive thresh


init : () -> ( Model, Cmd Msg )
init _ =
    ( { t = 0, agents = [] }
    , generateAgents
    )



-- UPDATE


generateAgents : Cmd Msg
generateAgents =
    Random.generate GotAgents randomAgents


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GotAgents agents ->
            ( { model | agents = agents }, Cmd.none )

        Tick time ->
            ( { model | t = model.t + 1 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Events.onAnimationFrame Tick



-- VIEW


viewAgent : Agent -> String
viewAgent (Agent state thresh) =
    let
        t =
            String.fromInt thresh

        s =
            case state of
                Active ->
                    "x"

                Passive ->
                    "."
    in
    [ "(", s, ":", t, ")" ] |> String.join ""


viewTime : Int -> Html Msg
viewTime x =
    Html.p [] [ String.fromInt x |> text ]


view : Model -> Html Msg
view model =
    case model.agents of
        [] ->
            text "nothing to see"

        lst ->
            Html.div []
                [ viewTime model.t
                , String.join " " (List.map viewAgent lst) |> text
                ]

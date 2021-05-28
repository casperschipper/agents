module Main exposing (Agent(..), Model, Msg(..), State(..), agentTrigger, generateAgents, init, main, randomAgent, randomAgents, subscriptions, update, view, viewAgent, viewTime)

import Array
import Browser
import Browser.Events as Events
import Html exposing (Html, pre, text)
import Html.Attributes as Attr
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
    { agents : Array.Array Agent
    , t : Int
    }


type State
    = Active
    | Passive


type alias AgentState =
    { state : State
    , threshold : Int
    , connections : List Int
    }


type Agent
    = Agent AgentState


type Msg
    = NoOp
    | GotAgents (List Agent)
    | Tick Posix


randomAgent : Int -> Int -> Random.Generator Agent
randomAgent numConnections maxConnection =
    let
        connections =
            Random.list 10 (Random.int 0 maxConnection)

        thresh =
            Random.int 0 20

        rstate =
            Random.weighted ( 90, Active ) [ ( 90, Passive ) ]
    in
    Random.map3 (\i cs state -> Agent (AgentState state i cs)) thresh connections rstate


table : List (Html Msg) -> Html Msg
table items =
    let
        n =
            List.length items

        side =
            sqrt (toFloat n) |> floor

        indexed =
            List.map2 (\x y -> (x,y)) (List.range 0 n) items

        rowNumbers =
            List.range 0 side

        rows =
            List.map
                (\nRow ->
                    let
                        i0 =
                            nRow * side

                        slice =
                            List.filter (\( i, _ ) -> i > i0 && i < (i0 + side)) indexed |> List.map (\(_,x) -> x)

                    in
                    Html.tr [] (List.map (\item -> Html.td [] [item]) slice)
                )
                rowNumbers
    in
    Html.table [] rows


randomAgents =
    let
        num =
            100

        numConnections =
            num // 10
    in
    Random.list num (randomAgent numConnections num)


agentTrigger : Int -> Agent -> Agent
agentTrigger value (Agent st) =
    case st.state of
        Active ->
            Agent st

        Passive ->
            if value > st.threshold then
                Agent { st | state = Active }

            else
                Agent { st | state = Passive }


evaluate : Agent -> Model -> Agent
evaluate (Agent agent) model =
    let
        connectedAgents =
            List.foldl
                (\idx acc ->
                    Array.get idx model.agents
                        |> Maybe.map
                            (\(Agent a) ->
                                case a.state of
                                    Active ->
                                        1 + acc

                                    Passive ->
                                        acc
                            )
                        |> Maybe.withDefault acc
                )
                0
                agent.connections
    in
    agentTrigger connectedAgents (Agent agent)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { t = 0, agents = Array.empty }
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
            ( { model | agents = Array.fromList agents }, Cmd.none )

        Tick time ->
            if remainderBy 60 model.t == 0 then
                let
                    newAgents =
                        Array.map (\agent -> evaluate agent model) model.agents
                in
                    ( { model
                          | t = model.t + 1
                          , agents = newAgents
                      }
                    , Cmd.none
                    )
            else
                ({ model | t = model.t + 1}, Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Events.onAnimationFrame Tick



-- VIEW


viewAgent : Agent -> Html Msg
viewAgent (Agent state) =
    let
        t =
            String.fromInt state.threshold |>
                                             (\str ->
                                                  if String.length str == 1 then
                                                      "0" ++ str
                                                  else str)

        (s,color) =
            case state.state of
                Active ->
                    ("x","green")

                Passive ->
                    (".","red")

        string = [ "(", s, ":", t, ")" ] |> String.join "" 
    in
    Html.div [ Attr.style "color" color ] [ text string ] 
        


viewTime : Int -> Html Msg
viewTime x =
    Html.p [] [ String.fromInt x |> text ]


view : Model -> Html Msg
view model =
    let
        lst =
            Array.map viewAgent model.agents |> Array.toList
    in
    case lst of
        [] ->
            text "nothing to see here"

        ls ->
            Html.div []
                [ viewTime model.t
                , lst |> table
                ]

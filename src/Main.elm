module Main exposing (Agent(..), Model, Msg(..), State(..), agentTrigger, generateAgents, init, main, randomAgent, randomAgents, subscriptions, update, view, viewAgent, viewTime)

import Array
import Browser
import Browser.Events as Events
import Html exposing (Html, pre, text)
import Html.Attributes as Attr
import Html.Events as E
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


totalNumber = 900
nNeighbours = 8
startCells = 1
                          
              
                 

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
    , birth : Int
    }


type Agent
    = Agent AgentState


type Msg
    = NoOp
    | GotAgents (List Agent)
    | Tick Posix
    | Reset


randomAgent : Int -> Int -> Random.Generator Agent
randomAgent numConnections maxConnection =
    let
        connections =
            Random.list 10 (Random.int 0 maxConnection)

        thresh =
            Random.int 1 10

        rstate =
            Random.weighted ( 10, Active ) [ ( 100, Passive ) ]
    in
    Random.map3 (\i cs state -> Agent (AgentState state i cs 0)) thresh connections rstate


table : List (Html Msg) -> Html Msg
table items =
    let
        n =
            List.length items

        side =
            sqrt (toFloat n) |> floor

        indexed =
            List.map2 (\x y -> ( x, y )) (List.range 0 n) items

        rowNumbers =
            List.range 0 side

        rows =
            List.map
                (\nRow ->
                    let
                        i0 =
                            nRow * side

                        slice =
                            List.filter (\( i, _ ) -> i > i0 && i < (i0 + side)) indexed |> List.map (\( _, x ) -> x)
                    in
                    Html.tr [] (List.map (\item -> Html.td [] [ item ]) slice)
                )
                rowNumbers
    in
    Html.table [] rows


randomAgents =
    let
        num =
           totalNumber

        numConnections =
            2
    in
    Random.list num (randomAgent numConnections num)


agentTrigger : Int -> Int -> Agent -> Agent
agentTrigger now value (Agent st) =
    case st.state of
        Active ->
            let age =
                    (now - st.birth) > (value * 4)

                overStimulated =
                    value > (st.threshold + 1)
            in
            case (age,overStimulated) of
                (True,True) ->
                    Agent { st | state = Passive }

                (_,_) ->
                    Agent st

        Passive ->
            if value > st.threshold then
                Agent { st | state = Active, birth = now}

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
    agentTrigger model.t connectedAgents (Agent agent)


init : () -> ( Model, Cmd Msg )
init _ =
    ( { t = 0, agents = Array.empty }
    , generateAgents
    )



-- UPDATE


generateAgents : Cmd Msg
generateAgents =
    Random.generate GotAgents randomAgents


wrap : Int -> Int -> Int -> Int
wrap low high x =
    let
        range =
            low - high |> abs

        modded =
            remainderBy (x - low) range
    in
    if modded < 0 then
        high + x

    else
        low + x


neighbours : Int -> Int -> Int -> List Int
neighbours n max index =
    List.range 0 n |> List.map ((\x -> x - (n // 2)) >> wrap 0 max)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Reset ->
            ( model, generateAgents )

        GotAgents agents ->
            let
                max =
                    List.length agents

                f idx (Agent agent) =
                    Agent { agent | connections = agent.connections ++ neighbours nNeighbours max idx }

                connected =
                    List.indexedMap f agents
            in
            ( { model | agents = Array.fromList connected }, Cmd.none )

        Tick time ->
            if remainderBy 4 model.t == 0 then
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
                ( { model | t = model.t + 1 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Events.onAnimationFrame Tick



-- VIEW


viewAgent : Agent -> Html Msg
viewAgent (Agent state) =
    let
        t =
            String.fromInt state.threshold
     

        ( s, color ) =
            case state.state of
                Active ->
                    ( "x", "green" )

                Passive ->
                    ( ".", "red" )

        string =
            [ "(", s, ":", t, ")" ] |> String.join ""
    in
    Html.div [ Attr.style "background-color" color, Attr.style "width" "20px" ] [ text string ]


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
            Html.div [ Attr.style "font-size" "8px"]
                [ Html.button [ E.onClick Reset ]
                    [ text "reset" ]
                , viewTime
                    model.t
                , lst |> table
                ]

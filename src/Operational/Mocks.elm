module Operational.Mocks exposing (runMocked)

import Expect exposing (Expectation, fail, equal)
import Tuple exposing (first, second)


-- fixme: remove calls to crash

import Debug exposing (crash)


runMocked :
    { program
        | init : ( model, List primitive )
        , update : msg -> model -> ( model, List primitive )
    }
    -> List ( primitive, msg )
    -> model
    -> Expectation
runMocked program expectedCommands expectedFinal =
    let
        ( model, cmd ) =
            program.init
    in
        simulate program cmd model expectedCommands expectedFinal


simulate :
    { program
        | init : ( model, List primitive )
        , update : msg -> model -> ( model, List primitive )
    }
    -> List primitive
    -> model
    -> List ( primitive, msg )
    -> model
    -> Expectation
simulate program cmds currentModel expectedCommands expectedFinal =
    case ( cmds, expectedCommands ) of
        ( [], [] ) ->
            currentModel |> equal expectedFinal

        ( [], [ ( expectedCommand, _ ) ] ) ->
            fail ("expected command: " ++ toString expectedCommand)

        ( cmd :: queuedCmds, ( expectedCmd, response ) :: restExpectedCommands ) ->
            (cmd |> equal expectedCmd)
                &&& let
                        ( nextModel, nextCmds ) =
                            program.update response currentModel
                    in
                        simulate program
                            (queuedCmds ++ nextCmds)
                            nextModel
                            restExpectedCommands
                            expectedFinal

        x ->
            crash (toString x)


(&&&) : Expectation -> Expectation -> Expectation
(&&&) a b =
    Expect.all [ first, second ] ( a, b )

module Operational.Mocks exposing (runMocked)

{-| `Operational.Mocks` allows you to test side-effects of elm applications
as long as they're written with the help of `elm-operational`.

@docs runMocked
-}

import Expect exposing (Expectation, fail, equal)
import Tuple exposing (first, second)


-- fixme: remove calls to crash

import Debug exposing (crash)


{-| `runMocked component expectedEffects finalState` tests the given elm
`component`. It fails when one of the expected effects is not issues by the
application or if the final resulting state is not equal to `finalState`.
-}
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

        ( [], expectedCommands ) ->
            fail
                ("expected commands: "
                    ++ toString (List.map first expectedCommands)
                )

        ( [ cmd ], [] ) ->
            fail ("unexpected command: " ++ toString cmd)

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

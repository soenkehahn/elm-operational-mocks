module Operational.Mocks exposing (runMocked, Step(..))

{-| `Operational.Mocks` allows you to test side-effects of elm applications
as long as they're written with the help of `elm-operational`.

@docs runMocked
-}

import Expect exposing (Expectation, fail, equal)
import Tuple exposing (first, second)


type Step cmd msg model
    = ExpectedCmd cmd
    | SendMsg msg
    | InspectModel model


{-| `runMocked component expectedEffects finalState` tests the given elm
`component`. It fails when one of the expected effects is not issues by the
application or if the final resulting state is not equal to `finalState`.
-}
runMocked :
    { program
        | init : ( model, List cmd )
        , update : msg -> model -> ( model, List cmd )
    }
    -> List (Step cmd msg model)
    -> Expectation
runMocked program expectedSteps =
    simulate program
        expectedSteps
        program.init


simulate :
    { program
        | init : ( model, List cmd )
        , update : msg -> model -> ( model, List cmd )
    }
    -> List (Step cmd msg model)
    -> ( model, List cmd )
    -> Expectation
simulate program expectedSteps ( currentModel, cmds ) =
    case ( cmds, expectedSteps ) of
        ( [], [] ) ->
            Expect.pass

        ( [], (ExpectedCmd expectedCommand) :: _ ) ->
            fail ("expected command: " ++ toString expectedCommand)

        ( [ cmd ], [] ) ->
            fail ("unexpected command: " ++ toString cmd)

        ( cmds, [] ) ->
            fail ("unexpected commands: " ++ toString cmds)

        ( queuedCmds, (SendMsg msg) :: restExpectedSteps ) ->
            let
                ( nextModel, nextCmds ) =
                    program.update msg currentModel
            in
                simulate program
                    restExpectedSteps
                    ( nextModel, (queuedCmds ++ nextCmds) )

        ( queuedCmds, (InspectModel expected) :: restExpectedSteps ) ->
            (currentModel |> equal expected)
                &&& simulate program
                        restExpectedSteps
                        ( currentModel, queuedCmds )

        ( cmd :: queuedCmds, (ExpectedCmd expectedCmd) :: restExpectedSteps ) ->
            (cmd |> equal expectedCmd)
                &&& simulate program
                        restExpectedSteps
                        ( currentModel, queuedCmds )


(&&&) : Expectation -> Expectation -> Expectation
(&&&) a b =
    Expect.all [ first, second ] ( a, b )

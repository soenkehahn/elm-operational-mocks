module Operational.MocksSpec exposing (all)

import Test exposing (..)
import Test.Runner exposing (..)
import Expect exposing (..)
import Tuple exposing (..)
import Platform.Cmd exposing (..)
import Http exposing (..)
import Operational
import Operational.Mocks exposing (..)


type Msg
    = ServerResponse String
    | Error Http.Error


type TestCmd
    = Get String
    | Post String (() -> ())


requestInfo : TestCmd -> ( String, String )
requestInfo cmd =
    case cmd of
        Get url ->
            ( "GET", url )

        Post url _ ->
            ( "POST", url )


mkProgram { init, update } =
    { init =
        ( [], init )
    , update =
        \msg model ->
            case msg of
                Error _ ->
                    ( model, [] )

                ServerResponse s ->
                    ( model ++ [ s ], update s )
    }


all : Test
all =
    describe "Operational.Mocks"
        [ describe "compatibility with elm-operational"
            [ test "allows to use toCmds on the same program"
                (\() ->
                    let
                        program =
                            mkProgram
                                { init = []
                                , update = \_ -> []
                                }

                        convert : TestCmd -> Cmd Msg
                        convert _ =
                            getString "/url"
                                |> send
                                    (\result ->
                                        case result of
                                            Ok s ->
                                                ServerResponse s

                                            Err err ->
                                                Error err
                                    )

                        x :
                            { init : ( List String, Cmd Msg )
                            , update : Msg -> List String -> ( List String, Cmd Msg )
                            }
                        x =
                            Operational.toCmd convert program
                    in
                        pass
                )
            ]
        , describe "runMocked"
            [ test "allows to test a request command"
                (\() ->
                    let
                        program =
                            mkProgram
                                { init = [ Get "/foo" ]
                                , update = \_ -> []
                                }
                    in
                        runMocked program
                            [ ExpectedCmd (Get "/foo")
                            ]
                )
            , test "detects missing commands"
                (\() ->
                    let
                        program =
                            mkProgram
                                { init = []
                                , update = \_ -> []
                                }
                    in
                        runMocked program
                            [ ExpectedCmd (Get "/foo")
                            ]
                            |> getFailure
                            |> equal
                                (Just
                                    { given = Nothing
                                    , message =
                                        "expected command: "
                                            ++ toString (Get "/foo")
                                    }
                                )
                )
            , test "detects unexpected commands"
                (\() ->
                    let
                        program =
                            mkProgram
                                { init = [ Get "/foo" ]
                                , update = \_ -> []
                                }
                    in
                        runMocked program
                            []
                            |> getFailure
                            |> equal
                                (Just
                                    { given = Nothing
                                    , message =
                                        "unexpected command: "
                                            ++ toString (Get "/foo")
                                    }
                                )
                )
            , test "allows to inspect part of expected commands"
                (\() ->
                    let
                        program =
                            mkProgram
                                { init = [ Post "/foo" (\() -> ()) ]
                                , update = \_ -> []
                                }
                    in
                        runMocked program
                            [ InspectCmd
                                (requestInfo
                                    >> equal ( "POST", "/foo" )
                                )
                            ]
                )
            , test "detects wrong final states"
                (\() ->
                    let
                        program =
                            mkProgram
                                { init = [ Get "/foo" ]
                                , update = \_ -> []
                                }
                    in
                        runMocked program
                            [ InspectModel (equal [ "foo" ])
                            ]
                            |> getFailure
                            |> equal
                                (getFailure
                                    ([]
                                        |> equal [ "foo" ]
                                    )
                                )
                )
            ]
        , describe "update"
            [ test "allows to test a request command"
                (\() ->
                    let
                        program =
                            mkProgram
                                { init = []
                                , update =
                                    \s ->
                                        case s of
                                            "foo-response" ->
                                                [ Get "/bar" ]

                                            _ ->
                                                []
                                }
                    in
                        runMocked program
                            [ SendMsg (ServerResponse "foo-response")
                            , ExpectedCmd (Get "/bar")
                            ]
                )
            , test "detects missing commands"
                (\() ->
                    let
                        program =
                            mkProgram
                                { init = []
                                , update = \_ -> []
                                }
                    in
                        runMocked program
                            [ SendMsg (ServerResponse "foo-response")
                            , ExpectedCmd (Get "/bar")
                            ]
                            |> getFailure
                            |> equal
                                (Just
                                    { given = Nothing
                                    , message = "expected command: " ++ toString (Get "/bar")
                                    }
                                )
                )
            , test "detects wrong commands"
                (\() ->
                    let
                        program =
                            mkProgram
                                { init = []
                                , update =
                                    \s ->
                                        case s of
                                            "foo-response" ->
                                                [ Get "/bar" ]

                                            _ ->
                                                []
                                }
                    in
                        runMocked program
                            [ SendMsg (ServerResponse "foo-response")
                            , ExpectedCmd (Get "/baz")
                            ]
                            |> getFailure
                            |> equal
                                (getFailure
                                    (Get "/bar"
                                        |> equal (Get "/baz")
                                    )
                                )
                )
            ]
        , test "allows to test for multiple commands"
            (\() ->
                let
                    program =
                        mkProgram
                            { init = []
                            , update =
                                \s ->
                                    case s of
                                        "foo-response" ->
                                            [ Get "/bar"
                                            , Get "/baz"
                                            ]

                                        _ ->
                                            []
                            }
                in
                    runMocked program
                        [ SendMsg (ServerResponse "foo-response")
                        , ExpectedCmd (Get "/bar")
                        , ExpectedCmd (Get "/baz")
                        ]
            )
        , test "it allows to inspect the model"
            (\() ->
                let
                    program =
                        mkProgram
                            { init = []
                            , update = \_ -> []
                            }
                in
                    runMocked program
                        [ SendMsg (ServerResponse "foo-response")
                        , InspectModel (List.length >> equal 1)
                        ]
            )
        , test "bigger example"
            (\() ->
                let
                    program =
                        mkProgram
                            { init = [ Get "/init" ]
                            , update =
                                \s ->
                                    case s of
                                        "init-response" ->
                                            [ Get "/update"
                                            ]

                                        "update-response" ->
                                            [ Get "/last"
                                            ]

                                        _ ->
                                            []
                            }
                in
                    runMocked program
                        [ InspectModel (equal [])
                        , ExpectedCmd (Get "/init")
                        , InspectModel (equal [])
                        , SendMsg (ServerResponse "init-response")
                        , InspectModel (equal [ "init-response" ])
                        , ExpectedCmd (Get "/update")
                        , InspectModel (equal [ "init-response" ])
                        , SendMsg (ServerResponse "update-response")
                        , ExpectedCmd (Get "/last")
                        , InspectModel (equal [ "init-response", "update-response" ])
                        ]
            )
        ]

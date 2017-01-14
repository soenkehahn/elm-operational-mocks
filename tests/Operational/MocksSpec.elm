module Operational.MocksSpec exposing (all)

import Test exposing (..)
import Expect exposing (..)
import Tuple exposing (..)
import Array exposing (..)
import Platform.Cmd exposing (..)
import Operational.Mocks exposing (..)


type Msg
    = FooResponse String
    | Error String


type TestPrimitive
    = Get String


mkProgram { init, update } =
    { init =
        ( empty, init )
    , update =
        \msg model ->
            case msg of
                Error _ ->
                    ( model, [] )

                FooResponse s ->
                    ( push s model, update s )
    }


all : Test
all =
    describe "Operational.Mocks"
        [ describe "runMocked"
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
                            [ ( Get "/foo", FooResponse "bar" ) ]
                            (fromList [ "bar" ])
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
                            [ ( Get "/foo", FooResponse "bar" ) ]
                            (fromList [ "bar" ])
                            |> getFailure
                            |> equal
                                (Just
                                    { given = ""
                                    , message = "expected command: " ++ toString (Get "/foo")
                                    }
                                )
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
                            [ ( Get "/foo", FooResponse "bar" ) ]
                            (fromList [ "baz" ])
                            |> getFailure
                            |> equal
                                (getFailure
                                    (fromList [ "bar" ]
                                        |> equal (fromList [ "baz" ])
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
                                { init = [ Get "/foo" ]
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
                            [ ( Get "/foo", FooResponse "foo-response" )
                            , ( Get "/bar", FooResponse "bar-response" )
                            ]
                            (fromList [ "foo-response", "bar-response" ])
                )
            , test "detects missing commands"
                (\() ->
                    let
                        program =
                            mkProgram
                                { init = [ Get "/foo" ]
                                , update = \_ -> []
                                }
                    in
                        runMocked program
                            [ ( Get "/foo", FooResponse "foo-response" )
                            , ( Get "/bar", FooResponse "bar-response" )
                            ]
                            (fromList [ "foo-response", "bar-response" ])
                            |> getFailure
                            |> equal
                                (Just
                                    { given = ""
                                    , message = "expected command: " ++ toString (Get "/bar")
                                    }
                                )
                )
            , test "detects wrong commands"
                (\() ->
                    let
                        program =
                            mkProgram
                                { init = [ Get "/foo" ]
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
                            [ ( Get "/foo", FooResponse "foo-response" )
                            , ( Get "/baz", FooResponse "bar-response" )
                            ]
                            (fromList [ "foo-response", "bar-response" ])
                            |> getFailure
                            |> equal
                                (getFailure
                                    (Get "/bar"
                                        |> equal (Get "/baz")
                                    )
                                )
                )
            , test "detects wrong final states"
                (\() ->
                    let
                        program =
                            mkProgram
                                { init = [ Get "/foo" ]
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
                            [ ( Get "/foo", FooResponse "foo-response" )
                            , ( Get "/bar", FooResponse "bar-response" )
                            ]
                            (fromList [ "expected" ])
                            |> getFailure
                            |> equal
                                (getFailure
                                    (fromList [ "foo-response", "bar-response" ]
                                        |> equal (fromList [ "expected" ])
                                    )
                                )
                )
            ]
        , test "allows to let mocks return multiple messages"
            (\() ->
                let
                    program =
                        mkProgram
                            { init = [ Get "/foo" ]
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
                        [ ( Get "/foo", FooResponse "foo-response" )
                        , ( Get "/bar", FooResponse "bar-response" )
                        , ( Get "/baz", FooResponse "baz-response" )
                        ]
                        (fromList [ "foo-response", "bar-response", "baz-response" ])
            )
        ]

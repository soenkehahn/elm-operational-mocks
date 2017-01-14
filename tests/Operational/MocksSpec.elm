module Operational.MocksSpec exposing (all)

import Test exposing (..)
import Expect exposing (..)
import Tuple exposing (..)
import Array exposing (..)
import Platform.Cmd exposing (..)
import Http exposing (..)
import Operational
import Operational.Mocks exposing (..)


type Msg
    = FooResponse String
    | Error Http.Error


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
        [ describe "compatibility with elm-operational"
            [ test "allows to use toCmds on the same program"
                (\() ->
                    let
                        program =
                            mkProgram
                                { init = []
                                , update = \_ -> []
                                }

                        convert : TestPrimitive -> Cmd Msg
                        convert _ =
                            getString "/url"
                                |> send
                                    (\result ->
                                        case result of
                                            Ok s ->
                                                FooResponse s

                                            Err err ->
                                                Error err
                                    )

                        x :
                            { init : ( Array String, Cmd Msg )
                            , update : Msg -> Array String -> ( Array String, Cmd Msg )
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

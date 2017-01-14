module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (list, int, tuple, string)
import String
import Operational.MocksSpec


all : Test
all =
    describe "client app"
        [ Operational.MocksSpec.all
        ]

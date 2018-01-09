module TestMatrix
    exposing
        ( equal
        )

import Expect
import Internal.Tensor as T
import Matrix
import Test exposing (..)
import TestFuzz


equal : Test
equal =
    describe "equal"
        [ fuzz TestFuzz.matrix "is reflexive" <|
            \matrix ->
                T.equal matrix matrix
                    |> Expect.true "should be: matrix == matrix"
        , fuzz2 TestFuzz.matrix TestFuzz.matrix "is symmetric" <|
            \m1 m2 ->
                T.equal m1 m2
                    |> Expect.equal (T.equal m2 m1)
        , fuzz3 TestFuzz.matrix TestFuzz.matrix TestFuzz.matrix "is transitive" <|
            \m1 m2 m3 ->
                let
                    ( equal12, equal23, equal13 ) =
                        ( T.equal m1 m2
                        , T.equal m2 m3
                        , T.equal m1 m3
                        )
                in
                (not equal12 || not equal23 || equal13)
                    |> Expect.true "a & b => c   should be equiv to   -a | -b | c"
        ]

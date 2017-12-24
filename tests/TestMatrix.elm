module TestMatrix
    exposing
        ( transpose
        )

import Expect
import Matrix
import Test exposing (..)
import TestFuzz as Fuzz


transpose : Test
transpose =
    fuzz Fuzz.rawMatrix "Transposition is a symmetric application" <|
        \matrix ->
            matrix
                |> Matrix.transpose
                |> Matrix.transpose
                |> Expect.equal matrix

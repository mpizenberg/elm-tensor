module TestMatrix
    exposing
        ( equal
        , transpose
        , unsafeSubmatrix
        )

import Expect
import Internal.Tensor as T
import List.Extra
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


transpose : Test
transpose =
    describe "transpose"
        [ fuzz TestFuzz.matrix "is a symmetry" <|
            \matrix ->
                matrix
                    |> Matrix.transpose
                    |> Matrix.transpose
                    |> T.equal matrix
                    |> Expect.true "should be: transpose (transpose matrix) == matrix"
        ]


unsafeSubmatrix : Test
unsafeSubmatrix =
    describe "unsafeSubmatrix"
        [ fuzz TestFuzz.submatrix "is coherent with unsafeGetAt" <|
            \( ( iStart, iEnd ), ( jStart, jEnd ), matrix ) ->
                let
                    ( height, width ) =
                        ( iEnd - iStart, jEnd - jStart )

                    submatrix =
                        Matrix.unsafeSubmatrix ( iStart, iEnd ) ( jStart, jEnd ) matrix

                    valuesFullMatrix =
                        [ List.range jStart (jEnd - 1), List.range iStart (iEnd - 1) ]
                            |> List.Extra.cartesianProduct
                            |> List.map List.reverse
                            |> List.map (\loc -> T.unsafeGetAt loc matrix)

                    valuesSubMatrix =
                        [ List.range 0 (width - 1), List.range 0 (height - 1) ]
                            |> List.Extra.cartesianProduct
                            |> List.map List.reverse
                            |> List.map (\loc -> T.unsafeGetAt loc submatrix)
                in
                valuesFullMatrix
                    |> Expect.equal valuesSubMatrix
        ]

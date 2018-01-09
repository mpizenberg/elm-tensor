module TestFuzz
    exposing
        ( matrix
        , rawMatrix
        , stridesMatrix
        , submatrix
        , transposedMatrix
        )

import Fuzz exposing (Fuzzer)
import Internal.Tensor as T
import JsFloat64Array
import JsTypedArray exposing (Float64, JsTypedArray, Uint8)
import Matrix exposing (Matrix)
import Random


maxSize : Int
maxSize =
    16


size : Fuzzer Int
size =
    Fuzz.intRange 0 maxSize


matrix : Fuzzer Matrix
matrix =
    Fuzz.oneOf [ rawMatrix, transposedMatrix, stridesMatrix ]


rawMatrix : Fuzzer Matrix
rawMatrix =
    Fuzz.tuple ( size, size )
        |> Fuzz.andThen (\( h, w ) -> Fuzz.map (T.unsafeFromTypedArray 2 [ h, w ]) (dataFuzzer ( h, w )))


transposedMatrix : Fuzzer Matrix
transposedMatrix =
    rawMatrix
        |> Fuzz.map Matrix.transpose


stridesMatrix : Fuzzer Matrix
stridesMatrix =
    rawMatrix
        |> Fuzz.andThen toStride


toStride : Matrix -> Fuzzer Matrix
toStride =
    toSubmatrix
        >> Fuzz.map (\( iRange, jRange, someM ) -> Matrix.unsafeSubmatrix iRange jRange someM)


submatrix : Fuzzer ( ( Int, Int ), ( Int, Int ), Matrix )
submatrix =
    matrix
        |> Fuzz.andThen toSubmatrix


toSubmatrix : Matrix -> Fuzzer ( ( Int, Int ), ( Int, Int ), Matrix )
toSubmatrix m =
    let
        ( height, width ) =
            Matrix.unsafeSize m

        rangeFuzzer maxValue =
            Fuzz.map2
                (\a b -> ( min a b, max a b ))
                (Fuzz.intRange 0 maxValue)
                (Fuzz.intRange 0 maxValue)
    in
    Fuzz.map2
        (\iRange jRange -> ( iRange, jRange, m ))
        (rangeFuzzer height)
        (rangeFuzzer width)



-- RANDOM GENERATORS HELPERS #########################################


dataFuzzer : ( Int, Int ) -> Fuzzer (JsTypedArray Float64 Float)
dataFuzzer ( height, width ) =
    jsFloat64Array (height * width)


jsFloat64Array : Int -> Fuzzer (JsTypedArray Float64 Float)
jsFloat64Array arrayLength =
    randomJsTypedArray JsFloat64Array.fromList (Random.float 0 1) arrayLength


randomJsTypedArray : (List b -> JsTypedArray a b) -> Random.Generator b -> Int -> Fuzzer (JsTypedArray a b)
randomJsTypedArray arrayFromList generator arrayLength =
    Fuzz.map (generateRandomArray arrayFromList generator arrayLength) Fuzz.int


generateRandomArray : (List b -> JsTypedArray a b) -> Random.Generator b -> Int -> Int -> JsTypedArray a b
generateRandomArray arrayFromList generator arrayLength intSeed =
    Random.initialSeed intSeed
        |> Random.step (Random.list arrayLength generator)
        |> Tuple.first
        |> arrayFromList

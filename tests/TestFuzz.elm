module TestFuzz
    exposing
        ( rawMatrix
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


rawMatrix : Fuzzer Matrix
rawMatrix =
    Fuzz.tuple ( size, size )
        |> Fuzz.andThen (\( h, w ) -> Fuzz.map (T.fromTypedArray [ h, w ]) (dataFuzzer ( h, w )))


transposedMatrix : Fuzzer Matrix
transposedMatrix =
    rawMatrix
        |> Fuzz.map Matrix.transpose



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

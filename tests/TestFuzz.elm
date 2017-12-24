module TestFuzz
    exposing
        ( arrangedMatrix
        , matrix
        , rawMatrix
        , transposedMatrix
        )

import Fuzz exposing (Fuzzer)
import JsFloat64Array
import JsTypedArray exposing (Float64, JsTypedArray, Uint8)
import Matrix exposing (Matrix)
import Random


maxDim : Int
maxDim =
    2


maxSize : Int
maxSize =
    16


size : Fuzzer Int
size =
    Fuzz.intRange 0 maxSize


rawMatrix : Fuzzer Matrix
rawMatrix =
    Fuzz.tuple ( size, size )
        |> Fuzz.andThen (\size -> Fuzz.map (Matrix.fromTypedArray size) (dataFuzzer size))


transposedMatrix : Fuzzer Matrix
transposedMatrix =
    rawMatrix
        |> Fuzz.map Matrix.transpose


arrangedMatrix : Fuzzer Matrix
arrangedMatrix =
    Debug.crash "TODO"


matrix : Fuzzer Matrix
matrix =
    Debug.crash "TODO"



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

module Internal.Tensor
    exposing
        ( FloatArray
        , IntArray
        , Tensor
        , TensorView(..)
        , extractValues
        )

import JsTypedArray exposing (Float64, JsTypedArray, Uint8)
import JsUint8Array


type alias Tensor =
    { data : FloatArray
    , dimension : Int
    , length : Int
    , shape : IntArray
    , view : TensorView
    }


type TensorView
    = RawView
    | TransposedView
    | ArrangedView { offset : Int, strides : IntArray }


type alias IntArray =
    JsTypedArray Uint8 Int


type alias FloatArray =
    JsTypedArray Float64 Float


stridesFromShape : Int -> List Int -> IntArray
stridesFromShape dimension shape =
    List.scanl (*) 1 shape
        |> List.take dimension
        |> JsUint8Array.fromList


{-| Extract values of a Tensor.
-}
extractValues : Tensor -> FloatArray
extractValues tensor =
    Debug.crash "TODO"

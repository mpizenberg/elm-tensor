module Internal.Tensor
    exposing
        ( FloatArray
        , IntArray
        , Tensor
        , TensorView(..)
        )

import JsTypedArray exposing (Float64, JsTypedArray, Uint8)


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

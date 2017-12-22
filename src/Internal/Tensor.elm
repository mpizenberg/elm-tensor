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
    , view : TensorView
    }


type TensorView
    = RawView { shape : IntArray }
    | TransposedView { shape : IntArray }
    | ArrangedView { shape : IntArray, offset : Int, strides : IntArray }


type alias IntArray =
    JsTypedArray Uint8 Int


type alias FloatArray =
    JsTypedArray Float64 Float

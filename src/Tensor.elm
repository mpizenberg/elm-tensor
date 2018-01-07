module Tensor
    exposing
        ( Tensor
        , fromTypedArray
        , transpose
        )

{-| Tensor (as multidimensional array).

@docs Tensor, fromTypedArray

@docs transpose

-}

import Internal.Tensor as T exposing (FloatArray, IntArray, TensorView)
import JsTypedArray exposing (Float64, JsTypedArray, Uint8)
import JsUint8Array


{-| Tensor (as multidimensional array).
-}
type alias Tensor =
    T.Tensor


{-| Create a Tensor from a typed array.
-}
fromTypedArray : List Int -> JsTypedArray Float64 Float -> Tensor
fromTypedArray shape array =
    let
        shapeArray =
            JsUint8Array.fromList shape
    in
    { data = array
    , dimension = JsTypedArray.length shapeArray
    , length = JsTypedArray.length array
    , shape = shapeArray
    , view = T.RawView
    }


{-| Transpose a tensor.
-}
transpose : Tensor -> Tensor
transpose tensor =
    let
        transposedShape =
            JsTypedArray.reverse tensor.shape
    in
    case tensor.view of
        T.RawView ->
            { tensor | shape = transposedShape, view = T.TransposedView }

        T.TransposedView ->
            { tensor | shape = transposedShape, view = T.RawView }

        T.ArrangedView view ->
            let
                transposedView =
                    { view | strides = JsTypedArray.reverse view.strides }
            in
            { tensor | shape = transposedShape, view = T.ArrangedView transposedView }

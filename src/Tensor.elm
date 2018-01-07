module Tensor
    exposing
        ( Tensor
        , transpose
        )

{-| Tensor (as multidimensional array).

@docs Tensor

@docs transpose

-}

import Internal.Tensor as T exposing (FloatArray, IntArray, TensorView)
import JsTypedArray exposing (Float64, JsTypedArray, Uint8)


{-| Tensor (as multidimensional array).
-}
type alias Tensor =
    T.Tensor


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

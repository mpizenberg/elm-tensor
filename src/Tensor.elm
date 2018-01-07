module Tensor
    exposing
        ( Tensor
        , fromTypedArray
        , innerProduct
        , transpose
        )

{-| Tensor (as multidimensional array).

@docs Tensor, fromTypedArray

@docs transpose, innerProduct

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


{-| Compute the inner product of two tensors.
Warning! Does not check sizes or dimensions.
-}
innerProduct : Tensor -> Tensor -> Float
innerProduct =
    fold2 (\x y acc -> x * y + acc) 0


{-| TODO (look at Matrix.fold2)
-}
fold2 : (Float -> Float -> a -> a) -> a -> Tensor -> Tensor -> a
fold2 f acc tensor1 tensor2 =
    Debug.crash "TODO"

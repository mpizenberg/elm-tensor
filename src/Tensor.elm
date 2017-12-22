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

        dimension =
            JsTypedArray.length shapeArray
    in
    { data = array
    , dimension = dimension
    , view = T.RawView { shape = shapeArray }
    }


stridesFromShape : Int -> List Int -> IntArray
stridesFromShape dimension shape =
    List.scanl (*) 1 shape
        |> List.take dimension
        |> JsUint8Array.fromList


{-| Transpose a tensor.
-}
transpose : Tensor -> Tensor
transpose tensor =
    case tensor.view of
        T.RawView view ->
            { tensor | view = T.TransposedView view }

        T.TransposedView view ->
            { tensor | view = T.RawView view }

        T.ArrangedView { shape, offset, strides } ->
            let
                transposedView =
                    { shape = JsTypedArray.reverse shape
                    , offset = offset
                    , strides = JsTypedArray.reverse strides
                    }
            in
            { tensor | view = T.ArrangedView transposedView }


{-| Compute the inner product of two tensors.
Warning! Does not check sizes or dimensions.
-}
innerProduct : Tensor -> Tensor -> Float
innerProduct =
    fold2 (\x y acc -> x * y + acc) 0


fold2 : (Float -> Float -> a -> a) -> a -> Tensor -> Tensor -> a
fold2 f acc tensor1 tensor2 =
    case ( tensor1.view, tensor2.view ) of
        ( T.RawView _, T.RawView _ ) ->
            JsTypedArray.indexedFoldl2 (always f) acc tensor1.data tensor2.data

        ( T.ArrangedView v1, T.ArrangedView v2 ) ->
            acc

        _ ->
            acc

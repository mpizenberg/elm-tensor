module Internal.Tensor
    exposing
        ( Tensor
        , TensorView(..)
        , extractValues
        , fromTypedArray
        , transpose
        , unsafeGetAt
        )

import Internal.List
import JsTypedArray exposing (Float64, JsTypedArray)


type alias Tensor =
    { data : DataArray
    , dimension : Int
    , length : Int
    , shape : List Int
    , view : TensorView
    }


type TensorView
    = RawView
    | TransposedView
    | ArrangedView { offset : Int, strides : List Int }


type alias DataArray =
    JsTypedArray Float64 Float


{-| Transpose a tensor.
-}
transpose : Tensor -> Tensor
transpose tensor =
    let
        transposedShape =
            List.reverse tensor.shape
    in
    case tensor.view of
        RawView ->
            { tensor | shape = transposedShape, view = TransposedView }

        TransposedView ->
            { tensor | shape = transposedShape, view = RawView }

        ArrangedView view ->
            let
                transposedView =
                    { view | strides = List.reverse view.strides }
            in
            { tensor | shape = transposedShape, view = ArrangedView transposedView }


stridesFromShape : Int -> List Int -> List Int
stridesFromShape dimension shape =
    List.scanl (*) 1 shape
        |> List.take dimension


unsafeGetAt : List Int -> Tensor -> Float
unsafeGetAt pos tensor =
    let
        tensorStrides =
            case tensor.view of
                RawView ->
                    List.scanl (*) 1 tensor.shape

                TransposedView ->
                    List.reverse tensor.shape
                        |> List.scanl (*) 1
                        |> List.reverse
                        |> List.drop 1

                ArrangedView { strides } ->
                    strides

        index =
            tensorStrides
                |> Internal.List.foldl2 (\p stride acc -> p * stride + acc) 0 pos
    in
    JsTypedArray.unsafeGetAt index tensor.data


{-| Extract values of a Tensor.
-}
extractValues : Tensor -> DataArray
extractValues tensor =
    Debug.crash "TODO"


{-| Create a Tensor from a typed array.
-}
fromTypedArray : List Int -> JsTypedArray Float64 Float -> Tensor
fromTypedArray shape array =
    { data = array
    , dimension = List.length shape
    , length = JsTypedArray.length array
    , shape = shape
    , view = RawView
    }

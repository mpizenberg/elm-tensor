module Internal.Tensor
    exposing
        ( Tensor
        , TensorView(..)
        , equal
        , extractValues
        , map
        , transpose
        , unsafeFromTypedArray
        , unsafeGetAt
        , unsafeReshape
        )

import Internal.List
import JsFloat64Array
import JsTypedArray exposing (Float64, JsTypedArray)
import List.Extra


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
    | StridesView (List Int)


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

        StridesView strides ->
            { tensor | shape = transposedShape, view = StridesView (List.reverse strides) }


{-| Just as a reminder of how to do it.
-}
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

                StridesView strides ->
                    strides

        index =
            tensorStrides
                |> Internal.List.foldl2 (\p stride acc -> p * stride + acc) 0 pos
    in
    JsTypedArray.unsafeGetAt index tensor.data


{-| Check if two arrays are equal.
-}
equal : Tensor -> Tensor -> Bool
equal tensor1 tensor2 =
    case ( tensor1.view, tensor2.view ) of
        ( RawView, RawView ) ->
            (tensor1.shape == tensor2.shape)
                && JsTypedArray.equal tensor1.data tensor2.data

        ( TransposedView, TransposedView ) ->
            (tensor1.shape == tensor2.shape)
                && JsTypedArray.equal tensor1.data tensor2.data

        _ ->
            (tensor1.shape == tensor2.shape)
                && JsTypedArray.equal (extractValues tensor1) (extractValues tensor2)


{-| Extract values given a shape, strides and data array.
-}
extractValuesDetailed : List Int -> List Int -> DataArray -> DataArray
extractValuesDetailed shape strides data =
    Debug.crash "TODO"


{-| Extract values of a Tensor.
-}
extractValues : Tensor -> DataArray
extractValues tensor =
    -- case tensor.view of
    --     RawView ->
    --         tensor.data
    --
    --     TransposedView ->
    --         let
    --             tensorStrides =
    --                 List.reverse tensor.shape
    --                     |> List.scanl (*) 1
    --                     |> List.reverse
    --                     |> List.drop 1
    --
    --             subToIndex : List Int -> Int
    --             subToIndex =
    --                 Internal.List.foldl2 (\p stride acc -> p * stride + acc) 0 tensorStrides
    --
    --             allSubscripts : List (List Int)
    --             allSubscripts =
    --                 tensor.shape
    --                     |> List.foldl (\s ranges -> List.range 0 (s - 1) :: ranges) []
    --                     |> List.Extra.cartesianProduct
    --                     |> List.map List.reverse
    --
    --             indices =
    --                 List.map (subToIndex >> toFloat) allSubscripts
    --                     |> JsFloat64Array.fromList
    --
    --             get =
    --                 JsTypedArray.unsafeGetAt
    --         in
    --         JsTypedArray.map (\floatId -> get (round floatId) tensor.data) indices
    --
    --     ArrangedView { offset, strides } ->
    --         let
    --             indices =
    --                 Debug.crash "TODO"
    --
    --             get =
    --                 JsTypedArray.unsafeGetAt
    --
    --             getDataAt id =
    --                 get (get id indices) tensor.data
    --         in
    --         JsFloat64Array.initialize tensor.length getDataAt
    Debug.crash "TODO"


{-| Create a Tensor from a typed array.
-}
unsafeFromTypedArray : Int -> List Int -> JsTypedArray Float64 Float -> Tensor
unsafeFromTypedArray dimension shape array =
    { data = array
    , dimension = dimension
    , length = JsTypedArray.length array
    , shape = shape
    , view = RawView
    }


{-| Reshape a tensor. Unsafe.
It is caller responsability to make sure shapes are compatible.
If tensor was internally a strides view, recreate an new raw tensor.
-}
unsafeReshape : Int -> List Int -> Tensor -> Tensor
unsafeReshape dimension shape tensor =
    case tensor.view of
        StridesView strides ->
            { tensor
                | data = extractValuesDetailed shape strides tensor.data
                , dimension = dimension
                , shape = shape
                , view = RawView
            }

        _ ->
            { tensor
                | dimension = dimension
                , shape = shape
            }


{-| Apply a function to each element of a tensor.
-}
map : (Float -> Float) -> Tensor -> Tensor
map f tensor =
    case tensor.view of
        StridesView strides ->
            { tensor
                | view = RawView
                , data =
                    extractValuesDetailed tensor.shape strides tensor.data
                        |> JsTypedArray.map f
            }

        _ ->
            { tensor | data = JsTypedArray.map f tensor.data }

module Internal.Tensor
    exposing
        ( Tensor
        , TensorView(..)
        , equal
        , extractValues
        , fromTypedArray
        , transpose
        , unsafeGetAt
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
fromTypedArray : List Int -> JsTypedArray Float64 Float -> Tensor
fromTypedArray shape array =
    { data = array
    , dimension = List.length shape
    , length = JsTypedArray.length array
    , shape = shape
    , view = RawView
    }

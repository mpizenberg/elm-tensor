module Matrix
    exposing
        ( Matrix
        , fold2
        , fromTypedArray
        , identity
        , innerProduct
        , size
        , transpose
        , unsafeColumnAt
        , unsafeGetAt
        , unsafeLineAt
        , unsafeSubmatrix
        )

{-| Matrix.

@docs Matrix, fromTypedArray, identity

@docs size

@docs transpose, fold2, innerProduct, unsafeSubmatrix

@docs unsafeGetAt, unsafeColumnAt, unsafeLineAt

-}

import Internal.Tensor as T exposing (FloatArray, IntArray, TensorView)
import JsFloat64Array
import JsTypedArray exposing (Float64, JsTypedArray, Uint8)
import JsUint8Array
import Tensor exposing (Tensor)


{-| Matrix is an alias for Tensor.
It is your responsability to use it correctly.
-}
type alias Matrix =
    Tensor


{-| Create a Matrix from a typed array.
-}
fromTypedArray : ( Int, Int ) -> JsTypedArray Float64 Float -> Matrix
fromTypedArray ( height, width ) =
    Tensor.fromTypedArray [ height, width ]


{-| Identity matrix of a given size.
-}
identity : Int -> Matrix
identity size =
    let
        positiveSize =
            max 0 size

        length =
            positiveSize * positiveSize

        initArray =
            JsFloat64Array.initialize length

        valueInIdentity index _ =
            if isOnDiag positiveSize index then
                1
            else
                0

        data =
            JsTypedArray.indexedMap valueInIdentity initArray
    in
    fromTypedArray ( positiveSize, positiveSize ) data


isOnDiag : Int -> Int -> Bool
isOnDiag matrixHeight index =
    index % (matrixHeight + 1) == 0


{-| Create a matrix holding the same constant for each element.
-}
constant : Float -> ( Int, Int ) -> Matrix
constant value size =
    Debug.crash "TODO"


{-| Get the size of a matrix.
-}
size : Matrix -> ( Int, Int )
size tensor =
    case tensor.view of
        T.RawView { shape } ->
            ( JsTypedArray.unsafeGetAt 0 shape, JsTypedArray.unsafeGetAt 1 shape )

        T.TransposedView { shape } ->
            ( JsTypedArray.unsafeGetAt 1 shape, JsTypedArray.unsafeGetAt 0 shape )

        T.ArrangedView { shape } ->
            ( JsTypedArray.unsafeGetAt 0 shape, JsTypedArray.unsafeGetAt 1 shape )


{-| Transpose a matrix
-}
transpose : Matrix -> Matrix
transpose =
    Tensor.transpose


{-| Stack all elements of a matrix in one column vector.
-}
stack : Matrix -> Matrix
stack matrix =
    Debug.crash "TODO"


{-| Apply a function to each element of a matrix.
-}
map : (Float -> Float) -> Matrix -> Matrix
map f matrix =
    Debug.crash "TODO"


{-| Apply a function on all elements of two matrices and reduce a result.
TODO: Optimize when only one is an arranged view.
-}
fold2 : (Float -> Float -> a -> a) -> a -> Matrix -> Matrix -> a
fold2 f initialValue m1 m2 =
    case ( m1.view, m2.view ) of
        ( T.RawView _, T.RawView _ ) ->
            JsTypedArray.indexedFoldl2 (always f) initialValue m1.data m2.data

        ( T.RawView _, T.TransposedView _ ) ->
            JsTypedArray.foldlr f initialValue m1.data m2.data

        ( T.TransposedView _, T.RawView _ ) ->
            JsTypedArray.foldlr (flip f) initialValue m2.data m1.data

        ( T.TransposedView _, T.TransposedView _ ) ->
            JsTypedArray.indexedFoldr2 (always f) initialValue m1.data m2.data

        -- In the remaining cases, at least one is an ArrangedView.
        -- TODO: For now, no optimization, just walk through indices.
        _ ->
            let
                ( height1, width1 ) =
                    size m1

                ( height2, width2 ) =
                    size m2

                lines =
                    List.range 0 (min height1 height2 - 1)

                columns =
                    List.range 0 (min width1 width2 - 1)

                columnSubscripts c =
                    List.map (\l -> ( l, c )) lines

                subscripts =
                    List.concatMap columnSubscripts columns

                processSubscript sub acc =
                    f (unsafeGetAt sub m1) (unsafeGetAt sub m2) acc
            in
            List.foldl processSubscript initialValue subscripts


{-| Compute the inner product of two tensors.
Warning! Does not check sizes or dimensions.
-}
innerProduct : Matrix -> Matrix -> Float
innerProduct =
    fold2 (\x y acc -> x * y + acc) 0


{-| Extract a submatrix (unsafe).
Warning! Does not check if out of bounds.
-}
unsafeSubmatrix : ( Int, Int ) -> ( Int, Int ) -> Matrix -> Matrix
unsafeSubmatrix ( iStart, iEnd ) ( jStart, jEnd ) tensor =
    let
        ( height, width ) =
            size tensor

        ( newHeight, newWidth ) =
            ( iEnd - iStart, jEnd - jStart )

        newShape =
            JsUint8Array.fromList [ newHeight, newWidth ]
    in
    case tensor.view of
        T.RawView { shape } ->
            let
                offset =
                    jStart * height + iStart
            in
            if iStart == 0 && iEnd == height then
                let
                    subData =
                        JsTypedArray.extract offset (offset + newHeight * newWidth) tensor.data
                in
                { tensor | data = subData, view = T.RawView { shape = newShape } }
            else
                let
                    arrangedView =
                        { shape = newShape
                        , offset = offset
                        , strides = JsUint8Array.fromList [ 1, height ]
                        }
                in
                { tensor | view = T.ArrangedView arrangedView }

        T.TransposedView _ ->
            tensor
                |> transpose
                |> unsafeSubmatrix ( jStart, jEnd ) ( iStart, iEnd )
                |> transpose

        T.ArrangedView { shape, offset, strides } ->
            let
                vStride =
                    JsTypedArray.unsafeGetAt 0 strides

                hStride =
                    JsTypedArray.unsafeGetAt 1 strides

                newArrangedView =
                    { shape = newShape
                    , offset = offset + jStart * hStride + iStart * vStride
                    , strides = strides
                    }
            in
            { tensor | view = T.ArrangedView newArrangedView }


{-| Access value in a matrix (unsafe).
Warning! Does not check if out of bounds.
-}
unsafeGetAt : ( Int, Int ) -> Matrix -> Float
unsafeGetAt ( line, column ) tensor =
    case tensor.view of
        T.RawView { shape } ->
            let
                height =
                    JsTypedArray.unsafeGetAt 0 shape

                index =
                    height * column + line
            in
            JsTypedArray.unsafeGetAt index tensor.data

        T.TransposedView { shape } ->
            let
                width =
                    JsTypedArray.unsafeGetAt 0 shape

                index =
                    width * line + column
            in
            JsTypedArray.unsafeGetAt index tensor.data

        T.ArrangedView { shape, offset, strides } ->
            let
                height =
                    JsTypedArray.unsafeGetAt 0 shape

                vStride =
                    JsTypedArray.unsafeGetAt 0 strides

                hStride =
                    JsTypedArray.unsafeGetAt 1 strides

                index =
                    offset + hStride * column + vStride * line
            in
            JsTypedArray.unsafeGetAt index tensor.data


{-| Extract a line from a matrix (unsafe).
Warning! Does not check if out of bounds.
-}
unsafeLineAt : Int -> Matrix -> Matrix
unsafeLineAt i tensor =
    case tensor.view of
        T.RawView { shape } ->
            let
                width =
                    JsTypedArray.unsafeGetAt 1 shape

                newShape =
                    JsUint8Array.fromList [ 1, width ]

                offset =
                    i

                oldHeight =
                    JsTypedArray.unsafeGetAt 0 shape

                strides =
                    JsUint8Array.fromList [ 1, oldHeight ]

                arrangedView =
                    T.ArrangedView
                        { shape = newShape, offset = offset, strides = strides }
            in
            { tensor | view = arrangedView }

        _ ->
            tensor
                |> transpose
                |> unsafeColumnAt i
                |> transpose


{-| Extract a column from a matrix (unsafe).
Warning! Does not check if out of bounds.
-}
unsafeColumnAt : Int -> Matrix -> Matrix
unsafeColumnAt j tensor =
    case tensor.view of
        T.RawView { shape } ->
            let
                height =
                    JsTypedArray.unsafeGetAt 0 shape

                matrixOffset =
                    j * height

                columnData =
                    JsTypedArray.extract matrixOffset (matrixOffset + height) tensor.data

                newShape =
                    JsUint8Array.fromList [ height, 1 ]
            in
            { tensor | data = columnData, view = T.RawView { shape = newShape } }

        T.TransposedView _ ->
            tensor
                |> transpose
                |> unsafeLineAt j
                |> transpose

        T.ArrangedView { shape, offset, strides } ->
            let
                height =
                    JsTypedArray.unsafeGetAt 0 shape

                vStride =
                    JsTypedArray.unsafeGetAt 0 strides

                hStride =
                    JsTypedArray.unsafeGetAt 1 strides

                newOffset =
                    offset + j * hStride

                newShape =
                    JsUint8Array.fromList [ height, 1 ]
            in
            if vStride == 1 then
                let
                    columnData =
                        JsTypedArray.extract newOffset (newOffset + height) tensor.data
                in
                { tensor | data = columnData, view = T.RawView { shape = newShape } }
            else
                let
                    newArrangedView =
                        { shape = newShape
                        , offset = newOffset
                        , strides = strides
                        }
                in
                { tensor | view = T.ArrangedView newArrangedView }


{-| Matrix multiplication
-}
times : Matrix -> Matrix -> Matrix
times m1 m2 =
    Debug.crash "TODO"


{-| Kronecker product
-}
kron : Matrix -> Matrix -> Matrix
kron m1 m2 =
    Debug.crash "TODO"

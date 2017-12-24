module Matrix
    exposing
        ( Matrix
        , constant
        , fold2
        , fromTypedArray
        , identity
        , innerProduct
        , ones
        , size
        , times
        , transpose
        , unsafeColumnAt
        , unsafeGetAt
        , unsafeLineAt
        , unsafeSubmatrix
        , zeros
        )

{-| Matrix.

@docs Matrix

@docs fromTypedArray, zeros, ones, constant, identity

@docs size

@docs transpose, fold2, innerProduct, unsafeSubmatrix

@docs unsafeGetAt, unsafeColumnAt, unsafeLineAt

@docs times

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


{-| Create a matrix of zeros.
-}
zeros : ( Int, Int ) -> Matrix
zeros ( height, width ) =
    let
        positiveHeight =
            max 0 height

        positiveWidth =
            max 0 width

        length =
            positiveHeight * positiveWidth

        data =
            JsFloat64Array.initialize length
    in
    fromTypedArray ( positiveHeight, positiveWidth ) data


{-| Create a matrix of ones.
-}
ones : ( Int, Int ) -> Matrix
ones size =
    constant 1 size


{-| Create a matrix holding the same constant for each element.
-}
constant : Float -> ( Int, Int ) -> Matrix
constant value ( height, width ) =
    let
        positiveHeight =
            max 0 height

        positiveWidth =
            max 0 width

        length =
            positiveHeight * positiveWidth

        data =
            JsFloat64Array.initialize length
                |> JsTypedArray.replaceWithConstant 0 length value
    in
    fromTypedArray ( positiveHeight, positiveWidth ) data


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
            case index % (positiveSize + 1) of
                0 ->
                    1

                _ ->
                    0

        data =
            JsTypedArray.indexedMap valueInIdentity initArray
    in
    fromTypedArray ( positiveSize, positiveSize ) data


{-| Get the size of a matrix.
-}
size : Matrix -> ( Int, Int )
size matrix =
    ( JsTypedArray.unsafeGetAt 0 matrix.shape, JsTypedArray.unsafeGetAt 1 matrix.shape )


{-| Transpose a matrix.
-}
transpose : Matrix -> Matrix
transpose =
    Tensor.transpose


{-| Reshape a matrix.
It is caller responsability to make sure shapes are compatible.
If matrix was internally an arranged view, recreate an new raw matrix.
-}
reshape : JsTypedArray Uint8 Int -> Matrix -> Matrix
reshape shape matrix =
    case matrix.view of
        T.ArrangedView _ ->
            extractRaw matrix
                |> reshape shape

        _ ->
            { matrix | shape = shape }


{-| Stack all elements of a matrix in one column vector.
If matrix was internally an arranged view, recreate an new raw matrix.
-}
stack : Matrix -> Matrix
stack matrix =
    Debug.crash "TODO"


{-| If the matrix is internally an arranged matrix extract values to form a new raw matrix.
Otherwise leave as it is.
-}
extractRaw : Matrix -> Matrix
extractRaw matrix =
    Debug.crash "TODO"


{-| Apply a function to each element of a matrix.
-}
map : (Float -> Float) -> Matrix -> Matrix
map f matrix =
    case matrix.view of
        T.ArrangedView _ ->
            extractRaw matrix
                |> map f

        _ ->
            { matrix | data = JsTypedArray.indexedMap (always f) matrix.data }


{-| Apply a function on all elements of two matrices and reduce a result.
The two matrices are supposed to be of the same size, but this is
the caller responsability.
TODO: Optimize when only one is an arranged view.
-}
fold2 : (Float -> Float -> a -> a) -> a -> Matrix -> Matrix -> a
fold2 f initialValue m1 m2 =
    case ( m1.view, m2.view ) of
        ( T.RawView, T.RawView ) ->
            JsTypedArray.indexedFoldl2 (always f) initialValue m1.data m2.data

        ( T.TransposedView, T.TransposedView ) ->
            JsTypedArray.indexedFoldr2 (always f) initialValue m1.data m2.data

        ( T.RawView, T.TransposedView ) ->
            JsTypedArray.foldlr f initialValue m1.data m2.data

        ( T.TransposedView, T.RawView ) ->
            JsTypedArray.foldlr (flip f) initialValue m2.data m1.data

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


{-| Compute the inner product of two matrices.
Warning! Does not check sizes or dimensions.
-}
innerProduct : Matrix -> Matrix -> Float
innerProduct =
    fold2 (\x y acc -> x * y + acc) 0


{-| Extract a submatrix (unsafe).
Warning! Does not check if out of bounds.
-}
unsafeSubmatrix : ( Int, Int ) -> ( Int, Int ) -> Matrix -> Matrix
unsafeSubmatrix ( iStart, iEnd ) ( jStart, jEnd ) matrix =
    let
        ( height, width ) =
            size matrix

        ( newHeight, newWidth ) =
            ( iEnd - iStart, jEnd - jStart )

        newLength =
            newHeight * newWidth

        newShape =
            JsUint8Array.fromList [ newHeight, newWidth ]
    in
    case matrix.view of
        T.RawView ->
            let
                offset =
                    jStart * height + iStart
            in
            if iStart == 0 && iEnd == height then
                let
                    subData =
                        JsTypedArray.extract offset (offset + newHeight * newWidth) matrix.data
                in
                { matrix | data = subData, length = newLength, shape = newShape }
            else
                let
                    arrangedView =
                        { offset = offset
                        , strides = JsUint8Array.fromList [ 1, height ]
                        }
                in
                { matrix | length = newLength, shape = newShape, view = T.ArrangedView arrangedView }

        -- TODO: indline optimization
        T.TransposedView ->
            matrix
                |> transpose
                |> unsafeSubmatrix ( jStart, jEnd ) ( iStart, iEnd )
                |> transpose

        T.ArrangedView { offset, strides } ->
            let
                vStride =
                    JsTypedArray.unsafeGetAt 0 strides

                hStride =
                    JsTypedArray.unsafeGetAt 1 strides

                newArrangedView =
                    { offset = offset + jStart * hStride + iStart * vStride
                    , strides = strides
                    }
            in
            { matrix | length = newLength, shape = newShape, view = T.ArrangedView newArrangedView }


{-| Access value in a matrix (unsafe).
Warning! Does not check if out of bounds.
-}
unsafeGetAt : ( Int, Int ) -> Matrix -> Float
unsafeGetAt ( line, column ) matrix =
    case matrix.view of
        T.RawView ->
            let
                height =
                    JsTypedArray.unsafeGetAt 0 matrix.shape

                index =
                    height * column + line
            in
            JsTypedArray.unsafeGetAt index matrix.data

        T.TransposedView ->
            let
                width =
                    JsTypedArray.unsafeGetAt 1 matrix.shape

                index =
                    width * line + column
            in
            JsTypedArray.unsafeGetAt index matrix.data

        T.ArrangedView { offset, strides } ->
            let
                vStride =
                    JsTypedArray.unsafeGetAt 0 strides

                hStride =
                    JsTypedArray.unsafeGetAt 1 strides

                index =
                    offset + hStride * column + vStride * line
            in
            JsTypedArray.unsafeGetAt index matrix.data


{-| Extract a line from a matrix (unsafe).
Warning! Does not check if out of bounds.
-}
unsafeLineAt : Int -> Matrix -> Matrix
unsafeLineAt i matrix =
    case matrix.view of
        T.RawView ->
            let
                width =
                    JsTypedArray.unsafeGetAt 1 matrix.shape

                newShape =
                    JsUint8Array.fromList [ 1, width ]

                offset =
                    i

                oldHeight =
                    JsTypedArray.unsafeGetAt 0 matrix.shape

                strides =
                    JsUint8Array.fromList [ 1, oldHeight ]

                arrangedView =
                    T.ArrangedView { offset = offset, strides = strides }
            in
            { matrix | length = width, shape = newShape, view = arrangedView }

        _ ->
            matrix
                |> transpose
                |> unsafeColumnAt i
                |> transpose


{-| Extract a column from a matrix (unsafe).
Warning! Does not check if out of bounds.
-}
unsafeColumnAt : Int -> Matrix -> Matrix
unsafeColumnAt j matrix =
    case matrix.view of
        T.RawView ->
            let
                height =
                    JsTypedArray.unsafeGetAt 0 matrix.shape

                matrixOffset =
                    j * height

                columnData =
                    JsTypedArray.extract matrixOffset (matrixOffset + height) matrix.data

                newShape =
                    JsUint8Array.fromList [ height, 1 ]
            in
            { matrix | data = columnData, length = height, shape = newShape }

        T.TransposedView ->
            matrix
                |> transpose
                |> unsafeLineAt j
                |> transpose

        T.ArrangedView { offset, strides } ->
            let
                height =
                    JsTypedArray.unsafeGetAt 0 matrix.shape

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
                        JsTypedArray.extract newOffset (newOffset + height) matrix.data
                in
                { matrix | data = columnData, shape = newShape, view = T.RawView }
            else
                let
                    newArrangedView =
                        { offset = newOffset
                        , strides = strides
                        }
                in
                { matrix | length = height, shape = newShape, view = T.ArrangedView newArrangedView }


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

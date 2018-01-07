module Matrix
    exposing
        ( Matrix
        , constant
        , identity
        , ones
        , size
        , times
        , transpose
        , unsafeColumnAt
        , unsafeGetAt
        , unsafeInnerProduct
        , unsafeLineAt
        , unsafeSubmatrix
        , zeros
        )

{-| Matrix.

@docs Matrix

@docs zeros, ones, constant, identity

@docs size

@docs transpose, unsafeInnerProduct, unsafeSubmatrix

@docs unsafeGetAt, unsafeColumnAt, unsafeLineAt

@docs times

-}

import Internal.Tensor as T exposing (FloatArray, IntArray, TensorView)
import JsFloat64Array
import JsTypedArray exposing (Float64, JsTypedArray, Uint8)
import JsUint8Array
import Tensor exposing (Tensor, fromTypedArray)


{-| Matrix is an alias for Tensor.
It is your responsability to use it correctly.
-}
type alias Matrix =
    Tensor


{-| Create a matrix of zeros.
-}
zeros : ( Int, Int ) -> Matrix
zeros ( height, width ) =
    let
        positiveHeight =
            max 0 height

        positiveWidth =
            max 0 width

        data =
            JsFloat64Array.zeros (positiveHeight * positiveWidth)
    in
    fromTypedArray [ positiveHeight, positiveWidth ] data


{-| Create a matrix of ones.
-}
ones : ( Int, Int ) -> Matrix
ones =
    constant 1


{-| Create a matrix holding the same constant for each element.
-}
constant : Float -> ( Int, Int ) -> Matrix
constant value ( height, width ) =
    let
        positiveHeight =
            max 0 height

        positiveWidth =
            max 0 width

        data =
            JsFloat64Array.repeat (positiveHeight * positiveWidth) value
    in
    fromTypedArray [ positiveHeight, positiveWidth ] data


{-| Identity matrix of a given size.
-}
identity : Int -> Matrix
identity size =
    let
        positiveSize =
            max 0 size

        valueInIdentity index =
            case index % (positiveSize + 1) of
                0 ->
                    1

                _ ->
                    0

        data =
            JsFloat64Array.initialize (positiveSize * positiveSize) valueInIdentity
    in
    fromTypedArray [ positiveSize, positiveSize ] data


{-| Get the size of a matrix.
-}
size : Matrix -> ( Int, Int )
size matrix =
    ( JsTypedArray.unsafeGetAt 0 matrix.shape
    , JsTypedArray.unsafeGetAt 1 matrix.shape
    )


{-| Transpose a matrix.
-}
transpose : Matrix -> Matrix
transpose =
    Tensor.transpose


{-| Reshape a matrix. Unsafe.
It is caller responsability to make sure shapes are compatible.
If matrix was internally an arranged view, recreate an new raw matrix.
-}
reshapeUnsafe : JsTypedArray Uint8 Int -> Matrix -> Matrix
reshapeUnsafe shape matrix =
    case matrix.view of
        T.ArrangedView _ ->
            { matrix | shape = shape, view = T.RawView, data = T.extractValues matrix }

        _ ->
            { matrix | shape = shape }


{-| Stack all elements of a matrix in one column vector.

Complexity:

  - RawView and TransposedView: O(1)
  - ArrangedView: O(length)

-}
stack : Matrix -> Matrix
stack matrix =
    let
        newShape =
            JsUint8Array.fromList [ matrix.length, 1 ]
    in
    reshapeUnsafe newShape matrix


{-| Apply a function to each element of a matrix.
-}
map : (Float -> Float) -> Matrix -> Matrix
map f matrix =
    case matrix.view of
        T.ArrangedView _ ->
            { matrix
                | view = T.RawView
                , data = JsTypedArray.map f (T.extractValues matrix)
            }

        _ ->
            { matrix | data = JsTypedArray.map f matrix.data }


{-| Apply a function on all elements of two matrices and reduce a result.
The two matrices are supposed to be of the same size, but this is
the caller responsability make sure of this.
TODO: Optimize the number of created arrays for non raw views.
-}
unsafeFold2 : (Float -> Float -> a -> a) -> a -> Matrix -> Matrix -> a
unsafeFold2 f initialValue m1 m2 =
    case ( m1.view, m2.view ) of
        ( T.RawView, T.RawView ) ->
            JsTypedArray.foldl2 f initialValue m1.data m2.data

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
unsafeInnerProduct : Matrix -> Matrix -> Float
unsafeInnerProduct =
    unsafeFold2 (\x y acc -> x * y + acc) 0


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

        -- TODO: inline optimization
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

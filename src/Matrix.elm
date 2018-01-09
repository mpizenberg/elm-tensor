module Matrix
    exposing
        ( Matrix
        , transpose
        , unsafeColumnAt
        , unsafeConstant
        , unsafeGetAt
        , unsafeIdentity
        , unsafeInnerProduct
        , unsafeLineAt
        , unsafeOnes
        , unsafeSize
        , unsafeSubmatrix
        , unsafeZeros
        )

{-| Matrix.

@docs Matrix

@docs unsafeZeros, unsafeOnes, unsafeConstant, unsafeIdentity

@docs unsafeSize

@docs transpose, unsafeInnerProduct, unsafeSubmatrix

@docs unsafeGetAt, unsafeColumnAt, unsafeLineAt

-}

import Internal.List
import Internal.Tensor as T exposing (TensorView, unsafeFromTypedArray)
import JsFloat64Array
import JsTypedArray exposing (Float64, JsTypedArray)
import Tensor exposing (Tensor)


{-| Matrix is an alias for Tensor.
It is your responsability to use it correctly.
-}
type alias Matrix =
    Tensor


{-| Create a matrix of zeros.
-}
unsafeZeros : ( Int, Int ) -> Matrix
unsafeZeros ( height, width ) =
    JsFloat64Array.zeros (height * width)
        |> unsafeFromTypedArray 2 [ height, width ]


{-| Create a matrix of ones.
-}
unsafeOnes : ( Int, Int ) -> Matrix
unsafeOnes =
    unsafeConstant 1


{-| Create a matrix holding the same constant for each element.
-}
unsafeConstant : Float -> ( Int, Int ) -> Matrix
unsafeConstant value ( height, width ) =
    JsFloat64Array.repeat (height * width) value
        |> unsafeFromTypedArray 2 [ height, width ]


{-| Identity matrix of a given size.
-}
unsafeIdentity : Int -> Matrix
unsafeIdentity size =
    let
        stepSize =
            size + 1

        valueInIdentity index =
            case index % stepSize of
                0 ->
                    1

                _ ->
                    0
    in
    JsFloat64Array.initialize (size * size) valueInIdentity
        |> unsafeFromTypedArray 2 [ size, size ]


{-| Get the size of a matrix.
-}
unsafeSize : Matrix -> ( Int, Int )
unsafeSize matrix =
    case matrix.shape of
        height :: width :: _ ->
            ( height, width )

        _ ->
            Debug.crash "This should never happen"


{-| Transpose a matrix.
-}
transpose : Matrix -> Matrix
transpose =
    T.transpose


{-| Stack all elements of a matrix in one column vector.

Complexity:

  - RawView and TransposedView: O(1)
  - ArrangedView: O(length)

-}
stack : Matrix -> Matrix
stack matrix =
    T.unsafeReshape [ matrix.length, 1 ] matrix


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
                ( height, width ) =
                    unsafeSize m1

                lines =
                    List.range 0 (height - 1)

                columns =
                    List.range 0 (width - 1)

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
            unsafeSize matrix

        ( newHeight, newWidth ) =
            ( iEnd - iStart, jEnd - jStart )

        newLength =
            newHeight * newWidth

        newShape =
            [ newHeight, newWidth ]
    in
    case matrix.view of
        T.RawView ->
            let
                offset =
                    iStart + jStart * height

                endIndex =
                    iEnd + (jEnd - 1) * height

                subData =
                    JsTypedArray.extract offset endIndex matrix.data
            in
            if iStart == 0 && iEnd == height then
                { matrix
                    | data = subData
                    , length = newLength
                    , shape = newShape
                }
            else
                { matrix
                    | data = subData
                    , length = newLength
                    , shape = newShape
                    , view = T.StridesView [ 1, height ]
                }

        -- TODO: inline optimization
        T.TransposedView ->
            matrix
                |> transpose
                |> unsafeSubmatrix ( jStart, jEnd ) ( iStart, iEnd )
                |> transpose

        T.StridesView strides ->
            let
                subToIndex =
                    Internal.List.foldl2 (\stride sub acc -> stride * sub + acc) 0 strides

                offset =
                    subToIndex [ iStart, jStart ]

                endIndex =
                    1 + subToIndex [ iEnd - 1, jEnd - 1 ]

                subData =
                    JsTypedArray.extract offset endIndex matrix.data
            in
            { matrix
                | data = subData
                , length = newLength
                , shape = newShape
            }


{-| Access value in a matrix (unsafe).
Warning! Does not check if out of bounds.
-}
unsafeGetAt : ( Int, Int ) -> Matrix -> Float
unsafeGetAt ( line, column ) =
    T.unsafeGetAt [ line, column ]


{-| Extract a line from a matrix (unsafe).
Warning! Does not check if out of bounds.
-}
unsafeLineAt : Int -> Matrix -> Matrix
unsafeLineAt i matrix =
    case matrix.view of
        T.RawView ->
            let
                ( height, width ) =
                    unsafeSize matrix

                endIndex =
                    1 + i + (width - 1) * height
            in
            { matrix
                | data = JsTypedArray.extract i endIndex matrix.data
                , length = width
                , shape = [ 1, width ]
                , view = T.StridesView [ 1, height ]
            }

        T.TransposedView ->
            -- Return a RawView instead of a TransposedView.
            -- TODO: inline optimization
            let
                transposedColumn =
                    transpose matrix
                        |> unsafeColumnAt i
            in
            { transposedColumn | shape = List.reverse transposedColumn.shape }

        T.StridesView _ ->
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
                ( height, width ) =
                    unsafeSize matrix

                offset =
                    j * height
            in
            { matrix
                | data = JsTypedArray.extract offset (offset + height) matrix.data
                , length = height
                , shape = [ height, 1 ]
            }

        -- TODO: inline optimization
        T.TransposedView ->
            matrix
                |> transpose
                |> unsafeLineAt j
                |> transpose

        T.StridesView strides ->
            let
                ( height, vStride, hStride ) =
                    case ( matrix.shape, strides ) of
                        ( h :: _, vStr :: hStr :: _ ) ->
                            ( h, vStr, hStr )

                        _ ->
                            Debug.crash "Should never happen"

                newShape =
                    [ height, 1 ]

                offset =
                    j * hStride

                endIndex =
                    1 + offset + (height - 1) * vStride

                columnData =
                    JsTypedArray.extract offset endIndex matrix.data
            in
            if vStride == 1 then
                { matrix
                    | data = columnData
                    , length = height
                    , shape = newShape
                    , view = T.RawView
                }
            else
                { matrix
                    | data = columnData
                    , length = height
                    , shape = newShape
                }


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

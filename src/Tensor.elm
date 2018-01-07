module Tensor
    exposing
        ( Tensor
        )

{-| Tensor (as multidimensional array).

@docs Tensor

-}

import Internal.Tensor as T


{-| Tensor (as multidimensional array).
-}
type alias Tensor =
    T.Tensor

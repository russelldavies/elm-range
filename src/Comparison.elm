module Comparison exposing (Comparison, instance)

{-| Typeclass which implements comparison operations for type `a`.
-}


type alias Comparison a =
    { compare : a -> a -> Order
    , eq : a -> a -> Bool
    , neq : a -> a -> Bool
    , lt : a -> a -> Bool
    , le : a -> a -> Bool
    , gt : a -> a -> Bool
    , ge : a -> a -> Bool
    , min : a -> a -> a
    , max : a -> a -> a
    }


{-| Create an instance from just a `compare` function.
-}
instance : (a -> a -> Order) -> Comparison a
instance compare_ =
    { compare = compare_
    , eq =
        \a b ->
            case compare_ a b of
                EQ ->
                    True

                _ ->
                    False
    , neq =
        \a b ->
            case compare_ a b of
                EQ ->
                    False

                _ ->
                    True
    , lt =
        \a b ->
            case compare_ a b of
                LT ->
                    True

                _ ->
                    False
    , le =
        \a b ->
            case compare_ a b of
                LT ->
                    True

                EQ ->
                    True

                _ ->
                    False
    , gt =
        \a b ->
            case compare_ a b of
                GT ->
                    True

                _ ->
                    False
    , ge =
        \a b ->
            case compare_ a b of
                GT ->
                    True

                EQ ->
                    True

                _ ->
                    False
    , min =
        \a b ->
            case compare_ a b of
                LT ->
                    a

                _ ->
                    b
    , max =
        \a b ->
            case compare_ a b of
                LT ->
                    b

                _ ->
                    a
    }

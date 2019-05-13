{-# LANGUAGE FlexibleInstances #-}

{-|
Module: Lambek
Description: Lambek grammatical types
License: GPL-3

This module implements Lambek grammatical types. This is done by
taking a free monoid over the base types with their left and right
adjoints and adding the Lambek pregroup reductions as equations for
that monoid.
|-}

module Discokitty.Lambek
  ( Type (..)
  , Lambek
  , agreeOn
  )
where

-- | Lambek basic grammatical types.
data Type = N | S | L Type | R Type deriving (Eq, Ord, Show)

-- | A Lambek pregroup type.
type Lambek = [Type]

-- | This operation checks if two types can be reduced. That is,
-- if one is the left/right adjoint of the other. Note that this is
-- not commutative.
(>~<) :: Type -> Type -> Bool
a     >~< (L b) = a == b
(R a) >~< b     = a == b
_     >~< _     = False

-- | Outputs true if the two Lambek pregroup words can be completely
-- reduced.
agree :: Lambek -> Lambek -> Bool
agree p = and . zipWith (>~<) p

-- | Checks if two Lambek types can be reduced a given number of
-- steps.
agreeOn :: Int -> Lambek -> Lambek -> Bool
agreeOn n p q = agree (take n (reverse p)) (take n q)

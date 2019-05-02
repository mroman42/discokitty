{-# LANGUAGE FlexibleInstances #-}

{-|
Module: Lambek
Description: Lambek grammatical types
License: GPL-3
|-}


module Lambek
  ( Type (..)
  , Lambek
  , agreeOn
  )
where

data Type = N | S | L Type | R Type deriving (Eq, Ord, Show)
type Lambek = [Type]


(>~<) :: Type -> Type -> Bool
a     >~< (L b) = (a == b)
(R a) >~< b     = (a == b)
_     >~< _     = False

agree :: Lambek -> Lambek -> Bool
agree p q = all id $ zipWith (>~<) p q

-- | Checks if two Lambek types can be reduced a given number of
-- times.
agreeOn :: Int -> Lambek -> Lambek -> Bool
agreeOn n p q = agree (take n (reverse p)) (take n q)

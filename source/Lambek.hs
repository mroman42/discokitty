{-# LANGUAGE FlexibleInstances #-}

module Lambek
  ( Type (..)
  , Lambek (..)
  , agreeOn
  )
where

import           Data.List
import           Data.Maybe
import           Dimension
import           HasCups
import           Rel


data Type = N | S | L Type | R Type deriving (Eq, Ord, Show)
type Lambek = [Type]


(>~<) :: Type -> Type -> Bool
a     >~< (L b) = (a == b)
(R a) >~< b     = (a == b)
c     >~< d     = False

agree :: Lambek -> Lambek -> Bool
agree p q = all id $ zipWith (>~<) p q

agreeOn :: Int -> Lambek -> Lambek -> Bool
agreeOn n p q = agree (take n (reverse p)) (take n q)

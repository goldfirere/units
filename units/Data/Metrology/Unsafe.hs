{-# LANGUAGE Unsafe, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Metrology.Unsafe
-- Copyright   :  (C) 2013 Richard Eisenberg
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  Richard Eisenberg (rae@cs.brynmawr.edu)
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module exports the constructor of the 'Qu' type. This allows you
-- to write code that takes creates and reads quantities at will,
-- which may lead to dimension unsafety. Use at your peril.
--
-- This module also exports 'UnsafeQu', which is a simple wrapper around
-- 'Qu' that has 'Functor', etc., instances. The reason 'Qu' itself doesn't
-- have a 'Functor' instance is that it would be unit-unsafe, allowing you,
-- say, to add 1 to a quantity.... but 1 what? That's the problem. However,
-- a 'Functor' instance is likely useful, hence 'UnsafeQu'.
-----------------------------------------------------------------------------

module Data.Metrology.Unsafe (
  -- * The 'Qu' type
  Qu(..),

  -- * 'UnsafeQu'
  UnsafeQu(..)
  ) where

import Data.Metrology.Qu

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
import Data.Foldable
import Data.Traversable
#endif

-- | A basic wrapper around 'Qu' that has more instances.
newtype UnsafeQu d l n = UnsafeQu { qu :: Qu d l n }

instance Functor (UnsafeQu d l) where
  fmap f (UnsafeQu (Qu x)) = UnsafeQu (Qu (f x))

instance Applicative (UnsafeQu d l) where
  pure x = UnsafeQu (Qu x)
  UnsafeQu (Qu f) <*> UnsafeQu (Qu x) = UnsafeQu (Qu (f x))

instance Foldable (UnsafeQu d l) where
  foldMap f (UnsafeQu (Qu x)) = f x

instance Traversable (UnsafeQu d l) where
  traverse f (UnsafeQu (Qu x)) = UnsafeQu . Qu <$> f x

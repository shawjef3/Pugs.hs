{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Pugs.AST.Scope (
    Scope(..),
) where
import Data.Typeable

-- | The scope of a variable declaration.
data Scope = SMy        -- ^ Ordinary lexically scoped variable
           | SConstant  -- ^ Lexically scoped alias to package variable
           | SHas       -- ^ Object attribute
           | SState     -- ^ Persistent lexical (cloned with closures)
           | SOur       -- ^ Lexically scoped compile-time constant
    deriving (Show, Eq, Ord, Enum, Typeable, Bounded)

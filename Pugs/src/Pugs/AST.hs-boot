{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, TypeSynonymInstances, FlexibleInstances #-}
module Pugs.AST where
import Pugs.Internals
import Pugs.Types
import Pugs.Class hiding (Val)
import {-# SOURCE #-} Pugs.AST.Internals

(./) :: ((:>:) Call a) => Val -> a -> Eval Val
instance ((:>:) Call) Cxt

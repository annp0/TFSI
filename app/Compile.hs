module Compile where

import Language.Haskell.TH
import qualified Language.Haskell.TH.Syntax as Syntax

newtype C a = C ExpQ

unC :: C a -> ExpQ
unC (C x) = x

liftC :: Syntax.Lift t => t -> C a
liftC x = C [||x||]

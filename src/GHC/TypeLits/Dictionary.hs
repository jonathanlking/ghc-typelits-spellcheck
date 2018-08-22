{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
module GHC.TypeLits.Dictionary
  ( Spellcheck )
where

import GHC.TypeLits (Symbol)

class Spellcheck (s :: Symbol)

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fplugin GHC.TypeLits.Dictionary.Solver #-}

import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit
import GHC.TypeLits (Symbol)

import GHC.TypeLits.Dictionary

spellcheck :: Spellcheck s => Proxy s
spellcheck = Proxy

test1 :: Proxy "hello"
test1 = spellcheck @"hello"

test2 :: Proxy "world"
test2 = spellcheck @"world"

-- I'm not sure what these tests actually do, apart from demonstrating
-- API usage!

tests :: TestTree
tests = testGroup "ghc-typelits-spellcheck"
   [ testGroup "Basic functionality"
     [ testCase "spellcheck @\"hello\"" $
       show test1 @?= "Proxy"
     , testCase "spellcheck @\"world\"" $
       show test2 @?= "Proxy"
     ]
   ]

main :: IO ()
main = defaultMain tests

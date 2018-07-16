{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# LANGUAGE DeriveFoldable       #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE DeriveTraversable    #-}
module Main where

import           BedRock
import           Run
import           Type

main :: IO ()
-- main = run "lib/test.raku"
main = print $ emit $
  Function
    { name = "main"
    , args = []
    , retT = int
    , body =
      let n = I 0
          r = Local n
       in B [] $ Return $ Lit $ Int 2
    }


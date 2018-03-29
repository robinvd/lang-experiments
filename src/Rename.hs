{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
module Rename where

import           Control.Monad.State
import           Data.Text           (Text)

import qualified Core                as Core
import           Type

rename :: Core.Core a Text -> (Core.Core a Name, Int)
rename core = runState (mapM f core) 0
  where f text = do
          i <- get
          return $ Name i text

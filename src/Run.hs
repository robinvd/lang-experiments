{-# LANGUAGE LambdaCase #-}
module Run where

import Prelude hiding (lookup)

import Bound
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Text.Megaparsec (parse, parseErrorPretty')
import Control.Monad.Except

import Core
import Interpreter
import Parser
import Typechecker

run :: String -> IO ()
run file = do
  c <- T.readFile file
  p <- case parse mainParse file c of
         Right x -> return x
         Left x -> do
           putStrLn $ parseErrorPretty' c x
           error "failed parsing"

  print p
  let t = ExceptT $ pure $ inferExpr mempty (fst . lookup <$> p) 
      e = t >> eval (snd . lookup <$> p)
  
  -- print t
  runExceptT e >>= \case
    Left err -> print err
    Right x -> case closed x of 
      Just x -> print (x :: Expr)
      Nothing -> putStrLn "end not a lit"

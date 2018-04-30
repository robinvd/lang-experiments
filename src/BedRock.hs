{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module BedRock where

import           Control.Monad
import           Data.Text                       (Text)
import qualified Data.Text                       as T

import qualified LLVM.AST                        as L
import           LLVM.AST.AddrSpace
import qualified LLVM.AST.Attribute              as A
import qualified LLVM.AST.CallingConvention      as CC
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.FloatingPointPredicate as F
import qualified LLVM.AST.Global                 as G
import qualified LLVM.AST.IntegerPredicate       as I
import qualified LLVM.AST.Linkage                as L
import qualified LLVM.AST.Type                   as AST.T
import           LLVM.AST.Typed
import           LLVM.IRBuilder                  hiding (call, double)

import           Emit                            (malloc)
import           Type                            hiding (Name)

data Function = Function
  { name :: Text
  , args :: [(Type,Name)]
  , retT :: Type
  , body :: Block
  }

-- nodes:
-- block always has one exit point (with a type)

-- solves the termintors return nothing
-- but not assign returns nothing
data Name = N Text | I Int

data Block
  = If Ref Block Block
  | B [Instruction] Terminator

data Ref
  = DeRef Int Ref
  | Ref Ref
  | Global Name
  | Local Name
  | Lit Lit

data Instruction
  = Alloc Name Type
  | Store Ref Instruction
  | Call Name Ref [Ref]

refLit l f = f $ Alloc (I 0) (typeLit l)

data Terminator
  = Return Ref
  | ReturnVoid

emit :: Function -> L.Definition
emit Function{..} =
  let (paramNames, blocks) = runIRBuilder emptyIRBuilder $ do
        paramNames <- replicateM (length args) fresh
        -- emitBlock (zipWith L.LocalReference (map typeOf argsT) paramNames) body
        emitBlock [] body
        return paramNames
  in L.GlobalDefinition L.functionDefaults
    { G.name = L.mkName $ T.unpack name
    , G.callingConvention = CC.Fast
    -- , linkage = if name == "main" then L.
    , G.parameters = ([], False)
      -- (zipWith
      --   (\ty nm ->
      --     G.Parameter (typeOf ty) (L.mkName $ T.unpack nm) []) argsT args, False)
    , G.returnType = typeOf retT
    , G.basicBlocks = blocks
    }

emitRet :: Terminator -> IRBuilder ()
emitRet = \case
  Return r -> ret =<< emitRef r

emitRef :: Ref -> IRBuilder L.Operand
emitRef = \case
  Lit l -> case l of
    Int i -> do
      space <- alloca AST.T.i64 Nothing 0
      store space 8 (L.ConstantOperand $ C.Int 64 $ toInteger i)
      return space

emitBlock :: [L.Operand] -> Block -> IRBuilder ()
emitBlock args = \case
  B xs term -> do
    mapM_ emitBedRock xs
    emitRet term
  If cond t f -> do
    finalizer <- freshName "end"
    trueName <- freshName "condTrue"
    falseName <- freshName "condFalse"
    undefined
--     condBr (operToLLVM cond) trueName falseName

emitBedRock :: Instruction -> IRBuilder ()
emitBedRock = \case
  Call space n args -> undefined
  Alloc space t -> undefined

-- emitBedRockVoid :: BedRockVoid -> IRBuilder ()
-- emitBedRockVoid = \case
--   Return o -> ret (operToLLVM o) >> return undefined
--   ReturnVoid -> retVoid >> return undefined
--   Assign addr val -> store (operToLLVM addr) 4 (operToLLVM val)

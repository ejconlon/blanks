module Test.Blanks.Eval where

-- import Blanks
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map
-- import Test.Blanks.Exp

-- type ExpEval = ExpScope (SplitVar Ident)
-- type Binder = BinderScope (NameOnly Ident) ExpEval

-- data EvalEnv = EvalEnv
--   { evalEnvBinders :: Map BinderId Binder
--   , evalEnvDefns :: Map Ident ExpEval
--   } deriving stock (Eq, Show)

-- data EvalError =
--     EvalErrorBinder
--   | EvalErrorBoundMissing !Int
--   | EvalErrorBinderMissing !BinderId
--   | EvalErrorDefnMissing !Ident
--   deriving stock (Eq, Show)

-- data Step a =
--     StepYes !a
--   | StepNo
--   | StepError !EvalError
--   deriving stock (Eq, Show, Functor, Foldable, Traversable)

-- data Kont =
--     KontTop
--   | KontIfGuard !ExpEval !ExpEval !Kont
--   deriving stock (Eq, Show)

-- newtype Ctx = Ctx { unCtx :: Map Int Closure }
--   deriving stock (Eq, Show)

-- emptyCtx :: Ctx
-- emptyCtx = Ctx Map.empty

-- data Closure = Closure
--   { closureExp :: !ExpEval
--   , closureCtx :: !Ctx
--   } deriving stock (Eq, Show)

-- data EvalTarget = EvalTarget
--   { evalTargetExp :: !ExpEval
--   , evalTargetCtx :: !Ctx
--   , evalTargetKont :: !Kont
--   } deriving stock (Eq, Show)

-- initEvalTarget :: ExpEval -> EvalTarget
-- initEvalTarget e = EvalTarget e emptyCtx KontTop

-- step :: EvalEnv -> EvalTarget -> Step EvalTarget
-- step env tgt =
--   case evalTargetExp tgt of
--     ScopeBinder _ _ _ -> StepError EvalErrorBinder
--     ScopeBound b ->
--       case Map.lookup b (unCtx (evalTargetCtx tgt)) of
--         Nothing -> StepError (EvalErrorBoundMissing b)
--         Just (Closure e c) -> StepYes (tgt { evalTargetExp = e, evalTargetCtx = c })
--     ScopeFree a ->
--       case a of
--         SplitVarBound bid ->
--           case Map.lookup bid (evalEnvBinders env) of
--             Nothing -> StepError (EvalErrorBinderMissing bid)
--             Just _binder -> error "TODO"
--         SplitVarFree dname ->
--           case Map.lookup dname (evalEnvDefns env) of
--             Nothing -> StepError (EvalErrorDefnMissing dname)
--             Just defn -> StepYes (tgt { evalTargetExp = defn, evalTargetCtx = emptyCtx })
--     ScopeEmbed fe ->
--       case fe of
--         ExpIf g t e -> StepYes (tgt { evalTargetExp = g, evalTargetKont = KontIfGuard t e (evalTargetKont tgt)})
--         -- TODO put in rest of cases
--         _ ->
--           case evalTargetKont tgt of
--             KontTop -> StepNo
--             _ -> error "TODO"

-- eval :: EvalEnv -> EvalTarget -> (Maybe EvalError, EvalTarget)
-- eval env = go where
--   go s =
--     case step env s of
--         StepYes s' -> go s'
--         StepNo -> (Nothing, s)
--         StepError e -> (Just e, s)

{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

import           Control.Monad
import           Control.Monad.Cont
import           Control.Monad.State
import           Data.Either
import           Data.List           hiding (lookup)
import           Data.Map
import           Prelude             hiding (lookup, seq)

type Opt a = ContT E (State Env) a

--------------------------------------------------------------------------------
-- Language Definition

-- data Prim = Plus
--           | Times
--           | Minud

data Prim = Not
          | Add1
          | Sub1
  deriving (Show, Eq)

data Const = N Int
           | Void
           | B Bool
  deriving (Show, Eq)

-- Variables are strings.
data E = C Const
       | Ref String
       | Primref Prim
       | If E E E
       | Seq E E
       | Assign String E
       | Lambda String E
       | Letrec [(String, E)] E
       | Call E E
  deriving (Show, Eq)

data Operand = Opnd E Env Loc
  deriving (Show)

instance (Eq Operand) where
  (Opnd e rho loc) == (Opnd e' rho' loc') =
    e == e' && loc == loc'

data Ctxt = Test
          | Effect
          | Value
          | App Operand Ctxt Loc
  deriving (Show, Eq)

data VarFlag = VRef
             | VAssign
  deriving (Show, Eq)

data CtxtFlag = Inlined
  deriving (Show, Eq)

data Var = Var String (Maybe Operand) [VarFlag] Loc
  deriving (Show, Eq)

type Env = Map Var Var

newtype OptimizerM a = OptimizerM { runOptimizer :: ContT E (State Store) a }
    deriving (Functor, Applicative, Monad, MonadState Store, MonadCont)

truthy :: Const -> Bool
truthy (B False) = False
truthy _         = True

--------------------------------------------------------------------------------
-- Store and store interactions

-------------------------------------------------
-- The original algorithm calls for three stores;
-- this is a headache in type land

data StoreEntry = SV [VarFlag]
                | SC [CtxtFlag]
                | SE (Either E Unvisited) -- Maybe E instead of Either E Unvisited?

data Unvisited = Unvisited

type Loc = Int

type Store = Map Loc StoreEntry

lookupStore :: Loc -> OptimizerM (Maybe StoreEntry)
lookupStore key = get >>= (\ sto -> return $ lookup key sto)

updateStore :: Loc -> StoreEntry -> OptimizerM ()
updateStore l entry = modify (alter usH l)
  where
    usH Nothing  = Just entry
    usH (Just x) =
      case (x, entry) of
        (SV vs, SV vs') -> Just $ SV $ nub $ vs ++ vs'
        (SC cs, SC cs') -> Just $ SC $ nub $ cs ++ cs'
        (SE (Right Unvisited), entry) -> Just entry
        _               -> error "Invalid store update"

--------------------------------------------------------------------------------
-- Inliner Helpers (Figure 4)

fold :: E -> Ctxt -> Env -> OptimizerM E
fold (Primref p) (App op ctxt loc) env = fmap result (visit op Value) >>= \case
    C c -> updateStore loc (SC [Inlined]) >> return (C $ doop p c)
    e1  -> return e1

-- For reference, I think the new code is equivalent to this in the new
-- OptimizerM
{-fold (Primref p) (App op ctxt loc) env = withContT k2 (visit op Value)-}
    {-where-}
        {-k2 k1 e1' = case result e1' of-}
                        {-C c -> do updateStore loc $ SC [Inlined]-}
                                  {-k1 $ C (doop p c)-}
                    {-_   -> k1 e1'-}

doop :: Prim -> Const -> Const
doop Not  (B b) = B (not b)
doop Add1 (N n) = N $ n + 1
doop Sub1 (N n) = N $ n - 1
doop p   c      = error $ "Invalid primop " ++ show p ++ " applied to constant " ++ show c

--------------------------------------------------------------------------------
-- Inliner Helpers (Figure 4)

visit :: Operand -> Ctxt -> OptimizerM E
visit (Opnd e rho loc) ctxt = lookupStore loc >>= \case

    Just (SE (Right _)) -> do
        e' <- inline e ctxt rho
        updateStore loc (SE (Left e'))
        return e'

    Just (SE (Left e')) -> return e'

-- Original implementation for reference. I think these are equivalent
{-visit :: Operand -> Ctxt -> OptimizerM E-}
{-visit (Opnd e rho loc) ctxt = do-}
    {-lookup <- lookupStore loc-}
    {-case lookup of-}
      {-Just (SE x) -> (flip . either) return x-}
                       {-(\_ -> withContT k1 (inline e ctxt rho loc))-}
                         {-where-}
                           {-k1 k e' = updateStore loc (SE (Left e')) >> k e'-}

seq :: E -> E -> E
seq (C Void) e2 = e2
seq e1 (Seq e3 e4) = (Seq (Seq e1 e3) e4)
seq e1 e2          = Seq e1 e2

result :: E -> E
result (Seq e1 e2) = e2
result e           = e

--------------------------------------------------------------------------------
-- Inliner

inline :: E -> Ctxt -> Env -> OptimizerM E

-- Constants
inline e@(C c) ctxt env = return $ case ctxt of
    Effect          -> C Void
    Test | truthy c -> C (B True)
    _               -> e

-- Seq (Rebuilds using the Seq constructor, but should use smart seq from the paper)
inline e@(Seq e1 e2) ctxt env = seq <$> inline e1 ctxt env <*> inline e2 ctxt env

inline e@(If e1 e2 e3) ctxt env = inline e1 Test env >>= \e1' -> case e1' of
   C (B True)  -> seq <$> pure e1' <*> inline e2 ctxt' env
   C (B False) -> seq <$> pure e1' <*> inline e3 ctxt' env
   -- TODO: Handle case when e2' and e3' are equal
   _           -> If  <$> pure e1' <*> inline e2 ctxt' env <*> inline e3 ctxt' env
   where
     ctxt' = case ctxt of { App{} -> Value ; _ -> ctxt }



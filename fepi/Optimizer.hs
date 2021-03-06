{-# LANGUAGE ConstraintKinds, FlexibleContexts #-}

import Control.Monad
import Control.Monad.State
import Control.Monad.Cont
import Data.Either
import Data.List hiding (lookup)
import Data.Map
import Prelude hiding (lookup)

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

--------------------------------------------------------------------------------
-- Store and store interactions

-------------------------------------------------
-- The original algorithm calls for three stores; 
-- this is a headache in type land 

data StoreEntry = SV [VarFlag]
                | SC [CtxtFlag]
                | SE (Either E Unvisited)

data Unvisited = Unvisited

type Loc = Int 

type Store = Map Loc StoreEntry

lookupStore :: Loc -> State Store (Maybe StoreEntry)
lookupStore key = get >>= (\ sto -> return $ lookup key sto)

updateStore :: Loc -> StoreEntry -> State Store ()
updateStore l entry = get >>= (\ sto -> put $ alter usH l sto)
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

fold :: E -> Ctxt -> Env -> ContT E (State Store) E
fold (Primref p) (App op ctxt loc) env =
  withContT k2 (visit op Value)
    where
      k2 k1 e1' = case result e1' of
                    C c -> do updateStore loc $ SC [Inlined]
                              k1 $ C (doop p c)
                    _   -> k1 e1'

doop :: Prim -> Const -> Const 
doop Not  (B b) = B (not b)
doop Add1 (N n) = N $ n + 1
doop Sub1 (N n) = N $ n - 1
doop p   c      = error $ "Invalid primop " ++ show p ++ " applied to constant " ++ show c

--------------------------------------------------------------------------------
-- Inliner Helpers (Figure 4)

visit :: Operand -> Ctxt -> ContT E (State Store) E
visit (Opnd e rho loc) ctxt = 
  do lookup <- lift $ lookupStore loc
     case lookup of
       Just (SE x) -> (flip . either) return x
                        (\_ -> withContT k1 (inline e ctxt rho loc))
                          where
                            k1 k e' = updateStore loc (SE (Left e')) >> k e'

seq :: E -> E -> E
seq (C Void) e2 = e2
seq e1 (Seq e3 e4) = (Seq (Seq e1 e3) e4)
seq e1 e2          = Seq e1 e2

result :: E -> E
result (Seq e1 e2) = e2
result e           = e

--------------------------------------------------------------------------------
-- Inliner

inline :: E -> Ctxt -> Env -> Loc -> ContT E (State Store) E
inline = undefined

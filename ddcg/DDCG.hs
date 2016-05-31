{-# LANGUAGE NamedFieldPuns #-}
import System.IO.Unsafe
import Data.IORef
import Data.List

type Var = String

type Program = [FunDecl]

data FunDecl = FunDecl 
  { name   :: Var
  , params :: [Var]
  , locals :: [Var]
  , body   :: Expr
  }


data Op = Plus | Times | Minus

data Expr
  = Seq        Expr   Expr
  | While      Expr   Expr
  | Break
  | Return     Expr
  | If         Expr   Expr Expr
  | IfL        Expr   Expr
  | Assign Var Expr
  | Binop Op   Expr   Expr
  | And        Expr   Expr
  | Or         Expr   Expr
  | Not        Expr
  | Call Var   [Expr]
  | VarV Var
  | IntV Int

data Register
  = EBP | R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10 | R11 | R12 | R13 | R14

instance (Show Register) where
  show EBP = "ebp"
  show R0  = "r0"
  show R1  = "r1"
  show R2  = "r2"
  show R3  = "r3"
  show R4  = "r4"
  show R5  = "r5"
  show R6  = "r6"
  show R7  = "r7"
  show R8  = "r8"
  show R9  = "r9"
  show R10 = "r10"
  show R11 = "r11"
  show R12 = "r12"
  show R13 = "r13"
  show R14 = "r14"

sp = EBP
ap = R2
fp = R3

registers = [R4,R5,R6,R7,R8,R9,R10,R11,R12,R13,R14]

data Location
  = Acc
  | Stack
  | Param Int
  | Local Int

instance (Show Location) where
  show = allocLoc

data Operand
  = OperLoc Location
  | OperImm Int

instance (Show Operand) where
  show (OperLoc l) = show l
  show (OperImm n) = "$" ++ show n

data Code
  = Jmp String
  | Ret
  | Nop
  | CallS String String
  | Mov String String
  | Push String
  | Pop String
  | Cmp String String
  | Jneq String
  | Lbl String

instance (Show Code) where
  show (Jmp s)     = "jmp " ++ s
  show (Ret)       = "ret"
  show (Nop)       = "nop"
  show (CallS n f) = "calls " ++ n ++ " " ++ f
  show (Mov s1 s2) = "mov " ++ s1 ++ " " ++ s2
  show (Push s)    = "push " ++ s
  show (Pop s)     = "pop " ++ s
  show (Cmp s1 s2) = "cmp " ++ s1 ++ " " ++ s2
  show (Jneq s)    = "jne " ++ s
  show (Lbl s)     = s ++ ":"

intSize = 8

spShow = "(" ++ show sp ++ ")"
apShow = "(" ++ show ap ++ ")"
fpShow = "(" ++ show fp ++ ")"

allocLoc :: Location -> String
allocLoc Acc       = "r0"
allocLoc Stack     = error "Shouldn't happen"
allocLoc (Param i) = "[" ++ show (intSize * i) ++ "+" ++ show ap ++ "]"
allocLoc (Local i) = "[" ++ show (-1 * intSize * i) ++ "+" ++ show fp ++ "]"

opLoc :: Operand -> String
opLoc (OperImm i) = show i
opLoc (OperLoc l) = case l of
  Acc       -> "r0"
  Stack     -> error "Shouldn't happen"
  (Param i) -> "[" ++ show (intSize * i) ++ "+" ++ show ap ++ "]"
  (Local i) -> "[" ++ show (-1 * intSize * i) ++ "+" ++ show fp ++ "]"

type Label = String
type Env = (Label, (String -> Location))

data DataDest = Effect | DestLoc Location 
data ControlDest = Jump Label | Branch Label Label

fromJump (Jump l) = l
fromJump (Branch l1 l2) = error "invalid jump"


effect = Effect
stack = DestLoc Stack
acc = DestLoc Acc
retLbl = Jump "return"

-- Effect + Jump    Statement
-- Loc + Jump       Expression
-- Effect + Branch  Test Expression
-- Loc + Branch     Test Expression

boolExpr :: Expr -> Bool
boolExpr (And _ _) = True
boolExpr (Or _ _)  = True
boolExpr (Not _)   = True

cg :: Expr -> Env -> DataDest -> ControlDest -> [Code]
cg (Seq e1 e2)   env ddest cdest   = let l = mkLabel "seq"
                                     in cg e1 env effect (Jump l) ++
                                        [Lbl l] ++
                                        cg e2 env ddest cdest
cg (If et ec ea) env ddest cdest   = let lthen = mkLabel "then"
                                         lelse = mkLabel "else"
                                     in cg et env effect (Branch lthen lelse) ++
                                        [Lbl lthen] ++
                                        cg ec env ddest cdest ++
                                        [Lbl lelse] ++
                                        cg ea env ddest cdest
cg (Call f es)   env ddest cdest   = let n = length es
                                     in (unravelArgs n env $ reverse es) ++ 
                                        [CallS (show n) f] ++  
                                        cgStore (OperLoc Acc) ddest cdest
cg (Return e)    env ddest cdest   = cg e env acc retLbl
cg (IntV n)      env ddest cdest   = cgStore (OperImm n) ddest cdest
cg (VarV x)    (_,m) ddest cdest   = cgStore (OperLoc $ m x) ddest cdest
cg (IfL et ec)   env ddest (Jump l) = let lthen = mkLabel "then"
                                      in cg et env effect (Branch lthen l) ++
                                         [Lbl lthen] ++
                                         cg ec env effect (Jump l)
cg (Break) (lbreak, m) ddest cdest  = [cgJump lbreak]
cg (While et eb) (lbreak, m) ddest (Jump l) =     
        let ltest = mkLabel "test"
            lbody = mkLabel "body"
        in [Lbl ltest] ++
           cg et (l, m) effect (Branch lbody l) ++
           [Lbl lbody] ++
           cg eb (l, m) effect (Jump ltest)
cg (Not e) env ddest (Branch ltrue lfalse) = 
        cg e env effect (Branch lfalse ltrue)
cg (And e1 e2) env ddest (Branch ltrue lfalse) = 
        let l = mkLabel "land"
        in cg e1 env effect (Branch ltrue l) ++
           [Lbl l] ++
           cg e2 env effect (Branch ltrue lfalse)
cg (Or e1 e2) env ddest (Branch ltrue lfalse) = 
        let l = mkLabel "lor"
        in cg e1 env effect (Branch l lfalse) ++
           [Lbl l] ++
           cg e2 env effect (Branch ltrue lfalse)

cgJump :: Label -> Code
cgJump "return" = Ret
cgJump l        = Jmp l

unravelArgs :: Int -> Env -> [Expr] -> [Code]
unravelArgs 0 env []     = []
unravelArgs n env (e:es) = let lbl = mkLabel $ "l" ++ (show n) ++  "_"
                           in cg e env stack (Jump lbl) ++
                              [Lbl lbl] ++
                              unravelArgs (n-1) env es

cgBranch :: (String -> Code) -> String -> (String -> Code) -> String -> [Code]
cgBranch jmp1 l1 jmp2 l2 =
  if l2 == "return" -- or l2 is lnext and l1 /= "return"
  then [jmp1 l1, cgJump l2]
  else [jmp2 l2, cgJump l1]


cgControl :: ControlDest -> [Code]
cgControl (Jump l)       = [cgJump l]
cgControl (Branch lt lf) = cgBranch Jneq lt Jmp lf

cgStore :: Operand -> DataDest -> ControlDest -> [Code]
cgStore op Effect (Jump l)                      = [cgJump l]
cgStore op Effect control                       = [Cmp "$0" $ show op] ++ cgControl control
cgStore (OperLoc Stack) (DestLoc Stack) control = cgControl control
cgStore (OperLoc Stack) (DestLoc a) control     = [Pop (allocLoc a)] ++ cgControl control
cgStore op (DestLoc Stack) control              = [Push (opLoc op)] ++ cgControl control
cgStore op (DestLoc a) control                  = [Mov (opLoc op) (allocLoc a)] ++ cgControl control


fngen :: FunDecl -> [Code]
fngen (FunDecl { name, params, locals, body }) = 
  cg body ("lend",setupEnv params locals) effect retLbl 

setupEnv :: [String] -> [String] -> String -> Location
setupEnv params locals x =
  case elemIndex x params of
  (Just n) -> Param n
  Nothing  -> case elemIndex x locals of
              (Just n) -> Local n
              Nothing  -> error $ "Didn't find: " ++ show x

emit :: [Code] -> String
emit [] = ""
emit (Lbl l:rest) = l ++ ":\n" ++ emit rest
emit (code:rest)  = "    " ++ show code ++ "\n" ++ emit rest

------------------------------------------------------------------
-- Tests

x = VarV "x"
y = VarV "y"

test0 = 
  FunDecl { name = "ortest"
          , params = ["x","y"]
          , locals = []
          , body = Return (IntV 5)
          }



test1 = 
  FunDecl { name = "ortest"
          , params = ["x","y"]
          , locals = []
          , body = If (Or x y)
                      (Return (IntV 5)) 
                      (If (And x y) 
                          (Return (IntV 10)) 
                          (Return (IntV 15)))
          }

test2 = 
  FunDecl { name = "ortest"
          , params = ["x","y"]
          , locals = []
          , body = If (Or x y)
                      (Return (IntV 5)) 
                      (If (And x y) 
                          (Return (IntV 10)) 
                          (Call "ortest" [IntV 0, VarV "y"]))
          }

------------------------------------------------------------------
-- Gensym Things

gensymKey :: IORef Int
gensymKey = unsafePerformIO $ newIORef 0

nextKey :: () -> IO Int
nextKey () = do 
  n <- readIORef gensymKey
  writeIORef gensymKey $ n + 1
  return n

mkLabel :: String -> Label
mkLabel s = 
  let next = show $ unsafePerformIO $ nextKey ()
  in s ++ next



type Binding = (Variable,Value)
type State = [Binding]
type Variable = String
type Op = String
type Target = Variable
type Source = Expression
type Test = Bexpression
type Body = Statement
type Thenbranch = Statement
type Elsebranch = Statement

data Value = Intval Integer deriving (Eq, Ord)
instance Show Value where
  show (Intval x) = show x
data Expression = Var Variable | Lit Value | Aop Op Expression Expression deriving (Eq,Ord,Show)
data Bexpression = Blit Bvalue | Bop Op Bexpression Bexpression | Rop Op Expression Expression deriving (Eq, Ord,Show)
data Bvalue = Boolval Bool deriving (Eq, Ord,Show)
data Statement = Skip | Assignment Target Source | Block Blocktype | Loop Test Body | Conditional Test Thenbranch Elsebranch deriving (Show)
data Blocktype = Nil | Nonnil Statement Blocktype deriving (Show)

get :: Variable -> State -> Value
get var ((nm,val):binds) = if var == nm then val else get var binds

onion :: Variable -> Value -> State -> State
onion var valExc = map (\(nm, valCurr) -> if nm == var then (nm, valExc) else (nm, valCurr))

eval:: Expression -> State -> Value
eval (Var v) binds = get v binds
eval (Lit v) binds = v
eval (Aop op e1 e2) binds = apply op (eval e1 binds) (eval e2 binds)

apply :: Op -> Value -> Value -> Value
apply "+" (Intval v1) (Intval v2) = Intval (v1 + v2)
apply "-" (Intval v1) (Intval v2) = Intval (v1 - v2)
apply "*" (Intval v1) (Intval v2) = Intval (v1 * v2)
apply "/" (Intval v1) (Intval v2) = Intval (round $ fromIntegral v1 / fromIntegral v2)

beval :: Bexpression -> State -> Bvalue
beval (Blit bv) _ = bv
beval (Bop op bv1 bv2) binds = bapply op (beval bv1 binds) (beval bv2 binds)
beval (Rop op v1 v2) binds = rapply op (eval v1 binds) (eval v2 binds)

bapply :: Op -> Bvalue -> Bvalue -> Bvalue
bapply "&&" (Boolval bval1) (Boolval bval2) = Boolval(bval1 && bval2)
bapply "||" (Boolval bval1) (Boolval bval2) = Boolval(bval1 || bval2)

rapply :: Op -> Value -> Value -> Bvalue
rapply op (Intval x) (Intval y) = Boolval (opFn op x y)
  where (opFn) "<" = (<)
        (opFn) ">" = (>)
        (opFn) "<=" = (<=)
        (opFn) ">=" = (>=)
        (opFn) "==" = (==)
        (opFn) "!=" = (/=)

m :: Statement -> State -> State
m Skip state = state
m (Assignment target source) state = onion target (eval source state) state
m (Loop t b) state = if beval t state == Boolval True then m (Loop t b) (m b state) else state
m (Conditional test thenbranch elsebranch) state = if beval test state == Boolval True then m thenbranch state else m elsebranch state
m (Block Nil) state = state
m (Block (Nonnil s b)) state = m (Block b) (m s state)

s1::State
s1=[("x", Intval 1) ,("y", Intval 5)]

p0::Statement -- An assignment
p0 = Assignment "x" (Aop "+" (Var "x") (Lit ( Intval 1)))

p1::Statement -- A loop. 
p1 = Loop (Rop "<" (Var "x") (Lit(Intval 10))) (Assignment "x" (Aop "+" (Var "x") (Lit(Intval 1))))

p2::Statement -- An IF-statement.
p2 = Conditional (Rop ">" (Var "x") (Var "y")) (Assignment "x" (Var "y")) (Assignment "x" (Aop "+" (Var "x") (Var "y")))

p3::Statement -- A Block (i.e. program) without instructions
p3=Block Nil

p4::Statement -- A Block (i.e. program) with instructions
p4=Block (Nonnil p2 (Nonnil p1 Nil))

run::Statement->State
run program = m program s1

--- Sample functions

fibState :: State
fibState = [("F_0",Intval 0),("F_1",Intval 1)]

fibNext :: State -> Value -> State
fibNext state (Intval n) = [newFib]
 where
   str = "F_"
   t0 = get (str ++ show (apply "-" (Intval n) (Intval 3))) state
   t1 = get (str ++ show (apply "-" (Intval n) (Intval 2))) state
   newFib_val = apply "+" t0 t1
   newFib = (str ++ show (apply "-" (Intval n) (Intval 1)), newFib_val)

getFibN :: State -> (State -> Value -> State) -> Value -> State
getFibN state fibNext (Intval 1) = [("F_0", get "F_0" state)] -- need to add el's from 3 up to n
getFibN state fibNext n = if rapply ">=" n (Intval 3) == Boolval True then getFibN state fibNext (apply "-" n (Intval 1)) ++ fibNext (getFibN state fibNext (apply "-" n (Intval 1))) n else state
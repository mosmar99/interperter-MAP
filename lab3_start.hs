type Binding = (Variable,Value)
type State = [Binding]
type Variable = String
data Value = Intval Integer deriving (Eq, Ord,Show)

-- task 1
get :: Variable -> State -> Value
get var ((nm,v):binds) = if var == nm then v else get var binds

onion :: Variable -> Value -> State -> State
onion var valExc = map (\(nm, valCurr) -> if nm == var then (nm, valExc) else (nm, valCurr))

-- test 1
{- PASSED
ghci> get "red" [("yellow",(Intval 5)),("red",(Intval 3))]
Intval 3
-}

data Expression = Var Variable | Lit Value | Aop Op Expression Expression -- Aritmetic operators 
    deriving (Eq,Ord,Show)

type Op = String

eval:: Expression -> State -> Value
eval (Var v) binds = get v binds
eval (Lit v) binds = v
eval (Aop op e1 e2) binds = apply op (eval e1 binds) (eval e2 binds) -- Missing apply op now

-- task 2

apply :: Op -> Value -> Value -> Value
apply "+" (Intval v1) (Intval v2) = Intval (v1 + v2)
apply "-" (Intval v1) (Intval v2) = Intval (v1 - v2)
apply "*" (Intval v1) (Intval v2) = Intval (v1 * v2)
apply "/" (Intval v1) (Intval v2) = Intval (round $ fromIntegral v1 / fromIntegral v2)

{- PASSED
ghci> s1=[("x",(Intval 1)) ,("y",(Intval 5))]  
ghci> eval (Var "x") s1 
Intval 1
ghci> eval (Aop "+" (Var "x") (Lit (Intval 4))) s1
Intval 5
ghci> eval (Aop "+" (Var "x") (Aop "*" (Lit (Intval 4)) (Var "y"))) s1
Intval 21
-}

data Bexpression = Blit Bvalue |
 Bop Op Bexpression Bexpression |   -- Boolean operators
 Rop Op Expression Expression       -- Relational operators 	
 deriving (Eq, Ord,Show)

data Bvalue = Boolval Bool deriving (Eq, Ord,Show)

-- task 3
beval :: Bexpression -> State -> Bvalue
beval (Blit bv) _ = bv
beval (Bop op bv1 bv2) binds = bapply op (beval bv1 binds) (beval bv2 binds)
beval (Rop op v1 v2) binds = rapply op (eval v1 binds) (eval v2 binds)

bapply :: Op -> Bvalue -> Bvalue -> Bvalue
bapply "&&" (Boolval bval1) (Boolval bval2) = Boolval(bval1 && bval2)
bapply "||" (Boolval bval1) (Boolval bval2) = Boolval(bval1 || bval2)

rapply :: Op -> Value -> Value -> Bvalue
rapply op (Intval x) (Intval y) = Boolval (compareWith op x y)
  where
    compareWith "<" = (<)
    compareWith ">" = (>)
    compareWith "<=" = (<=)
    compareWith ">=" = (>=)
    compareWith "==" = (==)
    compareWith "!=" = (/=)

{- PASSED
ghci> s1=[("x",(Intval 1)) ,("y",(Intval 5))]
ghci> beval (Bop "&&" (Blit (Boolval True)) (Rop "<" (Var "x") (Lit (Intval 3)))) s1
Boolval True
ghci> beval (Bop "&&" (Rop "<" (Var "y") (Lit (Intval 2))) (Rop "<" (Var "x") (Lit (Intval 3)))) s1
Boolval False
-}

-- task 4/5

data Statement = Skip |
 Assignment Target Source |
 Block Blocktype |
 Loop Test Body |
 Conditional Test Thenbranch Elsebranch
 deriving (Show)

type Target = Variable
type Source = Expression
type Test = Bexpression
type Body = Statement
type Thenbranch = Statement
type Elsebranch = Statement

data Blocktype = Nil |
 Nonnil Statement Blocktype
 deriving (Show)

m :: Statement -> State -> State
m Skip state = state
m (Assignment target source) state = onion target (eval source state) state 
m (Loop t b) state = if beval t state == Boolval True then m (Loop t b) (m b state) else state
m (Conditional test thenbranch elsebranch) state = if beval test state == Boolval True then m thenbranch state else m elsebranch state
m (Block Nil) = id
m (Block (Nonnil s b))  = m (Block b) . m s

{- PASSED
ghci> m p0 s1
[("x",Intval 2),("y",Intval 5)]
ghci> m p1 s1
[("x",Intval 10),("y",Intval 5)]
ghci> m p2 s1
[("x",Intval 6),("y",Intval 5)]
-}

s1::State
s1=[("x", Intval 1) ,("y", Intval 5)]

p0::Statement -- An assignment
p0 = (Assignment "x" (Aop "+" (Var "x") (Lit ( Intval 1))))

p1::Statement -- A loop. 
p1 = (Loop (Rop "<" (Var "x") (Lit(Intval 10))) (Assignment "x" (Aop "+" (Var "x") (Lit(Intval 1)))))

p2::Statement -- An IF-statement.
p2 = (Conditional (Rop ">" (Var "x") (Var "y")) (Assignment "x" (Var "y")) (Assignment "x" (Aop "+" (Var "x") (Var "y"))))

p3::Statement -- A Block (i.e. program) without instructions
p3=(Block Nil)

p4::Statement -- A Block (i.e. program) with instructions
p4=(Block (Nonnil p2 (Nonnil p1 Nil)))

run::Statement->State 
run program = m program s1
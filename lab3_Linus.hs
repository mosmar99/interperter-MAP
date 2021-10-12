type Binding = (Variable,Value)
type State = [Binding]
type Variable = String
data Value = Intval Integer deriving (Eq,Ord,Show)

get :: Variable -> State -> Value   --tested
get var (x:xs) = if var == fst x then snd x else get var xs

onion :: Variable -> Value -> State -> State    --tested
onion var val st = (var,val) : [x | x <- st, var /= fst x]

data Expression = Var Variable | Lit Value | Aop Op Expression Expression deriving (Eq,Ord,Show) -- Aritmetic operators

type Op = String

eval:: Expression -> State -> Value --tested
eval (Var v) state = get v state
eval (Lit v) _ = v
eval (Aop op e1 e2) state = apply op (eval e1 state) (eval e2 state)

apply :: Op -> Value -> Value -> Value  --tested
apply op (Intval x) (Intval y)
    | op == "+" = Intval $ x + y
    | op == "-" = Intval $ x - y
    | op == "*" = Intval $ x * y
    | op == "/" = Intval $ round $ fromIntegral x / fromIntegral y

data Bexpression = Blit Bvalue | Bop Op Bexpression Bexpression | Rop Op Expression Expression deriving (Eq,Ord,Show)
                                        -- Boolean operators               -- Relational operators
data Bvalue = Boolval Bool deriving (Eq,Ord,Show)

beval :: Bexpression -> State -> Bvalue --tested
beval (Blit b) _ = b
beval (Bop op b1 b2) st = bapply op (beval b1 st) (beval b2 st)
beval (Rop op e1 e2) st = rapply op (eval e1 st) (eval e2 st)

bapply :: Op -> Bvalue -> Bvalue -> Bvalue  --tested
bapply op (Boolval x) (Boolval y)
    | op == "&&" && x == True && y == True = Boolval True 
    | op == "||" && (x == True || y == True) = Boolval True
    | otherwise = Boolval False 

rapply :: Op -> Value -> Value -> Bvalue    --tested
rapply op (Intval x) (Intval y)
    | op == "<" && x < y = Boolval True 
    | op == "<=" && x <= y = Boolval True
    | op == "==" && x == y = Boolval True 
    | op == "!=" && x /= y = Boolval True 
    | op == ">=" && x >= y = Boolval True 
    | op == ">" && x > y = Boolval True
    | otherwise = Boolval False

data Statement = Skip | Assignment Target Source | Block Blocktype | Loop Test Body | Conditional Test Thenbranch Elsebranch deriving Show

type Target = Variable
type Source = Expression
type Test = Bexpression
type Body = Statement
type Thenbranch = Statement
type Elsebranch = Statement

m :: Statement -> State -> State
m (Skip) state = state
m (Assignment target source) state = onion target (eval source state) state
m (Loop test body) state    --fix
    | beval test state == Boolval True = 
    | otherwise = state --i.e. skip
m (Conditional test thenbranch elsebranch) state
    | beval test state == Boolval True = m thenbranch state
    | otherwise = m elsebranch state

data Blocktype = Nil | Nonnil Statement Blocktype deriving (Show)

s1 :: State
s1=[("x",(Intval 1)),("y",(Intval 5))]

p0 :: Statement -- An assignment
p0 = (Assignment "x" (Aop "+" (Var "x") (Lit (Intval 1))))

p1 :: Statement -- A loop
p1 = (Loop (Rop "<" (Var "x") (Lit(Intval 10))) (Assignment "x" (Aop "+" (Var "x") (Lit(Intval 1)))))

p2 :: Statement -- An IF-statement
p2 = (Conditional (Rop ">" (Var "x") (Var "y")) (Assignment "x" (Var "y")) (Assignment "x" (Aop "+" (Var "x") (Var "y"))))

--p3 :: Statement -- A Block (i.e. program) without instructions
--p3 = (Block Nil)

--p4 :: Statement -- A Block (i.e. program) with instructions
--p4 = (Block (Nonnil p2 (Nonnil p1 Nil)))

--run :: Statement->State 
--run program = m program s1
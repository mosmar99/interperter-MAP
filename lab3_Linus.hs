data Value = Intval Integer deriving (Eq,Ord,Show)
data Expression = Var Variable | Lit Value | Aop Op Expression Expression deriving (Eq,Ord,Show)
data Bvalue = Boolval Bool deriving (Eq,Ord,Show)
data Statement = Skip | Assignment Target Source | Block Blocktype | Loop Test Body | Conditional Test Thenbranch Elsebranch deriving Show
data Bexpression = Blit Bvalue | Bop Op Bexpression Bexpression | Rop Op Expression Expression deriving (Eq,Ord,Show)
data Blocktype = Nil | Nonnil Statement Blocktype deriving (Show)
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

get :: Variable -> State -> Value
get var (x:xs) = if var == fst x then snd x else get var xs

onion :: Variable -> Value -> State -> State
onion var val st = (var,val) : [x | x <- st, var /= fst x]

eval:: Expression -> State -> Value
eval (Var v) state = get v state
eval (Lit v) _ = v
eval (Aop op e1 e2) state = apply op (eval e1 state) (eval e2 state)

apply :: Op -> Value -> Value -> Value 
apply "+" (Intval x) (Intval y) = Intval $ x + y
apply "-" (Intval x) (Intval y) = Intval $ x - y
apply "*" (Intval x) (Intval y) = Intval $ x * y
apply "/" (Intval x) (Intval y) = Intval $ round $ fromIntegral x / fromIntegral y

beval :: Bexpression -> State -> Bvalue
beval (Blit b) _ = b
beval (Bop op b1 b2) st = bapply op (beval b1 st) (beval b2 st)
beval (Rop op e1 e2) st = rapply op (eval e1 st) (eval e2 st)

bapply :: Op -> Bvalue -> Bvalue -> Bvalue
bapply "&&" (Boolval x) (Boolval y) = Boolval $ x == True && y == True
bapply "||" (Boolval x) (Boolval y) = Boolval $ x == True || y == True

rapply :: Op -> Value -> Value -> Bvalue
rapply "<" (Intval x) (Intval y) = Boolval $ x < y
rapply "<=" (Intval x) (Intval y) = Boolval $ x <= y
rapply "==" (Intval x) (Intval y) = Boolval $ x == y
rapply "!=" (Intval x) (Intval y) = Boolval $ x /= y
rapply ">=" (Intval x) (Intval y) = Boolval $ x >= y
rapply ">" (Intval x) (Intval y) = Boolval $ x > y

m :: Statement -> State -> State
m (Skip) state = state
m (Assignment target source) state = onion target (eval source state) state
m (Loop test body) state = if beval test state == Boolval True then m (Loop test body) (m body state) else state
m (Conditional test thenbranch elsebranch) state = if beval test state == Boolval True then m thenbranch state else m elsebranch state
m (Block Nil) state = state
m (Block (Nonnil statement nextBlock)) state = m (Block nextBlock) (m statement state)

s1 :: State
s1=[("x",(Intval 1)),("y",(Intval 5))]

p0 :: Statement -- An assignment
p0 = (Assignment "x" (Aop "+" (Var "x") (Lit (Intval 1))))

p1 :: Statement -- A loop
p1 = (Loop (Rop "<" (Var "x") (Lit(Intval 10))) (Assignment "x" (Aop "+" (Var "x") (Lit(Intval 1)))))

p2 :: Statement -- An IF-statement
p2 = (Conditional (Rop ">" (Var "x") (Var "y")) (Assignment "x" (Var "y")) (Assignment "x" (Aop "+" (Var "x") (Var "y"))))

p3 :: Statement -- A Block (i.e. program) without instructions
p3 = Block Nil

p4 :: Statement -- A Block (i.e. program) with instructions
p4 = Block (Nonnil p2 (Nonnil p1 Nil))

pTest :: Statement
pTest = Block (Nonnil p1 (Nonnil p0 (Nonnil p0 Nil)))

run :: Statement -> State 
run program = m program s1

-- Example: generate infinite list of Fibonacci numbers
runFib :: State
runFib = m fib sFib

sFib :: State
sFib = [("counter",(Intval 3)),("Intval 2",(Intval 1)),("Intval 1",(Intval 1))]

fib :: Statement
fib = Loop (Blit (Boolval True)) fibBlock

fibBlock :: Statement
fibBlock = Block $ Nonnil calcNewVal $ Nonnil counterIncrease $ Nil

calcNewVal :: Statement
calcNewVal = Assignment newVarName $ Aop "+" v1 v2  --create new Var with the value v1 + v2
    where newVarName = show $ get "counter" sFib    --the value of "counter" is used for the name of the new fibonacci number
          v1 = Lit $ snd $ sFib !! 2    --2nd to last value
          v2 = Lit $ snd $ sFib !! 1    --last value
                                        --sFib !! 0 is counter
counterIncrease :: Statement
counterIncrease = Assignment "counter" $ Aop "+" (Var "counter") $ Lit $ Intval 1   --increase "counter" with 1

{-  Pseudo-code
while(True){
    v1 = 2nd to last number
    v2 = last number
    newNumber = v1 + v2
    newVarName = <value of "counter">
    (newVarName,newValue) : <list of fibonacci numbers>
    counter = counter + 1
}
-}

{-  output example
Prelude> sFib
    [("counter",Intval 3),("Intval 2",Intval 1),("Intval 1",Intval 1)]
Prelude> sFib2 = m fibBlock sFib
Prelude> sFib3 = m fibBlock sFib2
Prelude> sFib4 = m fibBlock sFib3

Prelude>
Prelude>
Prelude> sFib
    [("counter",Intval 3),("Intval 2",Intval 1),("Intval 1",Intval 1)]  --fine
Prelude> sFib2
    [("counter",Intval 4),("Intval 3",Intval 2),("Intval 2",Intval 1),("Intval 1",Intval 1)]    --fine
Prelude> sFib3
    [("counter",Intval 5),("Intval 3",Intval 2),("Intval 2",Intval 1),("Intval 1",Intval 1)]    --("Intval 4",Intval 3) is missing
Prelude> sFib4
    [("counter",Intval 6),("Intval 3",Intval 2),("Intval 2",Intval 1),("Intval 1",Intval 1)]    --("Intval 4",Intval 3) and ("Intval 5",Intval 5) are missing
-}
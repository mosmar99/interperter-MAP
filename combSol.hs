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

----------- Sample functions

-- Example: Fibonacci numbers
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

-- Example: (approximately) calculate diameter, circumference, surface area and volume of a sphere given a radius r
runSphere :: State
runSphere = m (Block sphere) sSphere

sSphere :: State
sSphere = [("Radius",(Intval 120))]

sphere :: Blocktype
sphere = Nonnil dia $ Nonnil circ $ Nonnil area $ Nonnil vol Nil

dia :: Statement    --2*r
dia = Assignment "Diameter" (Aop "*" (Lit (Intval 2)) (Var "Radius"))

circ :: Statement   --2*pi*r or dia*pi
circ = Assignment "Circumference" $ Aop "*" (Var "Diameter") (Lit (Intval 3))

area :: Statement   --4*pi*r^2 or 2*2*pi*r*r or circ*dia
area = Assignment "Area" $ Aop "*" (Var "Circumference") (Var "Diameter")

vol :: Statement    --(4/3)*pi*r^3 or area*r/3
vol = Assignment "Volume" $ Aop "/" (Aop "*" (Var "Area") (Var "Radius")) (Lit (Intval 3))

-- Example: get largest pythagoran triple sum (circumference) with integer sides < N
runPyth :: State
runPyth = m firstLoop sPyth

sPyth :: State
sPyth = [("a",(Intval 1)),("b",(Intval 1)),("c",(Intval 1)),("maxA",(Intval 0)),("maxB",(Intval 0)),("maxC",(Intval 0)),("maxSum",(Intval 0))]

firstLoop :: Statement
firstLoop = Loop (Rop "<" (Var "c") (Lit (Intval 20))) (Block secondBody)  --Intval N limits each of the three sides to a max length of (N-1)
            --the outmost loop: while(c < N)                do this
secondBody :: Blocktype
secondBody = Nonnil secondLoop $ Nonnil (Assignment "b" (Lit (Intval 1))) $ Nonnil (Assignment "c" $ Aop "+" (Var "c") $ Lit $ Intval 1) Nil
            --do middle loop            then reset "b" for further checking                 then increase "c" by 1
secondLoop :: Statement
secondLoop = Loop (Rop "<" (Var "b") (Var "c")) (Block thirdBody)
            --the middle loop: while(b < c)         do this
thirdBody :: Blocktype
thirdBody = Nonnil thirdLoop $ Nonnil (Assignment "a" (Lit (Intval 1))) $ Nonnil (Assignment "b" $ Aop "+" (Var "b") $ Lit $ Intval 1) Nil
            --do the inner most loop    then reset "a" for further checking                 then increase "b" by 1
thirdLoop :: Statement
thirdLoop = Loop (Rop "<" (Var "a") (Var "b")) (Block innerBody)
            --inner most loop: while(a < b)     do this
innerBody :: Blocktype
innerBody = Nonnil ifStatement $ Nonnil (Assignment "a" $ Aop "+" (Var "a") $ Lit $ Intval 1) Nil
            --do if-statement                   then increase "a" by 1
ifStatement :: Statement
ifStatement = Conditional (test) (thenBranch) Skip
                        --test is true when a new identity with a larger sum is found
test :: Bexpression
test = Bop "&&" (leftTest) (rightTest)

leftTest :: Bexpression
leftTest = Rop "==" (Aop "+" (Aop "*" (Var "a") (Var "a")) (Aop "*" (Var "b") (Var "b"))) (Aop "*" (Var "c") (Var "c"))
                        --check pythagoran identity (a*a + b*b == c*c)
rightTest :: Bexpression
rightTest = Rop ">" (Aop "+" (Aop "+" (Var "a") (Var "b")) (Var "c")) (Var "maxSum")
                        --check if a + b + c > maxSum
thenBranch :: Statement
thenBranch = Block $ Nonnil (Assignment "maxA" (Var "a")) $ Nonnil (Assignment "maxB" (Var "b")) $ Nonnil (Assignment "maxC" (Var "c")) $ 
                     Nonnil (Assignment "maxSum" ((Aop "+" (Aop "+" (Var "a") (Var "b")) (Var "c")))) Nil
                            --update maxA, maxB and maxC to their respective a,b,c vales and update maxSum = a + b + c

{-  Pseudo code
a = 1
b = 1
c = 1
maxSum = 3
maxA = 1
maxB = 1
maxC = 1
while(c < 10){
    while(b < c){
        while(a < b){
            if(a*a + b*b == c*c && a + b + c > maxSum){
                maxA = a
                maxB = b
                maxC = c
                maxSum = a + b + c
            }
            a++
        }
        a = 1
        b++
    }
    b = 1
    c++
}
-}

-- Exampel: factorial
runFact :: State
runFact = m factIf sFact

sFact :: State
sFact = [("counter",(Intval 1)),("Input",(Intval 11)),("Output",(Intval 1))]

factIf :: Statement
factIf = Conditional (Rop ">" (Var "Input") (Lit (Intval 0))) (factLoop) Skip
                        --filter illegal arguments (<0)
factLoop :: Statement
factLoop = Loop (Rop "<=" (Var "counter") (Var "Input")) (Block factInnerBody)
                        --while(counter <= Input)           do this
factInnerBody :: Blocktype
factInnerBody = Nonnil (Assignment "Output" (Aop "*" (Var "Output") (Var "counter"))) $ Nonnil (Assignment "counter" (Aop "+" (Var "counter") (Lit (Intval 1)))) Nil
                                    --Output = Output * counter                                     then increase "counter" by 1

{-  Pseudo code
input = 7
output = 1
counter = 1
if(input > 0){
    while(counter <= input){
        output *= counter
        counter++
    }
}
-}
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

{-  run examples
Prelude> runSphere      -- radius = 7
    [("Volume",Intval 1372),("Area",Intval 588),("Circumference",Intval 42),("Diameter",Intval 14),("Radius",Intval 7)]
Prelude> runSphere      -- radius = 120
    [("Volume",Intval 6912000),("Area",Intval 172800),("Circumference",Intval 720),("Diameter",Intval 240),("Radius",Intval 120)]
-}

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
{-  run examples
Prelude> runPyth    -- N = 100
    [("c",Intval 100),("b",Intval 1),("a",Intval 1),("maxSum",Intval 234),("maxC",Intval 97),("maxB",Intval 72),("maxA",Intval 65)]
Prelude> runPyth    -- N = 20
    [("c",Intval 20),("b",Intval 1),("a",Intval 1),("maxSum",Intval 40),("maxC",Intval 17),("maxB",Intval 15),("maxA",Intval 8)]
-}

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

{-  run examples
Prelude> runFact        -- 5!
    [("counter",Intval 6),("Output",Intval 120),("Input",Intval 5)]
Prelude> runFact        -- 11!
    [("counter",Intval 12),("Output",Intval 39916800),("Input",Intval 11)]
-}

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
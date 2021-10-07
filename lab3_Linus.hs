type Binding = (Variable,Value)
type State = [Binding]
type Variable = String
data Value = Intval Integer deriving (Eq, Ord,Show)

--Task 1
get :: Variable -> State -> Value   --tested
get var st = head [snd x | x <- st, var == fst x]

onion :: Variable -> Value -> State -> State    --tested
onion var val st = (var,val) : [x | x <- st, var /= fst x]

data Expression = Var Variable |
 Lit Value |
 Aop Op Expression Expression      -- Aritmetic operators
 deriving (Eq, Ord, Show)

type Op = String

eval:: Expression -> State -> Value
--eval (Var v) state = get v state
eval (Lit v) state = v
--eval (Aop op e1 e2) state = apply op (eval e1 state) (eval e2 state) -- Missing apply op now

data Bexpression = Blit Bvalue |
 Bop Op Bexpression Bexpression |   -- Boolean operators
 Rop Op Expression Expression       -- Relational operators 	
 deriving (Eq, Ord,Show) 

data Bvalue = Boolval Bool deriving (Eq, Ord,Show)  

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


s1::State
s1=[("x",(Intval 1)) ,("y",(Intval 5))]

--p0::Statement -- An assignment
--p0 = (Assignment "x" (Aop "+" (Var "x") (Lit ( Intval 1))))

--p1::Statement -- A loop. 
--p1=(Loop (Rop "<" (Var "x") (Lit(Intval 10))) (Assignment "x" (Aop "+" (Var "x") (Lit(Intval 1)))))

--p2::Statement -- An IF-statement.
--p2=(Conditional (Rop ">" (Var "x") (Var "y")) (Assignment (Var "x") (Var "y")) (Assignment "x" (Aop "+" (Var "x") (Var "y"))))

--p3::Statement -- A Block (i.e. program) without instructions
--p3=(Block Nil)

--p4::Statement -- A Block (i.e. program) with instructions
--p4=(Block (Nonnil p2 (Nonnil p1 Nil)))

--run::Statement->State 
--run program = m program s1
lucky 7 = "lucky Number 7"
lucky x = " You are Not lucky"

factorial 0 = 1
factorial n = n * factorial(n-1)

myHead[] = error "Empty List"
myHead (x:_) = x

myTail[] = error "Empty List"
myTail (_:xs) = xs

myLast[x] = x
myLast(_:xs) = myLast xs

swap[] = []
swap[x] = [x]
swap(x:y:xs) = 
    if(x>y) then y: (swap(x:xs))
    else x: (swap(y:xs))

bubbleSort xs = if(xs == swap(xs)) then xs
                else bubbleSort(swap xs)

split xs = let n = length xs
               half = div n 2
           in (take half xs, drop half xs)

merge xs [] = xs
merge [] ys = ys
merge (x:xs)(y:ys) = if(x<y) 
                        then x: (merge xs(y:ys))
                        else y: (merge(x:xs)ys)

mergeSort[] = []
mergeSort[x] = [x]
mergeSort xs = let(front, back) = split xs 
   in merge(mergeSort front) (mergeSort back)

--Test
-- (1,True) :: Num a => (a, Bool)
-- 1:2:[] :: Num a => [a]
-- map (\_ -> True) [1,2,3] :: [Bool]
-- \f -> f True :: (Bool -> a) -> a
-- \x -> x : [] :: a -> [a]
-- (d -> b) -> (c -> d) -> c -> b
-- (t1 -> t2) -> (t3 -> t1) -> t3 -> t2x
-- [Bool]

-- (Bool -> a) -> a
-- a -> [a]



-- (t4 -> t2) -> (t5 -> t4) -> t5 -> t2



-- foldl (*) [1,2,3,4,5] 1

-- 1 * 1 => 1

-- foldr (*) [2,3,4,5] 1

-- 2 * 1 => 2

-- foldr (*) [1,2,3] 20


-- (1,2)
-- show (Pair e1 e2) = "(" ++ (show e1) ++ "," ++ (show e2) ++ ")"

data Tree a = Node a [Tree a]
   deriving Show
t1 = Node 1 [Node 2 [Node 5 [Node 6 []]], Node 3 [], Node 2 [Node 4[]]]


-- count (Node x []) = [x]
-- count (Node x xs) = [x] ++ count' xs

-- count' [] = []
-- count' (x:xs) = count x ++ count' xs 

sndlayer (Node _ xs) = sndlayer' xs

sndlayer' [] = []
sndlayer' (x:xs) = sndlayer'' x ++ sndlayer' xsn

sndlayer'' (Node x _) = [x]

t2 = Node 1 [Node 2 [Node 5 [], Node 6 []], Node 3 []]

count (Node _ xs) = 1 + sum (map count xs)

depth (Node _ []) = 1
depth (Node _ xs) = 1 + maximum (map depth xs)

flatten xs = foldr (++) [] xs

list2numbers xs = foldl (\x y -> ((x*10) + y)) 0 xs

l2n (x:xs) = 



-- count (Node _ xs) = 1 + count' xs

-- count' [] = 0
-- count' (x:xs) = count x ++ count' xs



-- thdlayer (Node _ xs) = thdlayer' xs

-- thdlayer' [] = []
-- thdlayer' (Node _ xs) =  thdlayer'' xs

-- thdlayer'' [] = []
-- thdlayer'' (x:_) =  [x]

-- thdlayern (Node x _) = [x]


-- sndLayer (Node _ xs) = map (\(Node a ys) -> a) 


data Exp =  I Integer
         | B Bool
         | D Double
         | Var String 
         | Equals Exp Exp
         | If Exp Exp Exp   -- ifthenelse
         | Sum Exp Exp
         | Pair Exp Exp
instance Show Exp where
    show (I i)   = show i
    show (Var v) = v
    show (B b)   = show b
    show (Equals e1 e2) = show e1 ++ "==" ++ show e2
    show (If e1 e2 e3)  = "if (" ++ show e1 ++ ") then " ++ show e2 ++ " else " ++ show e3
    show (Pair e1 e2)= "("++show e1++","++show e2++")"

eval :: [(a,b)]-> Exp -> Integer
eval env (I i) = i
eval env (Sum e1 e2) = eval env e1 + eval env e2
eval env (Var s) = case lookup s env of
    Just n  -> n
    Nothing -> error "Variable not found"
eval env (Pair e1 e2)= (eval env e1,eval env e2)


-- e7 = If (Equals (I 2) (I 3)) (I 3) (I 1)
e1 = Sum (I 2) (Var "x")
e2  = Pair (I 5) (Var "y")
-- e2 = Sum (I 3) (I 4)


-- let x = 4, y = 5, z = 6 in
--     x + y + z
-- x + y + z



-- 4+2 = 6


data Exp =  I Integer
          | B Bool
          | Var String   
          | Sum Exp Exp
          | Mult Exp Exp -- Multiplication
          | Let String Exp Exp -- Local def
          | If Exp Exp Exp   -- ifthenelse
          | And Exp Exp
          | Or Exp Exp
          | Not Exp
          | Equals Exp Exp
          | Lambda String Exp
          | App Exp Exp
          | Pair Exp Exp

data Value = Number Integer
           | Boolean Bool
           | Func (Exp, [(String, Value)])
           | VPair Value Value
             -- closure

e1 = Mult (Sum (Var "x") (I 1)) (I 2)
e2 = Let "x" (I 2) (Sum (Var "x") (I 3))
e3 = If (Equals (Var "x") (I 0))
        (I 1)
        (Sum (Var "x") (I 2))
e4 = Lambda "x" (Sum (Var "x") (I 1))
e5 = App e4 (I 7)
e6 = Sum (I 2) (I 3)
e7 = If (Equals (I 2) (I 3)) (Var "x") (Var "y")

instance Show Exp where
   show (I i) = show i
   show (B b) = show b
   show (Var x) = x
   show (Sum e1 e2) =
      (show e1) ++ " + " ++ (show e2)
   show (Mult e1 e2) =
      (show e1) ++ " * " ++ (show e2)
   show (Let x e2 e3) =
      "let " ++ x ++ " = " ++ (show e2) ++
      " in " ++ (show e3)
   show (If e1 e2 e3) =
      "if " ++ (show e1) ++
      " then " ++ (show e2) ++
      " else " ++ (show e3)
   show (And e1 e2) =
      (show e1) ++ " && " ++ (show e2)
   show (Or e1 e2) =
      (show e1) ++ " || " ++ (show e2)
   show (Not e1) =
      "!" ++ (show e1)
   show (Equals e1 e2) =
      (show e1) ++ " == " ++ (show e2)
   show (Lambda x e) = "lambda " ++ x ++ "->" ++ (show e)
   show (App e1 e2) = "(" ++ (show e1) ++ ") (" ++ (show e2) ++ ")"
   show (Pair e1 e2) = "(" ++ (show e1) ++ "," ++ (show e2) ++ ")"

add :: Value -> Value -> Value
add (Number x) (Number y) = Number (x+y)
add _ _ = error "not two integers"

mult :: Value -> Value -> Value
mult (Number x) (Number y) = Number (x*y)
mult _ _ = error "not two integers"

logAnd :: Value -> Value -> Value
logAnd (Boolean x) (Boolean y) = Boolean (x && y)
logAnd _ _ = error "not two bools"

eval env (I n) = Number n
eval env (B b) = Boolean b
eval env (Var x) = case (lookup x env) of
    Just n  -> n
    Nothing -> error "var not found"
eval env (Sum e1 e2) = add (eval env e1) (eval env e2)
eval env (Mult e1 e2) = mult (eval env e1) (eval env e2)
eval env (And e1 e2) = logAnd (eval env e1) (eval env e2)
eval env (Let x e1 e2) =
   eval ((x, eval env e1):env) e2
eval env (Lambda x e) =
   Func (Lambda x e, env)  -- closure
eval env (App e1 e2) =
  let value = eval env e2
      (Func (Lambda x body, env')) = eval env e1
  in eval ((x,value):env') body
eval env (Pair e1 e2) = VPair (eval env e1) (eval env e1)
data Exp =      I Integer
            |   Sum Exp Exp

instance Show Exp where
    show (I v) = show v



e1 = (I 2)
e2 = (I 8)

add (I n) (I m) = I (n+m)

eval env(I v) = I v
eval env Sum e1 e2 = add (eval env e1) (eval env e2)


data Tree a = EMP 
   | Node(Tree a) a (Tree a)
   deriving Show 

t4 = Node EMP 9 EMP
t5 = Node EMP 6 EMP
t2 = Node t4 1 t5
t3 = Node EMP 2 EMP
t1 = Node t2 3 t3

depth EMP = 0 
depth (Node l _ r) = 1+ max (depth l) (depth r)


weight EMP = 0
weight (Node l v r) = v + (weight l) + (weight r) 


sizze EMP = 0
sizze (Node l _ r) = 1+ (sizze l) + (sizze r)

inOrder EMP = []
inOrder (Node l v r) = inOrder(l) ++ [v] ++ inOrder (r)

postOrder EMP = []
postOrder (Node l v r) = postOrder(l) ++ postOrder(r) ++ [v]

perOrder EMP = []
perOrder (Node l v r) = [v] ++ perOrder(l) ++ perOrder(r)

scndlayer EMP = 0
scndlayer(Node l _ r) = 1
scndlayer''(Node l _ r) = scndlayer(l) + scndlayer(r)


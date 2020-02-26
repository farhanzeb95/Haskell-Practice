data Tree a = EMP 
   | Node a [Tree a]
   deriving Show 


t5 = Node 4 [EMP]
t4 = Node 2 [t5]
t2 = Node 2 [EMP]
t3 = Node 3 [EMP]
t1 = Node 1 [t2,t3,t4]

sndLayer(Node _ xs) = map(\(Node x _) -> x) xs

count EMP = 0
count(Node _ xs) = 1 + sum(map count xs)

allLayer EMP = []
allLayer(Node _ xs) = map(\(Node x _) -> x ) xs 

mySize(Node x []) = 1
mySize(Node x xs) = 1 + mySize' xs

mySize'[] = 0
mySize' (x:xs) = mySize x + mySize' xs

sndlayer (Node _ xs) = sndlayer' xs

sndlayer' [] = []
sndlayer' (x:xs) = sndlayer'' x ++ sndlayer' xs

sndlayer'' (Node x _) = [x]
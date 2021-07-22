module Initial ( main ) where

-- Initial
data Exp = Lit Int
         | Neg Exp
         | Add Exp Exp

eval :: Exp -> Int
eval (Lit n) = n
eval (Neg e) = - eval e
eval (Add e1 e2) = eval e1 + eval e2

view :: Exp -> String
view (Lit n) = show n
view (Neg e) = "-" ++ view e
view (Add e1 e2) = "(" ++ view e1 ++ " + " ++ view e2 ++ ")"

ti1 = Add (Lit 8) (Neg (Add (Lit 1) (Lit 2)))

main :: IO ()
main = putStrLn ((view ti1) ++ " = " ++ show (eval ti1))











-- Final
type Repr = Int

lit :: Int -> Repr
lit n = n

neg :: Repr -> Repr
neg e = -e

add :: Repr -> Repr -> Int
add e1 e2 = e1 + e2

tf1 = add (lit 8) (neg (add (lit 1) (lit 2))) -- = 5

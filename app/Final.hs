{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Final (main) where
import Control.Monad


class ExpSYM repr where
  lit :: Int -> repr
  neg :: repr -> repr
  add :: repr -> repr -> repr

tf1 = add (lit 8) (neg (add (lit 1) (lit 2))) -- ExpSYM repr => repr


instance ExpSYM Int where
  lit n = n
  neg e = -e
  add e1 e2 = e1 + e2

instance ExpSYM String where
  lit n = show n
  neg e = "-" ++ e
  add e1 e2 = "(" ++ e1 ++ " + " ++ e2 ++ ")"


eval :: Int -> Int
eval = id

view :: String -> String
view = id

--main :: IO ()
--main = putStrLn (view tf1 ++ " = " ++ show (eval tf1))




class MulSYM repr where
  mul :: repr -> repr -> repr

instance MulSYM Int where
  mul e1 e2 = e1 * e2

instance MulSYM String where
  mul e1 e2 = "(" ++ e1 ++ " * " ++ e2 ++ ")"

tfm1 = add (lit 7) (neg (mul (lit 1) (lit 2)))
tfm2 = mul (lit 7) tfm1

--main :: IO ()
--main = do
--  putStrLn (view tfm1 ++ " = " ++ show (eval tfm1))
--  putStrLn (view tfm2 ++ " = " ++ show (eval tfm2))



data Tree = Leaf String
   | Node String [Tree]
     deriving (Eq, Read, Show)

instance ExpSYM Tree where
  lit n = Node "Lit" [Leaf $ show n]
  neg e = Node "Neg" [e]
  add e1 e2 = Node "Add" [e1, e2]

instance MulSYM Tree where
  mul e1 e2 = Node "Mul" [e1, e2]

type ErrMsg = String

safeRead :: Read a => String -> Either ErrMsg a
safeRead s = case reads s of
  [(x, "")] -> Right x
  _ -> Left $ "Read error: " ++ s

toTree :: Tree -> Tree
toTree = id

fromTreeExp :: (ExpSYM repr) => (Tree -> Either ErrMsg repr) -> (Tree -> Either ErrMsg repr)
fromTreeExp self (Node "Lit" [Leaf n]) = liftM lit $ safeRead n
fromTreeExp self (Node "Neg" [e]) = liftM neg $ self e
fromTreeExp self (Node "Add" [e1, e2]) = liftM2 add (self e1) (self e2)
fromTreeExp self e = Left $ "Invalid tree: " ++ show e

fix f = f (fix f)

fromTree = fix fromTreeExp

tfm1_tree = toTree tfm1

tryPrintResult f (Left e) = putStrLn $ "Error: " ++ e
tryPrintResult f (Right x) = print (f x)

printFromTree tree fromTreeF =
  tryPrintResult view (fromTreeF tree)

tfm1'_eval = printFromTree tfm1_tree fromTree

fromTreeMul :: (ExpSYM repr, MulSYM repr) => (Tree -> Either ErrMsg repr) -> (Tree -> Either ErrMsg repr)
fromTreeMul self (Node "Mul" [e1, e2]) = liftM2 mul (self e1) (self e2)
fromTreeMul self e = fromTreeExp self e

fromTreeExt = fix fromTreeMul

tfm1''_eval = printFromTree tfm1_tree fromTreeExt

main :: IO ()
main = do
  tfm1'_eval
  tfm1''_eval




tf1'evalAndView =
  let deserialized = fromTree tfm1_tree
  in case deserialized of
    Left e -> putStrLn $ "Error: " ++ e
    Right x -> putStrLn $ view x ++ show (eval x)

--main :: IO ()
--main = tf1'evalAndView


instance (ExpSYM repr1, ExpSYM repr2) => ExpSYM(repr1, repr2) where
  lit x = (lit x, lit x)
  neg (e1, e2) = (neg e1, neg e2)
  add (e11, e21) (e12, e22) = (add e11 e12, add e21 e22)

instance (MulSYM repr1, MulSYM repr2) => MulSYM(repr1, repr2) where
  mul (e11, e21) (e12, e22) = (mul e11 e12, mul e21 e22)

duplicate :: (ExpSYM repr1, MulSYM repr1, ExpSYM repr2, MulSYM repr2) =>
  (repr1, repr2) -> (repr1, repr2)
duplicate = id




-- Pushing negation down
data Ctx = Pos | Neg

instance ExpSYM repr => ExpSYM (Ctx -> repr) where
  lit n Pos = lit n
  lit n Neg = neg (lit n)
  neg e Pos = e Neg
  neg e Neg = e Pos
  add e1 e2 ctx = add (e1 ctx) (e2 ctx)

push_neg e = e Pos

tf1_push_neg = push_neg tf1

instance MulSYM repr => MulSYM (Ctx -> repr) where
  mul e1 e2 Pos = mul (e1 Pos) (e2 Pos)
  mul e1 e2 Neg = mul (e1 Neg) (e2 Pos)

--main :: IO ()
--main = do
--  putStrLn ((view tf1) ++ " = " ++ (view tf1_push_neg))
--  putStrLn ((view tfm1) ++ " = " ++ (view (push_neg tfm1)))
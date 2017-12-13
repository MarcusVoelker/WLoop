module Main where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Either
import qualified Data.Map.Strict as M

import Text.LParse.Atomics
import Text.LParse.Parser
import Text.LParse.Transformers

data T = T | F deriving Show
data U = P Integer deriving Show
data Instr a = Special a | Inc Integer | Dec Integer | Seq (Instr a) (Instr a) | Loop Integer (Instr a) | XLoop (Instr a) deriving Show
data Program = Program (Instr T) (Instr U) deriving Show

class Atom a where
    parseAtom :: Parser r String a

instance Atom T where
    parseAtom = (const T <$> consume "T") <|> (const F <$> consume "F")

instance Atom U where
    parseAtom = P <$> (consume "P[" >> integer << consume "]")

aInstr :: (Atom a) => Parser r String (Instr a)
aInstr = (Special <$> parseAtom)
    <|> (Inc <$> integer << consume "+")
    <|> (Dec <$> integer << consume "-")
    <|> (Loop <$> (integer << consume "[") <*> (instr << consume "]"))
    <|> (XLoop <$> (consume "X[" >> instr << consume "]"))

instr :: (Atom a) => Parser r String (Instr a)
instr = (Seq <$> aInstr <*> instr) <|> aInstr

prog :: Parser r String Program
prog = Program <$> (instr << consume "|") <*> instr

dec :: Integer -> Integer
dec 0 = 0
dec x = x - 1

evaluate :: Integer -> (Instr T) -> M.Map Integer Integer -> Either Bool (M.Map Integer Integer)
evaluate _ (Special T) _ = Left True
evaluate _ (Special F) _ = Left False
evaluate _ (Inc i) m = Right (M.insertWith (+) i 1 m) 
evaluate _ (Dec i) m = Right (M.update (Just . dec) i m) 
evaluate x (Seq l r) m = evaluate x l m >>= (evaluate x r)
evaluate x (Loop i instr) m = foldM (flip $ evaluate x) m ([1..M.findWithDefault 0 i m] >> [instr])
evaluate x (XLoop instr) m = foldM (flip $ evaluate x) m ([1..x] >> [instr])

fullEvaluate :: Integer -> (Instr T) -> M.Map Integer Integer -> Bool
fullEvaluate x i m = head $ lefts [evaluate x i m >> Left False]

loop :: (Instr T) -> M.Map Integer Integer -> Integer
loop i m = loop' 0 i m
    where loop' x i m | fullEvaluate x i m = x
                      | otherwise          = loop' (x+1) i m

pEvaluate :: Integer -> Instr U -> M.Map Integer Integer -> IO (M.Map Integer Integer)
pEvaluate _ (Special (P i)) m = (putChar $ chr $ fromInteger $ M.findWithDefault 0 i m) >> return m
pEvaluate _ (Inc i) m = return (M.insertWith (+) i 1 m) 
pEvaluate _ (Dec i) m = return (M.update (Just . dec) i m) 
pEvaluate x (Seq l r) m = pEvaluate x l m >>= (pEvaluate x r)
pEvaluate x (Loop i instr) m = foldM (flip $ pEvaluate x) m ([1..M.findWithDefault 0 i m] >> [instr])
pEvaluate x (XLoop instr) m = foldM (flip $ pEvaluate x) m ([1..x] >> [instr])

printRes :: Integer -> Instr U -> IO ()
printRes x i = void $ pEvaluate x i (M.fromList $ zip [0..255] [0..255])

run :: Program -> M.Map Integer Integer -> IO ()
run (Program l p) m = printRes (loop l m) p

interpret :: String -> [Integer] -> IO ()
interpret s i = parse prog s (\p -> run p (M.fromList $ zip [0..] i)) putStrLn

main :: IO ()
main = do
    program <- getLine
    input <- getLine
    interpret program (read input)
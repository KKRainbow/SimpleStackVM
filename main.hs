   {-# LANGUAGE FlexibleInstances,FlexibleContexts #-}
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.List
import Debug.Trace

type Name = Char

data Exp = Val Int | Var Name | App Op Exp Exp deriving Show
data Op = Add | Sub | Mul | Div deriving (Show, Eq)

data Prog = Assign Name Exp
  | If Exp Prog Prog
  | While Exp Prog
  | Seqn [Prog]
  deriving Show

type Label = Int
data Inst = PUSH Int
  | PUSHV Name
  | POP Name
  | DO Op
  | JUMP Label
  | JUMPZ Label
  | LABEL Label
  deriving Show
type Code = [Inst]

type ST = StateT Int (Writer Code)

fresh :: ST Int
fresh = StateT (\s -> (writer ((s, s + 1), mempty)))

comexp :: Exp -> Code
comexp (Val int) = [PUSH int]
comexp (Var name) = [PUSHV name]
comexp (App op exp1 exp2) = comexp exp1 ++ comexp exp2 ++ [DO op]


mlabel :: Prog -> ST ()
mlabel (Assign name expr) = do
    tell $ comexp expr
    tell [POP name]

mlabel (If expr prog1 prog2) = do
    n <- fresh
    m <- fresh
    tell $ comexp expr
    tell [JUMPZ n]
    mlabel prog1
    tell [JUMP m]
    tell [LABEL n]
    mlabel prog2
    tell [LABEL m]

mlabel (While expr prog) = do
    n <- fresh
    m <- fresh
    tell [LABEL n]
    tell $ comexp expr
    tell [JUMPZ m]
    mlabel prog
    tell [JUMP n]
    tell [LABEL m]

mlabel (Seqn []) = do
    tell []

mlabel (Seqn (c:cs)) = do
    mlabel c
    mlabel (Seqn cs)

comp :: Prog -> Code
comp prog = snd $ runWriter $ (runStateT $ mlabel prog) 0

factorial :: Int -> Prog
factorial n = Seqn [Assign 'A' (Val 1),
                   Assign 'B' (Val n),
                   While (Var 'B') 
                   (Seqn [Assign 'A' (App Mul (Var 'A') (Var 'B')),
                         Assign 'B' (App Sub (Var 'B') (Val 1))])]


type Counter = Int
type Stack = [Int]
type Machine = (Code, Stack, Mem, Counter)
type MacState = MaybeT (State Machine)
type Mem = [(Name, Int)]

getVal :: Name -> Mem -> Maybe Int
getVal target [] = Nothing

getVal target ((name, val):cs)
  | name == target = Just val
  | otherwise = getVal target cs

removeVal :: Name -> Mem -> Mem
removeVal target [] = []
removeVal target ((pair@(name, val)):cs)
  | name == target = cs
  | otherwise = pair:(removeVal target cs)

doOper :: Stack -> Inst -> Mem -> (Stack, Mem, Maybe Int)
doOper stack (PUSH int) mem = (int:stack, mem, Just int)

doOper stack (PUSHV name) mem = let res = getVal name mem in
                                    case res of
                                      Just x -> (x:stack, mem, Just x)
                                      Nothing -> (stack, mem, Nothing)

doOper (top:stack) (POP name) mem = let mem' = removeVal name mem in
                                      (stack, (name, top):mem', Just top)

doOper (f:s:stack) (DO op) mem = case op of
                                 Add -> ((s+f):stack, mem, Just (s+f))
                                 Sub -> ((s-f):stack, mem, Just (s-f))
                                 Mul -> ((s*f):stack, mem, Just (s*f))
                                 Div -> if (f == 0) then (stack, mem, Nothing)
                                                    else ((s`div`f):stack, mem, Just (s`div`f))

getCounter :: Label -> Code -> Counter
getCounter label code = let idx=findIndex (\x -> case x of 
                                           (LABEL lab) -> lab == label 
                                           otherwise -> False) code 
                        in
                        case idx of 
                          Just i -> i
                          Nothing -> undefined

stackPop :: Stack -> Stack
stackPop [] = []
stackPop (s:cs) = cs

--Int指的是栈顶元素，最后可以将结果直接输出
runCode :: Int -> MacState Int
runCode top = MaybeT $ state $ \(code, stack, mem, counter) ->
    if counter < length code
       then 
       let inst=code!!counter
           (state', res') = case inst of
                              JUMP label -> ((code, stack, mem, getCounter label code), Just top)
                              JUMPZ label -> ((code, stackPop stack, mem, if top==0 then getCounter label code else counter+1), Just top)
                              LABEL label ->((code, stack, mem, counter + 1), Just top)
                              otherwise -> let (stack', mem', res)=doOper stack inst mem in
                                               ((code, stack', mem', counter+1), res)
                                            in (res', state')
    else
        (Nothing, (code, stack, mem, counter))

runAux :: Int -> MacState Int
runAux x = runCode x >>= \n -> runAux n

run :: Code -> (Maybe Int, Machine)
run code = (runState $ runMaybeT (do
    runAux 1)) (code, [], [], 0)

test :: Int -> (Maybe Int, Machine)
test n = run (comp $ factorial n)

eval :: Code -> Maybe Int
eval code = case (run code) of
              (x, (code, stack, mem, counter)) -> getVal 'A' mem

testEval :: Int -> Maybe Int
testEval n = eval (comp $ factorial n)

{-# LANGUAGE TupleSections #-}

module Brute (
    brute
) where

import Control.Arrow
import Control.Monad.State
import Data.Monoid ((<>))
import Data.List (foldl')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Word
import Debug.Trace

import AST
import LocalEval

{-
Okay, so the point of this module is to brute force it.
In SubExp we keen an expression along with what it evaluates
to on each input.

The IntMap called SubExps stores all of the SubExps of a given level.
That way, given all SubExp of size 6 and below, we can construct all
SubExp of size 7, with most of the expression already evaluated for us.

See 'main' for usage. Once there are multiple possible [Expr], that's
when we can guess the first one, and if that doesn't work we have a new input to test
all our Exprs on.

CURRENT STATUS:
    should work for all expressions without if0, fold or tfold
    probably won't work for bonus, I have no idea
    probably incredibly slowly

    NOTE: Some problems are unsolvable since we throw away exprs
    with the same results for all inputs. To alleviate this, increase the size of the 'answers' field.
    Or we could possibly just combine Exprs that give the same value without actually
    just silently throwing them away i.e. make the expr field of a SubExp of type [Expr].

TODO:
    add more strictness?

    if0 - works just like the others

    maybe add an 'escape hatch' if we find a problem smaller than
    needed that gives the right answers?

    fold, tfold
        the trouble with this is that we currently assume (in 'firstLevel')
        that ALL identifiers are "x" and so we don't need an environment.
        When this stops being the case (i.e. when there is more than
        one lambda) some things need to be rewritten.
-}

data SubExp = SubExp
    { expr      :: Expr -- could make this [Expr] to account for Expressions where
                        -- the first set of inputs isn't enough to distinguish it
    , fstAnswer :: !Word64
    , sndAnswer :: !Word64
    , answers   :: [Word64]}

instance Eq SubExp where
    SubExp _ f s a == SubExp _ f' s' a' =
        f == f' && s == s' && a == a'

instance Ord SubExp where
    SubExp _ f s a `compare` SubExp _ f' s' a' =
        case compare f f' of
            EQ -> case compare s s' of
                    EQ -> compare a a'
                    x' -> x'
            x  -> x

type SubExps = IntMap (Set SubExp)

main = print $ brute 6 tempOps ins outs
    where
    ins = [2,3,4]
    outs = [4,5,6] -- corresponding to "Lambda x (Plus (Id "x" (Plus 1 1))""
    tempOps = [Op2 Plus, Op1 Not] -- note that 0, 1 and Id are always included

brute :: Int -> [Op] -> [Word64] -> [Word64] -> [Program]
brute n ops ins outs = map (Lambda "x" . expr) (filter isCorrect (S.toList (exps M.! (n-1))))
    -- since the lambda at the start is 1, we only need an expr of size (n-1)
    where
    -- do all of the outputs of the expr match the correct outputs?
    isCorrect (SubExp _ f s a) = and (zipWith (==) (f:s:a) outs)
    -- in turn, add each level of expressions to the map
    exps = foldl (addLevel ops) (firstLevel ins) [2..n-1]

firstLevel :: [Word64] -> SubExps
firstLevel (i1:i2:is) = M.singleton 1 (S.fromList [zero, one, iden])
    where
    zero = SubExp Zero 0 0 (replicate m 0)
    one  = SubExp One 1 1 (replicate m 1)
    iden = i1 `seq` i2 `seq` SubExp (Id "x") i1 i2 is
    m    = length is
firstLevel _ = error "Brute needs at least 2 input examples"

addLevel :: [Op] -> SubExps -> Int -> SubExps
-- ONLY call this with n increasing from 2 onwards
addLevel ops exps n = foldl' (addOpAt n) exps ops

-- XXX: perhaps remove these bounds checks?
--          would have to unroll the loop "[2..n-1]" in
--          getSolns to some degree.
addOpAt :: Int -> SubExps -> Op -> SubExps

-- A Unary expression. A simple case, match the Op to each of the expressions of size n-1
addOpAt n exps' (Op1 o1) = addOpAt' n exps' (map addO1 (S.toList (exps' M.! (n-1))))
    where
    addO1 :: SubExp -> SubExp
    addO1 (SubExp e f s a) = f' `seq` s' `seq` SubExp (UnaryOp o1 e) f' s' (map (evalUnary o1) a)
        where
        f' = evalUnary o1 f
        s' = evalUnary o1 s

-- A Binary expression - more complicated since the sizes of the two subtrees
-- can be different as long as they add up to n-1 and so 'possLevels' gives us
-- the different possibilities.
-- NOTE, this assumes that ALL of the binary expressions are symmetric i.e. that
--     FUN x y = FUN y x
-- which is the case for Plus, And, Or and Xor.
addOpAt n exps' (Op2 o2) | n >= 2 = addOpAt' n exps' $
    [ addO2 e1 e2
    | (l1,l2) <- possLevels (n-1)
    , (e1, e2) <- if l1 == l2
        then sym (S.toList (exps' M.! l1)) -- we don't want duplicates w.r.t. symmetry
        else [ (e1', e2')                  -- we're in no danger of duplicates as e1 and e2 are different sizes
             | e1' <- S.toList (exps' M.! l1)
             , e2' <- S.toList (exps' M.! l2)]]
    where
    -- sym [a,b,c] = [(a,a),(a,b),(a,c),(b,b),(b,c),(c,c)]
    sym :: [SubExp] -> [(SubExp, SubExp)]
    sym es@(e:es') = map (e,) es ++ sym es'
    sym [] = []

    -- possLevels 6 = [(1,5),(2,4),(3,3)]
    -- assumed that n > 2
    possLevels :: Int -> [(Int, Int)]
    possLevels n = [(x, n-x) | x <- [1..n `div` 2]]

    addO2 :: SubExp -> SubExp -> SubExp
    addO2 (SubExp e1 f1 s1 a1) (SubExp e2 f2 s2 a2) = f' `seq` s' `seq`
        SubExp (BinaryOp o2 e1 e2) f' s' (zipWith (evalBinary o2) a1 a2)
        where
        f' = evalBinary o2 f1 f2
        s' = evalBinary o2 s1 s2

addOpAt _ e _ = e

-- simply to reuse code
{-# INLINE addOpAt' #-}
addOpAt' :: Int -> SubExps -> [SubExp] -> SubExps
addOpAt' n exps' subExps = M.insertWith (<>) n (S.fromList subExps) exps'
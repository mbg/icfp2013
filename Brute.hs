{-# LANGUAGE TupleSections #-}

import Control.Arrow
import Control.Monad.State
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as M
import Data.Word
import Debug.Trace

import AST
import LocalEval

{-
Okay, so the point of this module is to brute force it.
We keep a pair SubExp = (Expr, [Word64]) of the expression
itself alone with what it will evaluate to on each input.

The IntMap called SubExps stores all of the SubExps of a given level.
That way, given all SubExp of size 6 and below, we can construct all
SubExp of size 7, with most of the expression already evaluated for us.

Ideally we want to be strict in the actual evaluated [Word64]
for efficiency reasons but if we have too many inputs this could slow
things down so we may only want to be strict in the first few (will
probably need a custom data type for this). I'm not yet sure if we
should be lazy in the Expr or not - if it's just data constructors
then would evaluating to whnf make a difference?

See 'main' for usage. Once there are multiple possible [Expr], that's
when we can either
    1) Guess the first one, and if that doesn't work we have a new input to test
        all our Exprs on or;
    2) Ask for more input (perhaps evenly spread) so that we can differentiate
        between our current [Expr]

CURRENT STATUS:
    should work for all expressions without if0, fold or tfold
    probably won't work for bonus, I have no idea
    probably incredibly slowly

TODO:
    change SubExp to something like
        data SubExp = SubExp
            { expr      :: !Expr -- strict or not?
            , fstAnswer :: !Word64
            , sndAnswer :: !Word64
            , answers   :: [Word64]}
        as just the first or second answer should be
        enough to differentiate between different SubExp
        while the rest should be lazy.

    add strictness elsewhere too?

    change SubExps to
        type SubExps = IntMap (Set SubExp)
    and add an Eq and Ord instance for SubExp which ignores the expr
    field. That way, we don't differentiate between different
    subexpressions that give the same answer for all inputs.
    This will be much faster BUT HOWEVER this will cause some problems to be
    unsolvable since we'll already have thrown away all the other possibilities.
    To alleviate this, increase the size of the 'answers' field.

    the skeleton for if0 is there, get it working

    maybe add an 'escape hatch' if we find a problem smaller than
    needed that gives the right answers? Probably a bad idea tbh.

    fold, tfold
        the trouble with this is that we currently assume (in 'firstLevel')
        that ALL identifiers are "x" and so we don't need an environment.
        When this stops being the case (i.e. when there is more than
        one lambda) some things need to be rewritten.
-}

type SubExp = (Expr, [Word64])
type SubExps = IntMap [SubExp]

-- an example of use. See how lots of solutions are generated that are all the same?
-- that Ord instance for SubExp above will stop this from happening
main = print $ getSolns 6 ins outs tempOps
    where
    ins = [2,3,4]
    outs = [4,5,6] -- corresponding to "Lambda x (Plus (Id "x" (Plus 1 1))""
    tempOps = [Op2 Plus, Op1 Not] -- note that 0, 1 and Id are always included

getSolns :: Int -> [Word64] -> [Word64] -> [Op] -> [Expr]
getSolns n ins outs ops = map fst $ filter isCorrect (exps M.! (n-1))
    -- since the lambda at the start is 1, we only need an expr of size (n-1)
    where
    -- do all of the outputs of the expr match the correct outputs?
    isCorrect = and . zipWith (==) outs . snd
    -- in turn, add each level of expressions to the map
    exps = foldl (addLevel ops) (firstLevel ins) [2..n-1]

firstLevel :: [Word64] -> SubExps
firstLevel input = M.singleton 1 [(Zero, replicate m 0), (One, replicate m 1), (Id "x", input)]
    where
    m = length input

addLevel :: [Op] -> SubExps -> Int -> SubExps
-- it'd be nice to remove these bounds checks
-- also ONLY call this with n increasing from 2 onwards
addLevel ops exps n = foldl (addOpAt n) exps ops

-- XXX: (++) on lists is bad!, use Set instead
-- XXX: perhaps remove these bounds checks?
--          would have to unroll the loop "[2..n-1]" in
--          getSolns to some degree.
addOpAt :: Int -> SubExps -> Op -> SubExps

-- A Unary expression. A simple case, match the Op to each of the expressions of size n-1
addOpAt n exps' (Op1 o1) = addOpAt' n exps' (map addO1 (exps' M.! (n-1)))
    where
    addO1 :: SubExp -> SubExp
    addO1 (e, ans) = (UnaryOp o1 e, map (evalUnary o1) ans)

-- A Binary expression - more complicated since the sizes of the two subtrees
-- can be different as long as they add up to n-1 and so 'possLevels' gives us
-- the different possibilities.
-- NOTE, this assumes that ALL of the binary expressions are symmetric i.e. that
--     FUN x y = FUN y x
-- which is the case for Plus, And, Or and Xor.
addOpAt n exps' (Op2 o2) | n >= 2 = addOpAt' n exps' $
    [ addO2 e1 e2
    | (l1,l2) <- possLevels (n-1)
    , (e1, e2) <- if False -- l1 == l2
        then sym (exps' M.! l1) -- we don't want duplicates w.r.t. symmetry
        else [ (e1', e2')       -- we're in no danger of duplicates as e1 and e2 are different sizes
             | e1' <- (exps' M.! l1)
             , e2' <- (exps' M.! l2)]]
    where
    -- sym [a,b,c] = [(a,a),(a,b),(a,c),(b,b),(b,c),(c,c)]
    sym :: [SubExp] -> [(SubExp, SubExp)]
    sym es@(e:es') = map (e,) es ++ sym es'
    sym [] = []

    -- possLevels 6 = [(1,5),(2,4),(3,3)]
    -- assumed that n > 2
    possLevels :: Int -> [(Int, Int)]
    possLevels n = [(x, n-x) | x <- [1..n `div` 2]]

    possExps :: [SubExp]
    possExps = undefined -- the levels of the sub expressions just need to sum to n-1

    addO2 :: SubExp -> SubExp -> SubExp
    addO2 (e1, ans1) (e2, ans2) = (BinaryOp o2 e1 e2, zipWith (evalBinary o2) ans1 ans2)
{-
addOpAt n exps' OIfZero | n >= 3 = addOpAt' n exps' $
    [ addIfZero e1 e2 e3
    | (l1, l2, l3) <- possLevels (n-1)
    , e1 <- exps' M.! l1
    , e2 <- exps' M.! l2
    , e3 <- exps' M.! l3 ]

    where
    -- see posLevels above, but must satisfy
        forall n. all (\(x,y,z) -> x+y+z == n) (possLevels n) == True
    possLevels :: Int -> [(Int, Int, Int)]
    possLevels = undefined

    addIfZero :: (SubExp, SubExp, SubExp) -> SubExp
    addIfZero ((e1, ans1), (e2, ans2), (e3, ans3)) = (IfZero e1 e2 e3, zipWith3 evalIfZero ans1 ans2 ans3)

    evalIfZero :: Word64 -> Word64 -> Word64 -> Word64
    -- TODO: "if (x .&. 0)" or something instead?
    evalIfZero x t f = if x == 0 then t else f
-}

addOpAt _ e _ = e

-- simply to reuse code
addOpAt' n exps' subExps = M.insertWith (++) n subExps exps'
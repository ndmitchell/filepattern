{-# LANGUAGE ViewPatterns, DeriveFunctor, BangPatterns, ScopedTypeVariables #-}

-- | Applying a set of paths vs a set of patterns efficiently
module System.FilePattern.Step(
    step, Step(..)
    ) where

import System.FilePattern.Core
import System.FilePattern.Wildcard

import Control.Monad.Extra
import Data.List.Extra
import Data.Semigroup
import Data.Tuple.Extra
import Data.Functor
import Data.Either
import qualified Data.List.NonEmpty as NE
import Prelude


-- | The result of 'step', used to process successive path components of a set of 'FilePath's.
data Step a = Step
    {stepDone :: [(a, [String])]
        -- ^ The files that match at this step. Includes the list that would have been produced by 'System.FilePattern.match',
        --   along with the values passed to 'step'. These results are not necessarily in order.
    ,stepNext :: Maybe [String]
        -- ^ If 'Just' then all non-included components will result in dull 'Step' values from 'stepApply',
        --   with 'stepNext' being @'Just' []@ and 'stepDone' being @[]@. The values in 'stepNext' form a set - their order
        --   is irrelevant but there will be no duplicates in values arising from 'step'.
    ,stepApply :: String -> Step a
        -- ^ Apply one component from a 'FilePath' to get a new 'Step'.
    }
    deriving Functor


instance Semigroup (Step a) where
    a <> b = sconcat $ NE.fromList [a,b]
    sconcat (NE.toList -> ss)
        | [s] <- ss = s
        | otherwise = Step
            {stepDone = concatMap stepDone ss
            ,stepNext = nubOrd <$> concatMapM stepNext ss
            ,stepApply = \x -> fastFoldMap (`stepApply` x) ss
            }

instance Monoid (Step a) where
    mempty = Step [] (Just []) $ const mempty
    mappend = (<>)
    mconcat = maybe mempty sconcat . NE.nonEmpty -- important: use the fast sconcat

fastFoldMap :: Monoid m => (a -> m) -> [a] -> m
fastFoldMap f = mconcat . map f -- important: use the fast mconcat


-- Invariant: No two adjacent Lits
-- Invariant: No empty Lits
data Pat = Lits [Wildcard String]
         | StarStar

toPat :: Pattern -> [Pat]
toPat (Pattern (Literal xs)) = [Lits xs]
toPat (Pattern (Wildcard pre mid post)) = intercalate [StarStar] $ map lit $ pre : mid ++ [post]
    where lit xs = [Lits xs | xs /= []]

-- | Efficient matching of a set of 'FilePattern's against a set of 'FilePath's.
--   First call 'step' passing in all the 'FilePattern's, with a tag for each one.
--   Next call the methods of 'Step', providing the components of the 'FilePath's in turn.
--
--   Useful for efficient bulk searching, particularly directory scanning, where you can
--   avoid descending into directories which cannot match.
step :: [(a, FilePattern)] -> Step a
step = fastFoldMap (step1 . second (toPat . parsePattern))

match1 :: Wildcard String -> String -> Maybe [String]
match1 w x = rights <$> wildcardMatch equals w x

step1 :: forall a . (a, [Pat]) -> Step a
step1 (val, pat) = f pat id
    where
        -- given the prefix of the parts (as a difference list), and the rest of the pattern, calc the Step
        f :: [Pat] -> ([String] -> [String]) -> Step a

        -- normal path, dispatch on what you find next
        f [] parts = mempty{stepDone = [(val, parts [])]}

        -- two stars in a row, the first will match nothing, the second everything
        f (StarStar:StarStar:ps) parts = f  (StarStar:ps) (parts . ([]:))

        -- if you have literals next, match them
        f (Lits (l:ls):ps) parts = Step
            {stepDone = []
            ,stepNext = case l of Literal v -> Just [v]; Wildcard{} -> Nothing
            ,stepApply = \s -> case match1 l s of
                Just xs -> f ([Lits ls | ls /= []] ++ ps) (parts . (xs++))
                Nothing -> mempty
            }

        -- if anything else is allowed, just quickly allow it
        f [StarStar] parts = g []
            where
                g rseen = Step
                    {stepDone = [(val, parts [mkParts $ reverse rseen])]
                    ,stepNext = Nothing
                    ,stepApply = \s -> g (s:rseen)
                    }

        -- if you have a specific tail prefix, find it
        f [StarStar, Lits (reverse &&& length -> (rls,nls))] parts = g 0 []
            where
                g !nseen rseen = Step
                    {stepDone = case zipWithM match1 rls rseen of
                        _ | nseen < nls -> [] -- fast path
                        Just xss -> [(val, parts $ mkParts (reverse $ drop nls rseen) : concat (reverse xss))]
                        Nothing -> []
                    ,stepNext = Nothing
                    ,stepApply = \s -> g (nseen+1) (s:rseen)
                    }

        -- we know the next literal, and it doesn't have any constraints immediately after
        f (StarStar:Lits [l]:StarStar:ps) parts = g []
            where
                g rseen = Step
                    {stepDone = []
                    ,stepNext = Nothing
                    ,stepApply = \s -> case match1 l s of
                        Just xs -> f (StarStar:ps) (parts . (++) (mkParts (reverse rseen) : xs))
                        Nothing -> g (s:rseen)
                    }

        -- the hard case, a floating substring, accumulate at least N, then star testing in reverse
        f (StarStar:Lits (reverse &&& length -> (rls,nls)):StarStar:ps) parts = g 0 []
            where
                g !nseen rseen = Step
                    {stepDone = []
                    ,stepNext = Nothing
                    ,stepApply = \s -> case zipWithM match1 rls (s:rseen) of
                        _ | nseen+1 < nls -> g (nseen+1) (s:rseen) -- not enough accumulated yet
                        Nothing -> g (nseen+1) (s:rseen)
                        Just xss -> f (StarStar:ps) (parts . (++) (mkParts (reverse $ drop nls $ s:rseen) : concat (reverse xss)))
                    }

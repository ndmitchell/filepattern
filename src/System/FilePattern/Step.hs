{-# LANGUAGE ViewPatterns, DeriveFunctor #-}

-- | Applying a set of paths vs a set of patterns efficiently
module System.FilePattern.Step(
    step, Step(..)
    ) where

import System.FilePattern.Core
import Control.Monad.Extra
import Data.List.Extra
import Data.Semigroup
import Data.Functor
import qualified Data.List.NonEmpty as NE
import Prelude


-- | The result of 'step', used to process successive path components of a set of 'FilePath's.
data Step a = Step
    {stepEmpty :: Bool
        -- ^ If 'False' then no future calls to 'stepApply' will produce a non-empty 'stepDone'.
        --   In other words, there are no further matching files down this path.
    ,stepDone :: [([String], a)]
        -- ^ The files that match at this step. Includes the list that would have been produced by 'match',
        --   along with the values passed to 'step'.
    ,stepRelevant :: Maybe [String]
        -- ^ If 'Just' then all non-included components will result in dull 'Step' values from 'stepApply',
        --   with 'stepEmpty' being 'True' and 'stepDone' being @[]@.
    ,stepApply :: String -> Step a
        -- ^ Apply one component from a 'FilePath' to get a new 'Step'.
    }
    deriving Functor


instance Semigroup (Step a) where
    a <> b = sconcat $ NE.fromList [a,b]
    sconcat (NE.toList -> ss)
        | [s] <- ss = s
        | otherwise = Step
            {stepEmpty = all stepEmpty ss
            ,stepDone = concatMap stepDone ss
            ,stepRelevant = nubOrd <$> concatMapM stepRelevant ss
            ,stepApply = \x -> fastFoldMap (`stepApply` x) ss
            }

instance Monoid (Step a) where
    mempty = Step True [] (Just []) $ const mempty
    mappend = (<>)
    mconcat = maybe mempty sconcat . NE.nonEmpty -- important: use the fast sconcat

fastFoldMap :: Monoid m => (a -> m) -> [a] -> m
fastFoldMap f = mconcat . map f -- important: use the fast mconcat


-- | Efficient matching of a set of 'FilePattern's against a set of 'FilePath's.
--   First call 'step' passing in all the 'FilePattern's, with a tag for each one.
--   Next call the methods of 'Step', providing the components of the 'FilePath's in turn.
--
--   Useful for efficient bulk searching, particularly directory scanning, where you can
--   avoid descending into directories which cannot match.
step :: [(FilePattern, a)] -> Step a
step [pat] = step1 pat
step xs = fastFoldMap step1 xs

step1 :: (FilePattern, a) -> Step a
step1 (parsePattern -> pat, val) = f []
    where
        f rpath = Step False (g $ reverse rpath) Nothing (\x -> f $ x : rpath)
        g path = case match pat $ mkPath path of
            Nothing -> []
            Just v -> [(v, val)]

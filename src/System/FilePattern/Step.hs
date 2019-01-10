{-# LANGUAGE DeriveFunctor #-}

-- | Applying a set of paths vs a set of patterns efficiently
module System.FilePattern.Step(
    step, Step(..)
    ) where

import System.FilePattern.Core


data Step a = Step
    {stepEmpty :: Bool -- if False then stepApply any number of times will never result in stepDone being non-empty
    ,stepDone :: [([String], a)] -- List of things that are done at this step, in order they were passed to step
    ,stepRelevant :: Maybe [String] -- If Just then a superset of the things that will return interesting results
    ,stepApply :: [String] -> Step a -- Apply a set of path components
    }
    deriving Functor

-- | Efficient matching of a set of paths with a set of patterns.
step :: [(FilePattern, a)] -> Step a
step = undefined

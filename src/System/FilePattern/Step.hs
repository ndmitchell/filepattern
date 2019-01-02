{-# LANGUAGE DeriveFunctor #-}

-- | The type of wildcards, which generalises to both patterns
--   inside a filename and along patterns. e.g.
--
-- > *xy* = Wildcard [] ["xy"] []
-- > **/xxx/yyy/** = Wildcard [] [[Literal "xxx", Literal "yyy"]] []
--
--   Some more examples focusing on the first type of pattern:
--
-- > xyz = Literal "xyz"
-- > x*y*z = Wildcard "x" ["y"] ["z"]
-- > x**z = Wildcard "x" [""] ["z"]
module System.FilePattern.Step(
    step, Step(..)
    ) where

import System.FilePattern.Core


data Step a = Step
    {stepDone :: [([Part], a)]
    ,stepRelevant :: Maybe [String]
    ,stepApply :: [String] -> Step a
    }

-- | Efficient path walking with a set of patterns.
--   The first component of the result is 'True' iff the empty string is matched by any pattern.
step :: [(FilePattern, a)] -> Step a
step = undefined

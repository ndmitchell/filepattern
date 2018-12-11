
-- | The type of patterns and wildcards
module System.FilePattern.Wildcard(
    Wildcard(..),
    wildcard,
    ) where

import Data.Functor
import Data.List.Extra
import System.FilePattern.ListBy
import Prelude


-- | Representing either literals, or wildcards
data Wildcard a = Wildcard a [a] a -- ^ prefix [mid-parts] suffix
                | Literal a -- ^ literal match
    deriving (Show,Eq,Ord)

-- | Given a wildcard, and a test string, return the matches.
--   Only return the first (all patterns left-most) valid star matching.
wildcard :: (a -> b -> Maybe c) -> Wildcard [a] -> [b] -> Maybe [Either [c] [b]]
wildcard eq (Literal mid) x = (:[]) . Left <$> eqListBy eq mid x
wildcard eq (Wildcard pre mid post) x = do
    (pre, x) <- stripPrefixBy eq pre x
    (x, post) <- stripSuffixBy eq post x
    mid <- stripInfixes mid x
    return $ [Left pre] ++ mid ++ [Left post]
    where
        stripInfixes [] x = Just [Right x]
        stripInfixes (m:ms) y = do
            (a,b,x) <- stripInfixBy eq m y
            (\c -> Right a:Left b:c) <$> stripInfixes ms x

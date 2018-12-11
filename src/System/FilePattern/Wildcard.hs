
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
module System.FilePattern.Wildcard(
    Wildcard(..),
    wildcard,
    equals
    ) where

import Data.Functor
import Data.List.Extra
import System.FilePattern.ListBy
import Prelude


equals :: Eq a => a -> a -> Maybe ()
equals x y = if x == y then Just () else Nothing


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

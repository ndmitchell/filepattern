
-- | The type of patterns and wildcards
module System.FilePattern.Type(
    FilePattern,
    Pats(..),
    Pat(..),
    Wildcard(..),
    wildcard,
    wildcardBy,
    lit, fromLit,
    star
    ) where

import Data.Functor
import Data.List.Extra
import System.FilePattern.ListBy
import Prelude


-- | A type synonym for file patterns, containing @**@ and @*@. For the syntax
--   and semantics of 'FilePattern' see '?=='.
--
--   Most 'FilePath' values lacking literal @.@ and @..@ components are suitable as 'FilePattern' values which match
--   only that specific file. On (Windows @\\@ is treated as equivalent to @\/@.
--
--   You can write 'FilePattern' values as a literal string, or build them
--   up using the operators '<.>' and '</>'.
type FilePattern = String


-- | Parsed 'FilePattern'.
newtype Pats = Pats {fromPats :: [Pat]}
    deriving (Eq,Show)

-- | Representing either literals, or wildcards
data Wildcard a = Wildcard a [a] a -- ^ prefix [mid-parts] suffix
                | Literal a -- ^ literal match
    deriving (Show,Eq,Ord)

-- | Given a wildcard, and a test string, return the matches.
--   Only return the first (all patterns left-most) valid star matching.
wildcard :: Eq a => Wildcard [a] -> [a] -> Maybe [[a]]
wildcard (Literal mid) x = if mid == x then Just [] else Nothing
wildcard (Wildcard pre mid post) x = do
    y <- stripPrefix pre x
    z <- if null post then Just y else stripSuffix post y
    stripInfixes mid z
    where
        stripInfixes [] y = Just [y]
        stripInfixes (m:ms) y = do
            (a,z) <- stripInfix m y
            (a:) <$> stripInfixes ms z


-- | Given a wildcard, and a test string, return the matches.
--   Only return the first (all patterns left-most) valid star matching.
wildcardBy :: (a -> b -> Maybe c) -> Wildcard [a] -> [b] -> Maybe [Either [c] [b]]
wildcardBy eq (Literal mid) x = (:[]) . Left <$> eqListBy eq mid x
wildcardBy eq (Wildcard pre mid post) x = do
    (pre, x) <- stripPrefixBy eq pre x
    (x, post) <- stripSuffixBy eq post x
    mid <- stripInfixes mid x
    return $ [Left pre] ++ mid ++ [Left post]
    where
        stripInfixes [] x = Just [Right x]
        stripInfixes (m:ms) y = do
            (a,b,x) <- stripInfixBy eq m y
            (\c -> Right a:Left b:c) <$> stripInfixes ms x


data Pat = Skip -- ^ /**/
         | Stars (Wildcard String) -- ^ *foo*, prefix (fixed), infix floaters, suffix
                                   -- e.g. *foo*bar = Stars "" ["foo"] "bar"
            deriving (Show,Eq,Ord)

-- | Create a mattern equivalent to *.
star :: Pat
star = Stars $ Wildcard "" [] ""

-- | Create a mattern equivalent to x as a literal.
lit :: String -> Pat
lit = Stars . Literal

fromLit :: Pat -> Maybe String
fromLit (Stars (Literal x)) = Just x
fromLit _ = Nothing

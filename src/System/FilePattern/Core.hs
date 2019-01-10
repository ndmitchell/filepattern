
-- | The type of patterns and wildcards
module System.FilePattern.Core(
    FilePattern,
    Pattern(..), parsePattern,
    Path(..), parsePath,
    match, subst,
    Fingerprint, fingerprint
    ) where

import Data.Functor
import System.FilePattern.Wildcard
import System.FilePath (isPathSeparator)
import Data.Either.Extra
import System.FilePattern.Monads
import Data.List.Extra


-- | A type synonym for file patterns, containing @**@ and @*@. For the syntax
--   and semantics of 'FilePattern' see '?=='.
--
--   Most 'FilePath' values lacking literal @.@ and @..@ components are suitable as 'FilePattern' values which match
--   only that specific file. On (Windows @\\@ is treated as equivalent to @\/@.
--
--   You can write 'FilePattern' values as a literal string, or build them
--   up using the operators '<.>' and '</>'.
type FilePattern = String


newtype Path = Path [String]
    deriving (Show,Eq,Ord)

newtype Pattern = Pattern (Wildcard [Wildcard String])
    deriving (Show,Eq,Ord)


-- [Note: Split on ""]
--
-- For parsing patterns and paths, "" can either be [] or [""].
-- Assuming they are consistent, the only cases that are relevant are:
--
-- > match "" "" = Just []
-- > match "*" "" = if [] then Nothing else Just [""]
-- > match "**" "" = if [] then Just [] else Just [""]
--
-- We pick "" splits as [""] because that is slightly more permissive,
-- follows the builtin semantics of split, and matches the 'filepath'
-- library slightly better.

parsePath :: FilePath -> Path
parsePath = Path . split isPathSeparator

parsePattern :: FilePattern -> Pattern
parsePattern = Pattern . fmap (map $ f '*') . f "**" . split isPathSeparator
    where
        f :: Eq a => a -> [a] -> Wildcard [a]
        f x xs = case split (== x) xs of
            pre:mid_post -> case unsnoc mid_post of
                Nothing -> Literal pre
                Just (mid, post) -> Wildcard pre mid post


-- [Note: Conversion to string]

mkPart :: String -> String
mkPart = id

mkParts :: [String] -> String
mkParts xs | all null xs = replicate (length xs) '/'
           | otherwise = intercalate "/" xs

fromPart :: String -> String
fromPart = id

fromParts :: String -> [String]
fromParts xs | all isPathSeparator xs = replicate (length xs) []
             | otherwise = split isPathSeparator xs

match :: Pattern -> Path -> Maybe [String]
match (Pattern w) (Path x) = f <$> wildcardMatch (wildcardMatch equals) w x
    where
        f :: [Either [[Either [()] String]] [String]] -> [String]
        f (Left x:xs) = map mkPart (rights $ concat x) ++ f xs
        f (Right x:xs) = mkParts x : f xs
        f [] = []


subst :: Pattern -> [String] -> Maybe Path
subst (Pattern w) ps = do
    let inner w = concat <$> wildcardSubst (fromPart <$> getNext) pure w
        outer w = concat <$> wildcardSubst (fromParts <$> getNext) (traverse inner) w
    (ps, v) <- runNext ps $ outer w
    if null ps then Just $ Path v else Nothing


newtype Fingerprint = Fingerprint [Bool]
    deriving Eq

fingerprint :: Pattern -> Fingerprint
fingerprint (Pattern w) = Fingerprint $ fst $ runOut $ outer w
    where
        inner = wildcardSubst (addOut False) (const $ pure ())
        outer = wildcardSubst (addOut True) (void . traverse inner)

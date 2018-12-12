{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ConstraintKinds #-}

-- | This library supports patterns containing @*@ and @**@, but also
--   \"legacy\" patterns including @\/\/@ as well.
--   To support that, we have 'with' patterns that are customized by the lexer.
module System.FilePattern.Core(
    -- * Primitive API, as exposed
    FilePattern, matchBoolWith, matchWith,
    -- * Optimisation opportunities
    simpleWith,
    -- * Multipattern file rules
    compatibleWith, substituteWith,
    -- * Accelerated searching
    Walk(..), walkWith
    ) where

import Control.Exception.Extra
import Data.Either.Extra
import Data.List.Extra
import Data.Maybe
import Data.Tuple.Extra
import Control.Applicative
import Prelude
import System.FilePattern.Type
import System.FilePattern.Wildcard
import System.FilePath (isPathSeparator)


---------------------------------------------------------------------
-- PATTERNS

matchWildcard :: Pattern -> Path -> Maybe [String]
matchWildcard (Pattern w) (Path x) = f <$> wildcard (wildcard equals) w x
    where
        f :: [Either [[Either [()] String]] [String]] -> [String]
        f (Left x:xs) = rights (concat x) ++ f xs
        f (Right x:xs) = concatMap (++ "/") x : f xs
        f [] = []


matchBoolWith :: Pats -> FilePath -> Bool
matchBoolWith pat = isJust . matchWith pat


-- | Like '?==', but returns 'Nothing' on if there is no match, otherwise 'Just' with the list
--   of fragments matching each wildcard. For example:
--
-- @
-- 'filePattern' \"**\/*.c\" \"test.txt\" == Nothing
-- 'filePattern' \"**\/*.c\" \"foo.c\" == Just [\"",\"foo\"]
-- 'filePattern' \"**\/*.c\" \"bar\/baz\/foo.c\" == Just [\"bar\/baz/\",\"foo\"]
-- @
--
--   Note that the @**@ will often contain a trailing @\/@, and even on Windows any
--   @\\@ separators will be replaced by @\/@.
matchWith :: Pats -> FilePath -> Maybe [String]
matchWith ps = matchWildcard (toWildcard ps) .
    Path . (\x -> if null x then [""] else x) . filter (/= ".") .
    split isPathSeparator


---------------------------------------------------------------------
-- MULTIPATTERN COMPATIBLE SUBSTITUTIONS

-- | Extract just the specials, return list of **=True, *=False
specialsWith :: Pats -> [Bool]
specialsWith = concatMap f . fromPats
    where
        f Skip = [True]
        f (Stars (Wildcard _ xs _)) = replicate (length xs + 1) False
        f (Stars Literal{}) = []

-- | Is the pattern free from any * and **.
simpleWith :: Pats -> Bool
simpleWith = null . specialsWith

-- | Do they have the same * and ** counts in the same order
compatibleWith :: [Pats] -> Bool
compatibleWith [] = True
compatibleWith (x:xs) = all ((==) (specialsWith x) . specialsWith) xs

-- | Given a successful 'match', substitute it back in to a 'compatible' pattern.
--
-- > p '?==' x ==> substitute (extract p x) p == x
substituteWith :: Partial => String -> [String] -> (FilePattern, Pats) -> FilePath
substituteWith func parts (pat, fromPats -> ps)
    | let want = sum $ map count ps, let got = length parts, want /= got =
        error $ func ++ " given the wrong number of parts, wanted " ++
                show want ++ ", got " ++ show got ++
                ", for the pattern " ++ show pat ++ " and parts " ++ show parts
    | otherwise = intercalate "/" $ concat $ snd $ mapAccumL f parts ps
    where
        count Skip = 1
        count (Stars Literal{}) = 0
        count (Stars (Wildcard _ mid _)) = length mid + 1

        f ms (Stars (Literal x)) = (ms, [x])
        f (m:ms) Skip = (ms, splitSep m)
        f ms (Stars (Wildcard pre mid post)) = (ms2, [concat $ pre : zipWith (++) ms1 (mid++[post])])
            where (ms1,ms2) = splitAt (length mid + 1) ms
        f _ _ = error $ "substitution, internal error, with " ++ show parts ++ " " ++ show ps

        splitSep = linesBy (== '/')


---------------------------------------------------------------------
-- EFFICIENT PATH WALKING

-- | The 'Walk' constructor provides an operation that when given a list of files/directories in this
--   directory returns the @(matches, continues)@ that are of interest. The @matches@ are things in this
--   directory which completely match a pattern. The @continues@ are those things which don't match in their
--   entirety, but may match if the path continues, and the 'Walk' that would make them match.
--   In all cases the @matches@ and @continues@ will be drawn from the list passed in.
--
--   The 'WalkTo' constructor is used when the matching items can be predicted in advance, without
--   enumerating all possible matches first.
data Walk = Walk   (String -> (Bool, Maybe Walk))
          | WalkTo ([String], [(String,Walk)])

walkWith :: [Pats] -> (Bool, Maybe Walk)
walkWith patterns = (any (\p -> matchBoolWith (mkPats p) "") ps2, f ps2)
    where
        ps2 = map fromPats patterns

        f [] = Nothing
        f (nubOrd -> ps) = Just $ case () of
            _ | Just fin <- mapM fromLit fin
              , Just nxt <- mapM (\(a,b) -> (,) <$> fromLit a <*> f b) nxt
              -> WalkTo (fin, nxt)
            _ -> Walk $ \x ->
                (finStar || any (`matchOne` x) fin
                ,f $ concat [b | (a,b) <- nxt, matchOne a x])
            where
                finStar = star `elem` fin
                fin = nubOrd $ mapMaybe final ps
                nxt = groupSort $ concatMap next ps

matchOne :: Pat -> String -> Bool
matchOne (Stars x) y = isJust $ wildcard equals x y
matchOne Skip _ = False

next :: [Pat] -> [(Pat, [Pat])]
next (Skip:xs) = (star,Skip:xs) : next xs
next (x:xs) = [(x,xs) | not $ null xs]
next [] = []

final :: [Pat] -> Maybe Pat
final (Skip:xs) = if isEmpty xs then Just star else final xs
final (x:xs) = if isEmpty xs then Just x else Nothing
final [] = Nothing

isEmpty :: [Pat] -> Bool
isEmpty = all (== Skip)

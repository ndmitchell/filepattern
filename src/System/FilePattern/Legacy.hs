{-# LANGUAGE ConstraintKinds #-}

-- | A module for pattern matching on file names.
--
-- >>> "//*.png" ?== "/foo/bar/baz.png"
-- True
--
--   This module supports @*@ and @**@ like "FilePattern", and also supports @\/\/@.
--   The inclusion of @\/\/@ in patterns was a misfeature, as it interacts poorly with
--   'Development.Shake.FilePath.<.>' and 'Development.Shake.FilePath.</>'.
--   This module will be deleted at some point in the future.
module System.FilePattern.Legacy
    {-# DEPRECATED "Use module System.FilePattern and avoid // in the patterns" #-}
    (
    -- * Primitive API
    FilePattern, (?==), match,
    -- * Optimisation opportunities
    simple,
    -- * Multipattern file rules
    compatible, substitute,
    -- * Accelerated searching
    Walk(..), walk,
    -- * Deprecation path
    addUnsafeLegacyWarning
    ) where

import Control.Exception.Extra
import Data.Maybe
import System.FilePattern.Core
import System.FilePattern.Parser(parseLegacy, addUnsafeLegacyWarning)
import Prelude


---------------------------------------------------------------------
-- PATTERNS

-- | Match a 'FilePattern' against a 'FilePath', There are three special forms:
--
-- * @*@ matches an entire path component, excluding any separators.
--
-- * @\/\/@ matches an arbitrary number of path components, including absolute path
--   prefixes.
--
-- * @**@ as a path component matches an arbitrary number of path components, but not
--   absolute path prefixes.
--   Currently considered experimental.
--
--   Some examples:
--
-- * @test.c@ matches @test.c@ and nothing else.
--
-- * @*.c@ matches all @.c@ files in the current directory, so @file.c@ matches,
--   but @file.h@ and @dir\/file.c@ don't.
--
-- * @\/\/*.c@ matches all @.c@ files anywhere on the filesystem,
--   so @file.c@, @dir\/file.c@, @dir1\/dir2\/file.c@ and @\/path\/to\/file.c@ all match,
--   but @file.h@ and @dir\/file.h@ don't.
--
-- * @dir\/*\/*@ matches all files one level below @dir@, so @dir\/one\/file.c@ and
--   @dir\/two\/file.h@ match, but @file.c@, @one\/dir\/file.c@, @dir\/file.h@
--   and @dir\/one\/two\/file.c@ don't.
--
--   Patterns with constructs such as @foo\/..\/bar@ will never match
--   normalised 'FilePath' values, so are unlikely to be correct.
(?==) :: FilePattern -> FilePath -> Bool
(?==) = matchBoolWith . parseLegacy


-- | Like 'System.FilePattern.match' but also deals with @\/\/@ patterns.
match :: FilePattern -> FilePath -> Maybe [String]
match = matchWith . parseLegacy

---------------------------------------------------------------------
-- MULTIPATTERN COMPATIBLE SUBSTITUTIONS

-- | Like 'System.FilePattern.simple' but also deals with @\/\/@ patterns.
simple :: FilePattern -> Bool
simple = simpleWith . parseLegacy

-- | Like 'System.FilePattern.compatible' but also deals with @\/\/@ patterns.
compatible :: [FilePattern] -> Bool
compatible = compatibleWith . map parseLegacy

-- | Like 'System.FilePattern.substitute' but also deals with @\/\/@ patterns.
substitute :: Partial => [String] -> FilePattern -> FilePath
substitute xs x = substituteWith "System.FilePattern.Legacy.substitute" xs (x, parseLegacy x)


---------------------------------------------------------------------
-- EFFICIENT PATH WALKING

-- | Like 'System.FilePattern.walk' but also deals with @\/\/@ patterns.
walk :: [FilePattern] -> (Bool, Maybe Walk)
walk = walkWith . map parseLegacy

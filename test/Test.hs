{-# LANGUAGE RecordWildCards #-}

module Main(main) where

import Control.Monad.Extra
import Data.List.Extra
import qualified Test.Util as T
import Data.Maybe
import System.FilePattern as FilePattern
import System.FilePath(isPathSeparator)
import System.IO.Unsafe
import Test.QuickCheck
import Test.Cases

---------------------------------------------------------------------
-- TEST UTILITIES

assertBool :: Bool -> String -> [String] -> IO ()
assertBool b msg fields = unless b $ error $ unlines $
    ("ASSERTION FAILED: " ++ msg) : fields

(#=) :: Show a => String -> a -> String
(#=) a b = a ++ ": " ++ show b


newtype ArbPattern = ArbPattern FilePattern deriving (Show,Eq)
newtype ArbPath    = ArbPath    FilePath    deriving (Show,Eq)

-- Since / and * are the only "interesting" elements, just add ab to round out the set

instance Arbitrary ArbPattern where
    arbitrary = fmap ArbPattern $ listOf $ elements "\\/*ab."
    shrink (ArbPattern x) = map ArbPattern $ shrinkList (\x -> ['/' | x == '\\']) x

instance Arbitrary ArbPath where
    arbitrary = fmap ArbPath $ listOf $ elements "\\/ab."
    shrink (ArbPath x) = map ArbPath $ shrinkList (\x -> ['/' | x == '\\']) x


{-
-- | Write 'matchBool' in terms of 'walker'.
walkerMatch :: Switch -> FilePattern -> FilePath -> Bool
walkerMatch Switch{..} a b = if null b2 then empty else maybe False (f b2) w
    where
        b2 = filter (/= ".") $ split isPathSeparator b
        (empty, w) = walk [a]

        f [x]    (Walk op) = fst $ op x
        f [x]    (WalkTo (file, dir)) = x `elem` file
        f (x:xs) (Walk op) = maybe False (f xs) $ snd $ op x
        f (x:xs) (WalkTo (file, dir)) | Just w <- lookup x dir = f xs w
        f _ _ = False

showWalk :: Walk -> String
showWalk (Walk _) = "Walk _"
showWalk (WalkTo (p,xs)) = "WalkTo " ++ pair (show p) (list [pair (show a) (showWalk b) | (a,b) <- xs])
    where
        pair a b = "(" ++ a ++ "," ++ b ++ ")"
        list xs = "[" ++ intercalate "," xs ++ "]"
-}

---------------------------------------------------------------------
-- DRIVER

main :: IO ()
main = do
    putStr "Testing..."
    testCases
    T.TestData{..} <- T.unsafeTestData
    putStrLn $ "Specific test cases passed (" ++ show testDataCases ++ ")"
    -- when False $ dot $ testWalk s
    putStr " "
    testProperties $ testDataPats ++ testDataPaths


{-
testWalk :: Switch -> IO ()
testWalk Switch{..} = do
    let shw (a, b) = "(" ++ show a ++ "," ++ maybe "Nothing" ((++) "Just " . showWalk) b ++ ")"
    let both p w = assertBool (shw res == shw w) "walk" ["Pattern" #= p, "Expected" #= shw w, "Got" #= shw res]
            where res = walk p
    let walk_ = Walk undefined

    both ["*.xml"] (False, Just walk_)
    both ["//*.xml"] (False, Just $ WalkTo ([], [("",walk_)]))
    both ["**/*.xml"] (False, Just walk_)
    both ["foo//*.xml"] (False, Just $ WalkTo ([], [("foo",walk_)]))
    both ["foo/**/*.xml"] (False, Just $ WalkTo ([], [("foo",walk_)]))
    both ["foo/bar/*.xml"] (False, Just $ WalkTo ([], [("foo",WalkTo ([],[("bar",walk_)]))]))
    both ["a","b/c"] (False, Just $ WalkTo (["a"],[("b",WalkTo (["c"],[]))]))
    let (False, Just (Walk f)) = walk ["*/bar/*.xml"]
        shw2 (b, mw) = (b, maybe "" showWalk mw)
    assertBool (shw2 (f "foo") == shw2 (False, Just $ WalkTo ([],[("bar",walk_)]))) "walk inner" []
    both ["bar/*.xml","baz//*.c"] (False, Just $ WalkTo ([],[("bar",walk_),("baz",walk_)]))
    both ["bar/*.xml","baz/**/*.c"] (False, Just $ WalkTo ([],[("bar",walk_),("baz",walk_)]))
    both [] (False, Nothing)
    both [""] (True, Just $ WalkTo ([""], []))
    both ["//"] (False, Just $ WalkTo ([], [("",WalkTo ([""],[]))]))
    both ["**"] (True, Just walk_)
-}

testProperties :: [String] -> IO ()
testProperties xs = do
    forM_ xs $ \x -> forM_ xs $ \y -> prop x y
    Success{} <- quickCheckWithResult stdArgs{maxSuccess=10000} $ \(ArbPattern p) (ArbPath x) ->
        (if p ?== x then label "match" else property) $ unsafePerformIO $ prop p x >> return True
    return ()
    where
        prop :: FilePattern -> FilePath -> IO ()
        prop pat file = do
            let b = pat ?== file
            let fields = ["Pattern" #= pat, "File" #= file, "?==" #= b]
            let res = FilePattern.match pat file in assertBool (b == isJust res) "match" $ fields ++ ["match" #= res]
            -- when False $ let res = walkerMatch switch pat file in assertBool (b == res) "walker" $ fields ++ ["walker" #= res]
            let norm = (\x -> if null x then [""] else x) . filter (/= ".") . split isPathSeparator
            when b $ let res = substitute pat (fromJust $ FilePattern.match pat file) in
                assertBool (norm res == norm file) "substitute" $ fields ++ ["Match" #= FilePattern.match pat file, "Got" #= res, "Input (norm)" #= norm file, "Got (norm)" #= norm res]

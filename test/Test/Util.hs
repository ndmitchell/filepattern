
module Test.Util(
    match, matchY, matchN,
    simple,
    arity,
    substitute, substituteErr,
    unsafeTestData
    ) where

import Control.Exception.Extra
import Control.Monad.Extra
import Data.List.Extra
import Data.IORef
import System.FilePattern(FilePattern)
import qualified System.FilePattern as FP
import System.IO.Unsafe


---------------------------------------------------------------------
-- COLLECT TEST DATA

{-# NOINLINE testData #-}
testData :: IORef ([FilePattern], [FilePath])
testData = unsafePerformIO $ newIORef ([],[])

addTestData :: [FilePattern] -> [FilePath] -> IO ()
addTestData x1 y1 = atomicModifyIORef' testData $ \(x2,y2) -> ((reverse x1 ++ x2, reverse y1 ++ y2), ())

unsafeTestData :: IO ([FilePattern], [FilePath])
unsafeTestData = atomicModifyIORef' testData $ \(x,y) -> (([],[]), (nubSort $ reverse x, nubSort $ reverse y))


---------------------------------------------------------------------
-- TEST UTILITIES

assertBool :: Partial => Bool -> String -> [String] -> IO ()
assertBool b msg fields = unless b $ error $ unlines $
    ("ASSERTION FAILED: " ++ msg) : fields

assertException :: (Show a, Partial) => IO a -> [String] -> String -> [String] -> IO ()
assertException a parts msg fields = do
    res <- try_ $ evaluate . length . show =<< a
    case res of
        Left e -> assertBool (all (`isInfixOf` show e) parts) msg $ ["Expected" #= parts, "Got" #= e] ++ fields
        Right _ -> assertBool False msg $ ["Expected" #= parts, "Got" #= "<No exception>"] ++ fields

(#=) :: Show a => String -> a -> String
(#=) a b = a ++ ": " ++ show b


---------------------------------------------------------------------
-- TEST WRAPPERS

match :: Partial => FilePattern -> FilePath -> Maybe [String] -> IO ()
match pat path want = do
    addTestData [pat] [path]
    let got = FP.match pat path
    assertBool (want == got) "match" ["Pattern" #= pat, "Path" #= path, "Expected" #= want, "Got" #= got]

matchY :: Partial => FilePattern -> FilePath -> [String] -> IO ()
matchY pat path xs = match pat path $ Just xs

matchN :: Partial => FilePattern -> FilePath -> IO ()
matchN pat path = match pat path Nothing


simple :: Partial => FilePattern -> Bool -> IO ()
simple pat want = do
    addTestData [pat] []
    let got = FP.simple pat
    assertBool (want == got) "simple" ["Pattern" #= pat, "Expected" #= want, "Got" #= got]

arity :: Partial => FilePattern -> Int -> IO ()
arity pat want = do
    addTestData [pat] []
    let got = FP.arity pat
    assertBool (want == got) "arity" ["Pattern" #= pat, "Expected" #= want, "Got" #= got]

substitute :: Partial => FilePattern -> [String] -> FilePath -> IO ()
substitute pat parts want = do
    addTestData [pat] [want]
    let got = FP.substitute pat parts
    assertBool (want == got) "substitute" ["Pattern" #= pat, "Parts" #= parts, "Expected" #= want, "Got" #= got]

substituteErr :: Partial => FilePattern -> [String] -> [String] -> IO ()
substituteErr pat parts want = do
    addTestData [pat] []
    assertException (return $ FP.substitute pat parts) want "substitute" ["Pattern" #= pat, "Parts" #= parts]

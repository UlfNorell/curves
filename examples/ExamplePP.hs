{-# LANGUAGE PatternGuards #-}

import Control.Arrow (first, second, (***), (&&&))
import Control.Applicative
import Data.List
import System.Process
import System.Environment

-- Syntax?
--    %eval{ code }
--    %exec{ code }
-- Sure why not.
--
-- Indent of multi-line results? PrettyPrint style I think.

-- Let's do this naively first.

-- Need to be able to invoke ghci and get the result. ghc -e does that.

-- How to avoid code duplication. In my case I want to write down some image
-- code and have it expand to the code and a link to a newly created image.
--
-- Not obvious that hscolour can deal with anything other than top-level
-- declarations so maybe not a problem. Can just do
--
-- > ex1 = imageCode
-- %eval{ createImage_ ex1 }

dropLastNewLine = reverse . d . reverse
  where d ('\n':s) = s
        d s        = s

runCode :: FilePath -> String -> IO String
runCode m s = dropLastNewLine <$> readProcess "ghc" [m, "-e", s] ""

dropPrefix :: Eq a => [a] -> [a] -> Maybe [a]
dropPrefix p s | isPrefixOf p s = Just (drop (length p) s)
               | otherwise      = Nothing

matchBrace 0 ('}':s) = ("", s)
matchBrace n (c:s)   = first (c:) $ matchBrace (count c + n) s
  where
    count '{' = 1
    count '}' = -1
    count _   = 0

data Token = Eval String
           | Exec String
           | Print String
           | Chunk String

lexString :: String -> [Token]
lexString "" = []
lexString s@(c:s')
  | Just s' <- dropPrefix "%{"  s = go Eval s'
  | Just s' <- dropPrefix "%#{" s = go Print s'
  | Just s' <- dropPrefix "%!{" s = go Exec s'
  | otherwise = char c $ lexString s'
  where
    go mkTok s = mkTok code : lexString rest
      where (code, rest) = matchBrace 0 s

    char c (Chunk s : ts) = Chunk (c : s) : ts
    char c ts             = Chunk [c] : ts

runTokens :: FilePath -> [Token] -> IO String
runTokens file ts = spliceResults ts . splitResults
                <$> runCode file codeToRun
  where
    magicNumber = "zgiyttxhvcoshdcwkwpf"
    sep         = "; putStr " ++ show magicNumber ++ "; "
    codeToRun = "do {" ++ intercalate sep (concatMap action ts) ++ "}"

    action Chunk{}      = []
    action (Print code) = ["print (" ++ code ++ ")"]
    action (Eval code)  = ["(" ++ code ++ ")"]
    action (Exec code)  = ["(" ++ code ++ ")"]

    splitResults :: String -> [String]
    splitResults [] = []
    splitResults s@(c:s')
      | Just s' <- dropPrefix magicNumber s = [] : splitResults s'
      | otherwise = char c $ splitResults s'
      where
        char c (s:ss) = (c:s):ss
        char c []     = [[c]]

    spliceResults (Chunk s : ts) rs = s ++ spliceResults ts rs
    spliceResults (_ : ts) (r : rs) = r ++ spliceResults ts rs
    spliceResults [] []             = ""

ppFile :: FilePath -> IO String
ppFile file = runTokens file . lexString =<< readFile file

main = do
  args <- getArgs
  case args of
    [file] -> putStr =<< ppFile file
    _ -> do
      name <- getProgName
      putStrLn $ "Usage: " ++ name ++ " FILE"


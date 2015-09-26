{- | A module implementing regexp-based split and substitute.

>>> substituteCompile "(a+)" "lapin" "'\\1'"
Right "l'a'pin"

>>> splitCompile "\\d" "a1b2c3"
Right ["a","b","c"]
-}
module Text.Regex.PCRE.ByteString.Utils
    ( -- * Perl-like utility functions
      substitute
    , split
    , substituteCompile
    , splitCompile
    -- * Re-exports from "Text.Regex.PCRE.ByteString"
    , Regex
    , CompOption
    , ExecOption
    , compBlank
    , execBlank
    -- * Pure version of the functions, using 'unsafePerformIO'
    , compile'
    , execute'
    , substitute'
    , split'
    , substituteCompile'
    , splitCompile'
    ) where

import Text.Regex.PCRE.ByteString
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Except
import qualified Data.Vector as V
import Data.Attoparsec.ByteString.Char8 as A
import Control.Applicative
import Data.Char (digitToInt)
import System.IO.Unsafe
import qualified Data.Array as A

{-| Substitutes values matched by a `Regex`. References can be used.

It doesn't support anything else than global substitution for now ..

-}
substitute :: Regex             -- ^ The regular expression, taken from a call to `compile`
           -> BS.ByteString     -- ^ The source string
           -> BS.ByteString     -- ^ The replacement string
           -> IO (Either String BS.ByteString)
substitute regexp srcstring repla = runExceptT $ do
    parsedReplacement <- case parseOnly repparser repla of
                             Right y -> return y
                             Left rr -> throwError (rr ++ " when parsing the replacement string")
    (matches, captures) <- getMatches regexp srcstring V.empty
    let !replaceString = applyCaptures firstMatch parsedReplacement captures
        applyReplacement :: RegexpSplit BS.ByteString -> BS.ByteString
        applyReplacement (Unmatched x) = x
        applyReplacement (Matched _) = replaceString
        firstMatch = case filter isMatched matches of
                         (Matched x:_) -> x
                         _     -> ""
    return $! BS.concat $! map applyReplacement matches

-- Transforms the parsed replacement and the vector of captured stuff into
-- the destination ByteString.
applyCaptures :: BS.ByteString -> [Replacement] -> V.Vector BS.ByteString -> BS.ByteString
applyCaptures firstmatch repl capt = BS.concat (map applyCaptures' repl)
    where
        applyCaptures' :: Replacement -> BS.ByteString
        applyCaptures' WholeMatch = firstmatch
        applyCaptures' (RawReplacement r) = r
        applyCaptures' (IndexedReplacement idx) = if V.length capt < idx
                                                      then ""
                                                      else capt V.! (idx-1)

-- | Splits strings, using a `Regex` as delimiter.
split :: Regex  -- ^ The regular expression, taken from a call to `compile`
      -> BS.ByteString -- ^ The source string
      -> IO (Either String [BS.ByteString])
split regexp srcstring = fmap (removeEmptyLeft . regexpUnmatched . fst) <$> runExceptT (getMatches regexp srcstring V.empty)
    where
        removeEmptyLeft = reverse . dropWhile BS.null . reverse

-- | A pure version of 'substitute', using unsafePerformIO.
substitute' :: Regex             -- ^ The regular expression, taken from a call to `compile`
            -> BS.ByteString     -- ^ The source string
            -> BS.ByteString     -- ^ The replacement string
            -> Either String BS.ByteString
substitute' regexp srcstring repla = unsafePerformIO (substitute regexp srcstring repla)

-- | A pure version of 'split', using unsafePerformIO.
split' :: Regex  -- ^ The regular expression, taken from a call to `compile`
       -> BS.ByteString -- ^ The source string
       -> Either String [BS.ByteString]
split' regexp srcstring = unsafePerformIO (split regexp srcstring)

data RegexpSplit a = Matched a
                   | Unmatched a
                   deriving (Show, Eq, Ord)

instance Functor RegexpSplit where
    fmap f (Matched x)   = Matched (f x)
    fmap f (Unmatched x) = Unmatched (f x)

regexpAll :: [RegexpSplit a] -> [a]
regexpAll = map unreg
    where
        unreg ( Matched x   ) = x
        unreg ( Unmatched x ) = x

isMatched :: RegexpSplit a -> Bool
isMatched (Matched _) = True
isMatched _ = False

regexpUnmatched :: [RegexpSplit a] -> [a]
regexpUnmatched = regexpAll . filter (not . isMatched)

getMatches :: Regex -> BS.ByteString -> V.Vector BS.ByteString -> ExceptT String IO ([RegexpSplit BS.ByteString], V.Vector BS.ByteString)
getMatches _ "" curcaptures  = return ([], curcaptures)
getMatches creg src curcaptures = do
    x <- liftIO $ regexec creg src
    case x of
        Left (rcode, rerror) -> throwError ("Regexp application error: " ++ rerror ++ "(" ++ show rcode ++ ")")
        Right Nothing -> return ([Unmatched src], curcaptures)

        -- Now this is a trick, I don't know exactly why this happens, but this happens with empty regexps. We are going to cheat here
        Right (Just ("","",rm,_)) -> return (map (Unmatched . BS.singleton) (BS.unpack rm), curcaptures)

        Right (Just (before,current,remaining,captures)) -> do
            (remain, nextcaptures) <- getMatches creg remaining (curcaptures V.++ V.fromList captures)
            return (Unmatched before : Matched current : remain, nextcaptures)


data Replacement = RawReplacement BS.ByteString
                 | IndexedReplacement Int
                 | WholeMatch
                 deriving (Show)

repparser :: Parser [Replacement]
repparser = many replacement <* endOfInput

replacement :: Parser Replacement
replacement = fmap RawReplacement rawData <|> escapedThing

rawData :: Parser BS.ByteString
rawData = takeWhile1 (/= '\\')

escapedThing :: Parser Replacement
escapedThing = do
    void (char '\\')
    let ac = do
            n <- anyChar
            r <- rawData
            return $ BS.cons n r
        toReplacement 0 = WholeMatch
        toReplacement n = IndexedReplacement n
    fmap (toReplacement . digitToInt) digit <|> fmap (RawReplacement . BS.cons '\\') ac

-- | Compiles the regular expression (using default options) and `substitute`s
substituteCompile :: BS.ByteString     -- ^ The regular expression
                  -> BS.ByteString     -- ^ The source string
                  -> BS.ByteString     -- ^ The replacement string
                  -> IO (Either String BS.ByteString)
substituteCompile regexp srcstring repla = do
    re <- compile compBlank execBlank regexp
    case re of
        Right cre -> substitute cre srcstring repla
        Left rr   -> return $ Left $ "Regexp compilation failed: " ++ show rr

-- | Compiles the regular expression (using default options) and `split`s.
splitCompile :: BS.ByteString -- ^ The regular expression
             -> BS.ByteString -- ^ The source string
             -> IO (Either String [BS.ByteString])
splitCompile regexp srcstring = do
    re <- compile compBlank execBlank regexp
    case re of
        Right cre -> split cre srcstring
        Left rr   -> return $ Left $ "Regexp compilation failed: " ++ show rr

-- | A pure version of 'compile', using unsafePerformIO.
compile' :: CompOption -> ExecOption -> BS.ByteString -> Either (MatchOffset, String) Regex
compile' co eo s = unsafePerformIO (compile co eo s)

-- | A pure version of 'execute', using unsafePerformIO.
execute' :: Regex -> BS.ByteString -> Either WrapError (Maybe (A.Array Int (MatchOffset, MatchLength)))
execute' r s = unsafePerformIO (execute r s)

-- | A pure version of 'substituteCompile', using unsafePerformIO.
substituteCompile' :: BS.ByteString     -- ^ The regular expression
                   -> BS.ByteString     -- ^ The source string
                   -> BS.ByteString     -- ^ The replacement string
                   -> Either String BS.ByteString
substituteCompile' regexp srcstring repla = unsafePerformIO (substituteCompile regexp srcstring repla)

-- | A pure version of 'splitCompile', using unsafePerformIO.
splitCompile' :: BS.ByteString -- ^ The regular expression
              -> BS.ByteString -- ^ The source string
              -> Either String [BS.ByteString]
splitCompile' regexp srcstring = unsafePerformIO (splitCompile regexp srcstring)

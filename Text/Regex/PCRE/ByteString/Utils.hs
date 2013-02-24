module Text.Regex.PCRE.ByteString.Utils
    ( substitute
    , split
    , substituteCompile
    , splitCompile
    ) where

import Text.Regex.PCRE.ByteString
import qualified Data.ByteString.Char8 as BS
import Control.Monad.Error
import qualified Data.Vector as V
import Data.Attoparsec.ByteString.Char8
import Control.Applicative

{-| Substitutes values matched by a `Regex`. References can be used:

> sdqqsd

-}
substitute :: Regex             -- ^ The regular expression, taken from a call to `compile`
           -> BS.ByteString     -- ^ The source string
           -> BS.ByteString     -- ^ The replacement string
           -> IO (Either String BS.ByteString)
substitute regexp srcstring repla = runErrorT $ do
    let check x = case x of
                      Right y -> return y
                      Left rr -> throwError rr
    parsedReplacement <- check (parseOnly repparser repla)
    (matches, captures) <- getMatches regexp srcstring V.empty
    let !replaceString = applyCaptures parsedReplacement captures
        applyReplacement :: RegexpSplit BS.ByteString -> BS.ByteString
        applyReplacement (Unmatched x) = x
        applyReplacement (Matched _) = replaceString
    return $! BS.concat $! map applyReplacement matches

-- Transforms the parsed replacement and the vector of captured stuff into
-- the destination ByteString.
applyCaptures :: [Replacement] -> V.Vector BS.ByteString -> BS.ByteString
applyCaptures repl capt = BS.concat (map applyCaptures' repl)
    where
        applyCaptures' :: Replacement -> BS.ByteString
        applyCaptures' (RawReplacement r) = r
        applyCaptures' (IndexedReplacement idx) = if V.length capt < idx
                                                      then ""
                                                      else capt V.! (idx-1)

-- | Splits strings, using a `Regex` as delimiter.
split :: Regex  -- ^ The regular expression, taken from a call to `compile`
      -> BS.ByteString -- ^ The source string
      -> IO (Either String [BS.ByteString])
split regexp srcstring = fmap (either Left (Right . removeEmptyLeft . regexpUnmatched . fst)) $ runErrorT (getMatches regexp srcstring V.empty)
    where
        removeEmptyLeft = reverse . dropWhile BS.null . reverse

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

getMatches :: Regex -> BS.ByteString -> V.Vector BS.ByteString -> ErrorT String IO ([RegexpSplit BS.ByteString], V.Vector BS.ByteString)
getMatches _ "" curcaptures  = return ([], curcaptures)
getMatches creg src curcaptures = do
    x <- liftIO $ regexec creg src
    case x of
        Left (rcode, rerror) -> throwError ("Regexp application error: " ++ rerror ++ "(" ++ show rcode ++ ")")
        Right Nothing -> return ([Unmatched src], curcaptures)

        -- Now this is a trick, I don't know exactly why this happens, but this happens with empty regexps. We are going to cheat here
        Right (Just ("","",rm,_)) -> return (map (Unmatched . BS.singleton) (BS.unpack rm), curcaptures)

        Right (Just (before,current,remaining,captures)) -> do
            (remain, nextcaptures) <- getMatches creg remaining (curcaptures V.++ (V.fromList captures))
            return (Unmatched before : Matched current : remain, nextcaptures)


data Replacement = RawReplacement BS.ByteString
                 | IndexedReplacement Int
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
    fmap IndexedReplacement decimal <|> fmap (RawReplacement . BS.cons '\\') rawData


-- | Compiles the regular expression and `substitute`s
substituteCompile :: BS.ByteString     -- ^ The regular expression
                  -> CompOption        -- ^ Compilation options
                  -> BS.ByteString     -- ^ The source string
                  -> BS.ByteString     -- ^ The replacement string
                  -> IO (Either String BS.ByteString)
substituteCompile regexp comp srcstring repla = do
    re <- compile comp execBlank regexp
    case re of
        Right cre -> substitute cre srcstring repla
        Left rr   -> return $ Left $ "Regexp compilation failed: " ++ show rr

-- | Compiles the regular expression and `split`s.
splitCompile :: BS.ByteString -- ^ The regular expression
             -> CompOption        -- ^ Compilation options
             -> BS.ByteString -- ^ The source string
             -> IO (Either String [BS.ByteString])
splitCompile regexp comp srcstring = do
    re <- compile comp execBlank regexp
    case re of
        Right cre -> split cre srcstring
        Left rr   -> return $ Left $ "Regexp compilation failed: " ++ show rr

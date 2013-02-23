module Text.Regex.PCRE.ByteString.Utils
    ( substitute
    , split
    ) where

import Text.Regex.PCRE.ByteString
import qualified Data.ByteString as BS
import Control.Monad.Error

-- | Substitutes values matched by a "Regexp". References (such as \2) can be
-- used.
substitute :: Regex             -- ^ The regular expression, taken from a call to `compile`
           -> BS.ByteString     -- ^ The source string
           -> BS.ByteString     -- ^ The replacement string
           -> IO (Either String BS.ByteString)
substitute regexp srcstring replacement = runErrorT $ do
    (matches, captures) <- getMatches regexp srcstring []
    throwError (show (matches, captures, replacement))


-- | Splits strings, using a "Regexp" as delimiter.
split :: Regex  -- ^ The regular expression, taken from a call to `compile`
      -> BS.ByteString -- ^ The source string
      -> IO (Either String [BS.ByteString])
split regexp srcstring = fmap (either Left (Right . removeAllEmpty . regexpUnmatched . fst)) $ runErrorT (getMatches regexp srcstring [])
    where
        removeAllEmpty x = if all (=="") x
                               then []
                               else x

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

regexpMatched :: [RegexpSplit a] -> [a]
regexpMatched = regexpAll . filter isMatched

regexpUnmatched :: [RegexpSplit a] -> [a]
regexpUnmatched = regexpAll . filter (not . isMatched)

getMatches :: Regex -> BS.ByteString -> [BS.ByteString] -> ErrorT String IO ([RegexpSplit BS.ByteString], [BS.ByteString])
getMatches _ "" curcaptures  = return ([], curcaptures)
getMatches creg src curcaptures = do
    x <- liftIO $ regexec creg src
    case x of
        Left (rcode, rerror) -> throwError ("Regexp application error: " ++ rerror ++ "(" ++ show rcode ++ ")")
        Right Nothing -> return ([Unmatched src], curcaptures)
        Right (Just ("",_,"",_)) -> return ([], curcaptures)

        -- Now this is a trick, I don't know exactly why this happens, but this happens with empty regexps. We are going to cheat here
        Right (Just ("","",rm,_)) -> return (map (Unmatched . BS.singleton) (BS.unpack rm), curcaptures)

        Right (Just (before,current,remaining,captures)) -> do
            (remain, nextcaptures) <- getMatches creg remaining (curcaptures ++ captures)
            return (Unmatched before : Matched current : remain, nextcaptures)


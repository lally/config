{-# LANGUAGE OverloadedStrings #-}
module Recognize.BaseMatcher (loadConfig, matchTags,
                              tagString, MatcherSet(..)) where
import Control.Monad
import Data.Either
import Data.List
import Data.Maybe
import Support.Types
import qualified Control.Exception as E
import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DCT
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import qualified Text.Regex.PCRE.String as R
import qualified Data.Set as S
import qualified Data.Text as T

data MatcherImpl
      -- | A standard regex matcher.
    = RegexMatch { mcTag :: Tag, mcRegex :: R.Regex, mcRegexSpec :: String }
      -- | A plugin matcher.
    | Plugin { pPluginName :: String
             , pOperator :: WindowInfo -> IO ([Tag])
             , pParams :: DCT.Value
               -- ^ The params here were fed into the matcher at
               -- construction time.  We only keep them here for the
               -- Show instance.
             }

instance Show MatcherImpl where
    show RegexMatch { mcTag = tag, mcRegexSpec = regex } =
         (tagString tag) ++ "= regex(" ++ (show regex) ++ ")"
    show Plugin { pPluginName = plugin, pParams = params } =
        "PLUGIN " ++ plugin ++ ", Params: " ++ show params

data Matcher = Matcher { mClass :: String
                       , mDefault :: Bool
                       , mImpl :: MatcherImpl }
             deriving Show

type MatcherSet = M.Map String [Matcher]

-- | Data from the config file, used to build the MatcherSet
type MatcherConfig = HM.HashMap DCT.Name DCT.Value

tagString :: Tag -> String
tagString (Tag tag) = tag

pluginNames :: [T.Text]
pluginNames = [] -- ["_X_PROPS"]

eitherM :: Monad m => (a -> m c) -> (b -> m c) -> Either a b -> m c
eitherM leftFun rightFun (Left param) = leftFun param
eitherM leftFun rightFun (Right param) = rightFun param

-- | The key is in the format 'wm_class.tag', and the value is the
-- regex.  Monadic for FFI to regex library, and to report compile
-- errors.
makeRegexMatcher :: MatcherConfig -> (Bool, DCT.Name) -> IO (Maybe Matcher)
makeRegexMatcher config (runDefault, key) = do
      let path = map T.unpack $ T.splitOn "." key
          regexStr = maybe (DCT.String "") id $ HM.lookup key config
          regexText = maybe "" id $ DCT.convert regexStr
          processError (offset, err) = do
            putStrLn $ (show regexText) ++ ":" ++ (show offset) ++ ":: " ++ err
            return Nothing
          processRegex regex = do
            let impl = RegexMatch { mcTag = Tag (path !! 1)
                                  , mcRegex = regex
                                  , mcRegexSpec = regexText}
            return $ Just Matcher { mClass = path !! 0
                                  , mDefault = runDefault
                                  , mImpl = impl }
      compileResult <- R.compile R.compBlank R.execBlank regexText
      maybeMatcher <- eitherM processError processRegex compileResult
      return maybeMatcher

makePluginMatcher :: MatcherConfig -> (Bool, DCT.Name) -> IO (Maybe Matcher)
makePluginMatcher = undefined

buildMatchersForClass :: MatcherConfig -> DCT.Name -> IO ([Matcher])
buildMatchersForClass config cls = do
  let
    clsPrefix = T.append cls "."
    clsPrefixLen = T.length clsPrefix
    allMatchKeys = filter (T.isPrefixOf clsPrefix) $ HM.keys config
    matchedDefaultOptions =
      map (maybe True id) $
      map (maybe (Just True) DCT.convert) $ map ((flip HM.lookup) config) $
      map (\n -> T.append n ".option_default") allMatchKeys
    isPluginKey (dflt, name) =
      if strippedName `elem` pluginNames
         then Right (dflt, name)
         else Left (dflt, name)
      where strippedName = T.takeWhile (/= '.') name
    (regexMatcherKeys, pluginMatcherKeys) =
      partitionEithers $ map isPluginKey $ zip matchedDefaultOptions allMatchKeys

  regexMatchers <- mapM (makeRegexMatcher config) regexMatcherKeys
  pluginMatchers <- mapM (makePluginMatcher config) pluginMatcherKeys
  return $ catMaybes $ concat [regexMatchers, pluginMatchers]

buildMatchers :: DCT.Config -> IO (MatcherSet)
buildMatchers config = do
  kvmap <- DC.getMap config
  let
    classNames = nub $ map (T.takeWhile (/= '.')) $ HM.keys kvmap
  matchers <- mapM (buildMatchersForClass kvmap) classNames
  let
    matcherKVs = zip (map T.unpack classNames) matchers
  return $ M.fromList matcherKVs

concatMaybesIO :: Monad m => (a -> m (Maybe b)) -> [a] -> m ([b])
concatMaybesIO fun inputs = do
  let
    mapFun (x:xs) = do
      singleResult <- fun x
      remainingResults <- mapFun xs
      return $ singleResult : remainingResults
    mapFun [] = return []
  maybeValues <- mapFun inputs
  return $ catMaybes maybeValues

runMatcher :: WindowInfo -> Matcher -> IO ([Tag])
runMatcher winInfo Matcher { mImpl = impl } = do
  let title = maybe [] id $ M.lookup "WM_NAME" winInfo
      funTrue x = True
      funFalse x = False
  case impl of
      (RegexMatch { mcTag = tag, mcRegex = regex }) ->
        do regexMatches <- mapM (R.execute regex) title
           let mapResult val =
                 either funFalse (maybe False funTrue) val
           if any mapResult regexMatches
              then return [tag]
              else return []
      (Plugin { pOperator = op }) ->
        op winInfo

concatMapM fun list = liftM concat (mapM fun list)

matchTags :: MatcherSet -> WindowInfo -> IO ([Tag])
matchTags matchers wininfo = do
  let wm_classes = maybe [] id $ M.lookup "WM_CLASS" wininfo
      matchingConfigs :: [Matcher]
      matchingConfigs = concat $ catMaybes $
                        map ((flip  M.lookup) matchers) wm_classes
      defaultMatcher = M.lookup "default" matchers
      xm_defaults = map (\cls -> cls ++ ".option_default") wm_classes
      defaultsForAlwaysExcludedInMatches :: Bool
      defaultsForAlwaysExcludedInMatches =
          all mDefault $ concat $ catMaybes $
          map ((flip M.lookup) matchers) xm_defaults
      hasDefault = isJust defaultMatcher
      addDefault = hasDefault && (((length matchingConfigs) == 0) ||
                   (not $ defaultsForAlwaysExcludedInMatches))

      configsToTry = if addDefault
                       then matchingConfigs ++ fromJust defaultMatcher
                       else matchingConfigs
  tags <- mapM (runMatcher wininfo) configsToTry
  return $ nub $ concat tags

loadConfig :: IO (MatcherSet)
loadConfig = do
  let failHandler e = do let err = show (e :: E.SomeException)
                         putStrLn ("Warning: Failed to load config file: " ++ err)
                         return DC.empty
  config <- E.catch (DC.load [ DCT.Optional "$(HOME)/.xmonadrc.cfg"
                             , DCT.Optional "$(HOME)/config/xmonadrc.cfg"
                             ])
                    failHandler
  matchers <- buildMatchers config
  putStrLn $ "Loaded Matchers " ++ (show matchers)
  return matchers

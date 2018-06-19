{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE NamedFieldPuns #-}
module Main where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.Trans.State.Lazy
import           Data.Aeson
import           Data.Either
import           Data.List
import qualified Data.Map                       as Map
import           Data.Maybe
import           Data.Ord
import qualified Data.Set                       as Set
import           Data.Time.Clock
import           GHC.Generics
import           Parser
import           System.Directory
import           System.FilePath.Posix
import           System.Random
import           System.Random.Shuffle

type SaveData = Map.Map String Option

data Option = Option {
    name     :: String
  , tries    :: Double
  , reward   :: Double
  , finished :: Bool
} deriving (Generic, Show)
instance ToJSON Option
instance FromJSON Option
instance Eq Option where
  x == y = name x == name y
newOption name = Option{
      name = name
    , tries = 0
    , reward = 0
    , finished = False
  }

main :: IO ()
main = do
  args <- runParser
  saveData <- fromMaybe newSaveData
              <$> fromRight Nothing
              <$> ( (try $ decodeFileStrict =<< getSaveFilePath)
                    :: IO (Either SomeException (Maybe SaveData))
                  )
  let saveData' = applyArgs args saveData
  if not (Map.null saveData')
    then putStrLn $ name $ getPick saveData'
    else putStrLn $ "Empty."
  save saveData'


newSaveData :: SaveData
newSaveData = Map.empty

getSaveFilePath :: IO FilePath
getSaveFilePath = do
  xdgDirectory <- getXdgDirectory XdgData ""
  createDirectoryIfMissing False (xdgDirectory </> appName)
  eitherResult <- try (getXdgDirectory XdgData appName) :: IO (Either SomeException FilePath)
  case eitherResult of
    Left exception -> error $ "Error: " ++ show exception
    Right value    -> return $ flip combine saveFileName value

applyArgs :: Args -> SaveData -> SaveData
applyArgs args saveData =
  case args of
    Add name    -> Map.insert name (newOption name) saveData
    Rate rating -> Map.insert (name topPick) (rate rating topPick) saveData
    Finish      -> Map.insert (name topPick) topPick{finished = True} saveData
    Skip        -> Map.insert (name topPick) (applySkip saveData topPick) saveData
    Show        -> saveData
  where topPick = getPick saveData
        rate rating option = option{reward = reward topPick + rating,
                                    tries = tries topPick + 1}

getPick :: SaveData -> Option
getPick options
  | Map.null options = error "[getPick] List is empty."
  | otherwise = maximumBy (comparing $ ucb $ sumTries options) <$> filter active $ shuffledOptions
  where
    gen = mkStdGen 879833250
    optionsList = Map.elems options
    shuffledOptions = shuffle' optionsList (length optionsList) gen

ucb :: Double -> Option -> Double
ucb tries' Option{reward, tries}
        | tries > 0 = reward / tries + sqrt (3 / 2) * log tries' / tries
        | otherwise = 0.25 / 2 + sqrt (3 / 2) * log tries' / 2

sumTries :: SaveData -> Double
sumTries saveData = sum $ map tries $ Map.elems saveData

active :: Option -> Bool
active Option{finished} = not finished

appName :: String
appName = "apick"

save :: SaveData -> IO ()
save saveData = flip encodeFile saveData =<< getSaveFilePath

applySkip :: SaveData -> Option -> Option
applySkip saveData option = until lower (applyRating 0) option
  where
    lower x = (getPick $ Map.insert (name x) x saveData) /= option
    tries' = sumTries saveData
    applyRating x y@Option{reward, tries} = y{reward = reward + x, tries = tries + 1}

saveFileName :: FilePath
saveFileName = appName ++ ".json"

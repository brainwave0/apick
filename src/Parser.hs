module Parser where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Args =
    Add String
  | Rate Double
  | Finish
  | Skip
  | Show

runParser :: IO Args
runParser = execParser parserWithInfo

parserWithInfo :: ParserInfo Args
parserWithInfo = info (parser <**> helper) (
       fullDesc
    <> progDesc "Recommend an activity."
    <> header "apick - An activity recommender for bored people."
  )

parser :: Parser Args
parser = subparser $
       (command "add" $ info addParser $ progDesc "Add an activity to the list.")
    <> (command "rate" $ info rateParser $ progDesc "Apply a rating to the activity.")
    <> (command "finish" $ info (pure Finish) $ progDesc "Remove the activity from recommendations.")
    <> (command "skip" $ info (pure Skip) $ progDesc "Recommend something else.")
    <> (command "show" $ info (pure Show) $ progDesc "Show the current recommendation.")

addParser :: Parser Args
addParser = Add <$> (argument str $ metavar "NAME")

rateParser :: Parser Args
rateParser = Rate <$> (argument auto $ metavar "RATING")

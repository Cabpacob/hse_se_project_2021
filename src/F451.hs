{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module F451 where

import Data.Char (isSpace, toLower)
import GHC.OldList (find)

newtype Score = Score Float deriving (Eq, Num, Show)

data ScoreConstraints = ScoreConstraints
  { minScore :: Score,
    noAnswerScore :: Score,
    maxScore :: Score
  }

oneZeroConstraints :: ScoreConstraints
oneZeroConstraints = ScoreConstraints (Score 0) (Score 0) (Score 1)

data Task q a = Task
  { question :: q,
    eval :: Maybe a -> Score
  }

evalAns :: (t -> Bool) -> ScoreConstraints -> Maybe t -> Score
evalAns  _        cs  Nothing = noAnswerScore cs
evalAns doesMatch cs (Just ans)
  | doesMatch ans = maxScore cs
  | otherwise     = minScore cs

type StringProcessor = String -> String
type StrProcList = [StringProcessor]

procTrim :: String -> String
procTrim = f . f
  where
    f = reverse . dropWhile isSpace

procToLower :: String -> String
procToLower = fmap toLower

processWithParams :: [StringProcessor] -> String -> String
processWithParams = foldl (.) id

doesMatchStrictString :: [StringProcessor] -> String -> String -> Bool
doesMatchStrictString params corr attempt = process corr == process attempt
  where
    process = processWithParams params

strictStringEval :: StrProcList -> String -> ScoreConstraints -> Maybe String -> Score
strictStringEval params correct = evalAns (doesMatchStrictString params correct)

taskStrict :: StrProcList -> q -> String -> ScoreConstraints -> Task q String
taskStrict params quest corr constr = Task quest (strictStringEval params corr constr)

taskStrictDefault :: q -> String -> ScoreConstraints -> Task q String
taskStrictDefault = taskStrict []

scoreTask :: Eq a => Task q a -> Maybe a -> Score
scoreTask (Task _ ev) = ev

scoreTasks :: Eq a => Task q a -> [Maybe a] -> [Score]
scoreTasks (Task _ ev) attList = ev <$> attList

newtype OneOf a = OneOf[a]

doesMatchOneOf :: StrProcList -> OneOf String -> String -> Bool
doesMatchOneOf params (OneOf ops) attempt = f attempt `elem` (f <$> ops)
  where
    f = processWithParams params

taskMultipleStrict :: StrProcList -> q -> OneOf String -> ScoreConstraints -> Task q String
taskMultipleStrict params quest corr constr = Task quest $ evalAns (doesMatchOneOf params corr) constr

doesMatchNothing :: StrProcList -> OneOf String -> String -> Bool
doesMatchNothing params (OneOf ops) attempt = f attempt `notElem` (f <$> ops)
  where
    f = processWithParams params

taskNonMultipleStrict :: StrProcList -> q -> OneOf String -> ScoreConstraints -> Task q String
taskNonMultipleStrict params quest corr constr = Task quest $ evalAns (doesMatchNothing params corr) constr


newtype Property a = Property (a -> Bool)

taskNumber :: q -> Property Int -> ScoreConstraints -> Task q Int
taskNumber quest (Property prop) constr = Task quest $ evalAns prop constr

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module F451 where

import Data.Char (isSpace, toLower)

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

data StrictEvalParams = StrictEvalParams {trim :: Bool, noCaseSense :: Bool}

evPs :: Bool -> Bool -> StrictEvalParams
evPs = StrictEvalParams

strictStringEval :: StrictEvalParams -> String -> ScoreConstraints -> Maybe String -> Score
strictStringEval _      _    cs  Nothing = noAnswerScore cs
strictStringEval params corr cs (Just ans)
  | process corr == process ans = maxScore cs
  | otherwise = minScore cs
  where
    process = processTrim . processSenseCase
    processTrim = if trim params then trimStr else idStr
    processSenseCase = if noCaseSense params then fmap toLower else idStr
    trimStr = f . f
      where
        f = reverse . dropWhile isSpace
    idStr s = s


taskStrict :: StrictEvalParams -> q -> String -> ScoreConstraints -> Task q String
taskStrict params quest corr constr = Task quest (strictStringEval params corr constr)

taskStrictDefault :: q -> String -> ScoreConstraints -> Task q String
taskStrictDefault = taskStrict (StrictEvalParams False False)


scoreTask :: Eq a => Task q a -> Maybe a -> Score
scoreTask (Task _ ev) = ev

scoreTasks :: Eq a => Task q a -> [Maybe a] -> [Score]
scoreTasks (Task _ ev) attList = ev <$> attList

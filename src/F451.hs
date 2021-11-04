{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module F451 where

import Data.Char (isSpace, toLower)

newtype Score = Score Float deriving (Eq, Num, Show)

data OneOf a = OneOf[a]

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

data StrictEvalParams = StrictEvalParams {trim :: Bool, noCaseSense :: Bool}

evPs :: Bool -> Bool -> StrictEvalParams
evPs = StrictEvalParams

doesMatchStrictString :: StrictEvalParams -> String -> String -> Bool
doesMatchStrictString params corr attempt = process corr == process attempt
  where
    process = processTrim . processSenseCase
    processTrim = if trim params then trimStr else idStr
    processSenseCase = if noCaseSense params then fmap toLower else idStr
    trimStr = f . f
      where
        f = reverse . dropWhile isSpace
    idStr s = s

strictStringEval :: StrictEvalParams -> String -> ScoreConstraints -> Maybe String -> Score
strictStringEval params correct = evalAns (doesMatchStrictString params correct)

taskStrict :: StrictEvalParams -> q -> String -> ScoreConstraints -> Task q String
taskStrict params quest corr constr = Task quest (strictStringEval params corr constr)

taskStrictDefault :: q -> String -> ScoreConstraints -> Task q String
taskStrictDefault = taskStrict (StrictEvalParams False False)

taskMultipleStrict :: StrictEvalParams -> q -> OneOf String -> ScoreConstraints -> Task q String
taskMultipleStrict params quest corr constr = undefined


scoreTask :: Eq a => Task q a -> Maybe a -> Score
scoreTask (Task _ ev) = ev

scoreTasks :: Eq a => Task q a -> [Maybe a] -> [Score]
scoreTasks (Task _ ev) attList = ev <$> attList

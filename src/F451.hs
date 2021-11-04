{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module F451 where

import Data.Char

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

getStrictEval :: Eq a => ScoreConstraints -> a -> Maybe a -> Score
getStrictEval scco answ Nothing = noAnswerScore scco
getStrictEval scco answ (Just attm)
  | attm == answ = maxScore scco
  | otherwise = minScore scco

getStrictTask :: Eq a => q -> a -> ScoreConstraints -> Task q a
getStrictTask ques answ scco = Task ques (getStrictEval scco answ)

scoreTask :: Eq a => Task q a -> Maybe a -> Score
scoreTask (Task _ ev) = ev

scoreTasks :: Eq a => Task q a -> [Maybe a] -> [Score]
scoreTasks (Task _ ev) attList = ev <$> attList

getNotCaseSensetiveEval :: ScoreConstraints -> [Char] -> Maybe [Char] -> Score
getNotCaseSensetiveEval scco answ Nothing = noAnswerScore scco
getNotCaseSensetiveEval scco answ (Just attm)
  | map toLower attm == map toLower answ = maxScore scco
  | otherwise                            = minScore scco

getTaskNonCaseSensetive :: q -> [Char] -> ScoreConstraints -> Task q [Char]
getTaskNonCaseSensetive ques answ scco = Task ques (getNotCaseSensetiveEval scco answ)

getTestTaskStrip = undefined
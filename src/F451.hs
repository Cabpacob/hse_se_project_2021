{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module F451 where

newtype Score = Score Float deriving (Eq, Num, Show)

data ScoreConstraints = ScoreConstraints
  { minScore :: Score,
    noAnswerScore :: Score,
    maxScore :: Score
  }

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

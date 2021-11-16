{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module F451 where

import Data.Char (isSpace, toLower)
import Data.Bool (bool)
import Data.List (length)
import Foreign.Marshal.Utils (fromBool)
import GHC.OldList (find)
import GHC.Float (int2Float)

newtype Score = Score Float deriving (Eq, Num, Show)

data ScoreConstraints = ScoreConstraints
  { minScore :: Score
  , noAnswerScore :: Score
  , maxScore :: Score
  }

oneZeroConstraints :: ScoreConstraints
oneZeroConstraints = ScoreConstraints (Score 0) (Score 0) (Score 1)

data Task q a = Task
  { question :: q
  , eval :: Maybe a -> Score
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

newtype OneOf a = OneOf [a]

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

taskQuiz :: q -> OneOf a -> Int -> ScoreConstraints -> Task q Int
taskQuiz quest vars corr constr = Task quest $ Score . maybe 0 (fromBool . (corr == ))

newtype AllOf a = AllOf [(a, Score)]

sumOfAns :: Eq a => ScoreConstraints -> AllOf a -> Maybe [a] -> Score
sumOfAns constr _ Nothing 
    = noAnswerScore constr
sumOfAns constr (AllOf vars) (Just as) 
    = Score $ sum (fmap (\ans -> sum $ fmap (\(var, Score scr) -> bool 0.0 scr (var == ans)) vars) as) / int2Float (length vars)

taskMultipleRequired :: Eq a => q -> AllOf a -> ScoreConstraints -> Task q [a]
taskMultipleRequired quest vars constr = Task quest $ sumOfAns constr vars

mistakes :: String -> String -> Int
mistakes [] [] = 0
mistakes (x:xs) (y:ys)
  | x == y    = mistakes xs ys
  | otherwise = 1 + mistakes xs ys

taskOneMistake :: q -> String -> ScoreConstraints -> Task q String
taskOneMistake quest corr constr = Task quest $ Score . maybe 0 (fromBool . (1 ==) . mistakes corr)

taskNMistakes :: q -> String -> Int -> ScoreConstraints -> Task q String
taskNMistakes quest corr num constr = Task quest $ Score . maybe 0 (fromBool . (num ==) . mistakes corr)

{- ax + b = c С автоматическим вычислением ответа -}
taskSimpleEquation :: q -> Float -> Float -> Float -> ScoreConstraints -> Task q Float
taskSimpleEquation quest a b c constr = Task quest $ Score . maybe 0 (\x -> fromBool (abs(a * x + b - c) < eps))
  where
    eps = 1e-9

taskNoMoreThanOneMistake :: q -> String -> ScoreConstraints -> Task q String
taskNoMoreThanOneMistake quest corr constr = Task quest $ Score . maybe 0 (fromBool . (1 >=) . mistakes corr)

taskQuadraticEquation :: q -> Float -> Float -> Float -> ScoreConstraints -> Task q (Float, Float)
taskQuadraticEquation quest a b c constr = Task quest $ Score . maybe 0 (\(x1, x2) -> fromBool ((abs(a * x1 * x1 + b * x1 + c) < eps) && (abs(a * x2 * x2 + b * x2 + c) < eps)))
  where
    eps = 1e-9

taskIntervalChecking :: q -> Int -> Int -> ScoreConstraints -> Task q Int
taskIntervalChecking quest a b constr = Task quest $ Score . maybe 0 (fromBool . (\x -> a <= x && x <= b))

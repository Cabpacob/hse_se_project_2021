module Main where
import F451
import Data.List (intercalate)

questions = [
    taskQuiz "How many countries still have the shilling as currency?" (OneOf ["1", "2", "4", "0"]) 2 oneZeroConstraints,
    taskQuiz "How many languages are written from right to left?" (OneOf ["12", "31", "23", "8"]) 0 oneZeroConstraints,
    taskQuiz "Demolition of the Berlin wall separating East and West Germany began in what year?" (OneOf ["1991", "1985", "1992", "1989"]) 3 oneZeroConstraints,
    taskQuiz "What is the rarest M&M color?" (OneOf ["Brown", "Yellow", "Blue", "Red"]) 0 oneZeroConstraints
    ]

newtype Student a = Student (String, [Maybe a])

students = [
    Student ("Lisa", [Just 2, Nothing , Just 3, Just 0]),
    Student ("David", [Just 2, Nothing , Nothing, Just 0]),
    Student ("Stan", [Just 1, Nothing , Just 2, Just 0]),
    Student ("Alex", [Nothing , Nothing , Just 3, Just 1])
    ]

score :: Score -> Float
score (Score s) = s

evalTaskStudent :: [Task q a] -> Student a -> Float
evalTaskStudent tasks (Student (_, answers)) = sum $ score . uncurry eval <$> zip tasks answers

printStudentReport :: [Task q a] -> Student a -> [Char]
printStudentReport tasks s@(Student (name, _)) = name ++ ": " ++ show (evalTaskStudent tasks s)

printQuestions :: [Task String a] -> String
printQuestions tasks = intercalate "\n" $ (\(n, t) -> show n ++ ". " ++ question t) <$> zip [1..] tasks

main :: IO ()
main = putStrLn $ unlines [
    "Today's test: ",
    printQuestions questions,
    "Results: ",
    intercalate "\n" (printStudentReport questions <$> students)
    ]

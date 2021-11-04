import F451
import Test.HUnit

testTask :: Eq a => Task q a -> [(Maybe a, Score)] -> Test
testTask task correctAndScore = TestCase (assertEqual "Task scores must match" attempts (scoreTasks task correct))
  where
    (correct, attempts) = unzip correctAndScore

testStrictTask =
  testTask
    (taskStrictDefault "How much?" "42" oneZeroConstraints)
    [ (Just "0", Score 0),
      (Just "42", Score 1),
      (Nothing, Score 0)
    ]

testTaskNonCaseSensetive =
  testTask
    (taskStrict [procToLower] "What???" "Haha" oneZeroConstraints)
    [ (Just "Haha", Score 1),
      (Just "hAhA", Score 1),
      (Just "Hahah", Score 0),
      (Nothing, Score 0)
    ]

testTaskStrip =
  testTask
    (taskStrict [procTrim] "Who killed the tzar?" " Kommunyaki" oneZeroConstraints)
    [ (Just "Kommunyaki", Score 1),
      (Just " Kommunyaki   ", Score 1),
      (Just "Nobody", Score 0),
      (Nothing, Score 0)
    ]

testTaskStripAndSenseCase =
  testTask
    (taskStrict [procTrim, procToLower] "What does the fox say?" " no one knows " oneZeroConstraints)
    [ (Just "woof", Score 0),
      (Just "tweet", Score 0),
      (Just "No one knows", Score 1),
      (Nothing, Score 0)
    ]

testTaskMultipleAnswers =
  testTask
    (taskMultipleStrict [procTrim, procToLower] "What is 2 + 2 * 2?" (OneOf ["4", "6"]) oneZeroConstraints)
    [ (Just "4", Score 1),
      (Just "6", Score 1),
      (Just " 4    ", Score 1),
      (Just " 6    ", Score 1),
      (Nothing, Score 0)
    ]

testTaskMultipleAnswersNonCaseSensetive =
  testTask
    (taskMultipleStrict [procTrim, procToLower] "xxx" (OneOf ["x", "B ", " t "]) oneZeroConstraints)
    [ (Just " X", Score 1),
      (Just " b", Score 1),
      (Just " tt", Score 0),
      (Just " 6    ", Score 0),
      (Nothing, Score 0)
    ]

testNotOneOf =
  testTask
    (taskNonMultipleStrict [procTrim, procToLower] "what can you do at home" (OneOf ["make fire", "jump from window"]) oneZeroConstraints)
    [ (Just " Make Fire", Score 0),
      (Just "jump from window", Score 0),
      (Just "eat", Score 1),
      (Just "sleep!", Score 1),
      (Nothing, Score 0)
    ]

testWriteEvenNumber =
  testTask
    (taskNumber "Write an even number" (Property even) oneZeroConstraints)
    [ (Just 228, Score 1),
      (Just 1337, Score 0),
      (Nothing, Score 0)
    ]

tests =
  TestList
    [ testStrictTask,
      testTaskNonCaseSensetive,
      testTaskStrip,
      testTaskStripAndSenseCase,
      testTaskMultipleAnswers,
      testTaskMultipleAnswersNonCaseSensetive,
      testNotOneOf,
      testWriteEvenNumber
    ]

main = runTestTT tests

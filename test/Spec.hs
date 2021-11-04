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
    (taskStrict (evPs False True) "What???" "Haha" oneZeroConstraints)
    [ (Just "Haha", Score 1),
      (Just "hAhA", Score 1),
      (Just "Hahah", Score 0),
      (Nothing, Score 0)
    ]

testTaskStrip =
  testTask
    (taskStrict (evPs True False) "Who killed the tzar?" " Kommunyaki" oneZeroConstraints)
    [ (Just "Kommunyaki", Score 1),
      (Just " Kommunyaki   ", Score 1),
      (Just "Nobody", Score 0),
      (Nothing, Score 0)
    ]

testTaskStripAndSenseCase =
  testTask
    (taskStrict (evPs True True) "What does the fox say?" " no one knows " oneZeroConstraints)
    [ (Just "woof", Score 0),
      (Just "tweet", Score 0),
      (Just "No one knows", Score 1),
      (Nothing, Score 0)
    ]

testTaskMultipleAnswers =
  testTask
    (taskMultipleStrict (evPs True True) "What is 2 + 2 * 2?" (OneOf ["4", "6"]) oneZeroConstraints)
    [ (Just "4", Score 1),
      (Just "6", Score 1),
      (Just " 4    ", Score 1),
      (Just " 6    ", Score 1),
      (Nothing, Score 0)
    ]

tests =
  TestList
    [ testStrictTask,
      testTaskNonCaseSensetive,
      testTaskStrip,
      testTaskStripAndSenseCase,
      testTaskMultipleAnswers
    ]

main = runTestTT tests

import F451
import Test.HUnit

testTask :: Eq a => Task q a -> [(Maybe a, Score)] -> Test
testTask task correctAndScore = TestCase (assertEqual "Task scores must match" (scoreTasks task correct) attempts)
  where
    (correct, attempts) = unzip correctAndScore

testStrictTask =
  testTask
    (getStrictTask "How much?" 42 oneZeroConstraints)
    [ (Just 0, Score 0),
      (Just 42, Score 1),
      (Nothing, Score 0)
    ]

testTaskNonCaseSensetive =
  testTask
    (getTaskNonCaseSensetive "What???" "Haha" oneZeroConstraints)
    [ (Just "Haha", Score 1),
      (Just "hAhA", Score 1),
      (Just "Hahah", Score 0),
      (Nothing, Score 0)
    ]

testTaskStrip =
  testTask
    (getTestTaskStrip "Who killed the tzar?" "Kommunyaki" oneZeroConstraints)
    [ (Just "Kommunyaki", Score 1),
      (Just " Kommunyaki   ", Score 1),
      (Just "Nobody", Score 0),
      (Nothing, Score 0)
    ]

tests =
  TestList
    [ testStrictTask,
      testTaskNonCaseSensetive,
      getTestTaskStrip
    ]

main = runTestTT tests

import Test.HUnit

import F451

testStrictTask = TestCase (
    assertEqual "Task with 0-1 constraints"
        (scoreTasks (getStrictTask "How much?" 42 oneZeroConstraints) [Just 0, Just 42, Nothing])
        [Score 0, Score 1, Score 0]
    )

test2 = TestCase (assertEqual "s" 1 2)

tests =
  TestList
    [ TestLabel "testStrictTask" testStrictTask
    ]

main = runTestTT tests

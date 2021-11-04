import F451
import Test.HUnit

testStrictTask =
  TestCase
    ( assertEqual
        "Strict task with 0-1 constraints"
        ( scoreTasks
            (getStrictTask "How much?" 42 oneZeroConstraints)
            [Just 0, Just 42, Nothing]
        )
        [Score 0, Score 1, Score 0]
    )

testTaskNonCaseSensetive =
  TestCase
    ( assertEqual
        "Case non-sensetive task with 0-1 constraints"
        ( scoreTasks
            (getTaskNonCaseSensetive "What???" "Haha" oneZeroConstraints)
            [Just "Haha", Just "hAhA", Just "Hahah", Nothing]
        )
        [Score 1, Score 1, Score 0, Score 0]
    )

test2 = TestCase (assertEqual "s" 1 2)

tests =
  TestList
    [ TestLabel "Strict task" testStrictTask,
      TestLabel "Case non sensetive" testTaskNonCaseSensetive
    ]

main = runTestTT tests

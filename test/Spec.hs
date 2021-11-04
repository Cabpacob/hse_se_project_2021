import Test.HUnit

test1 = TestCase (assertEqual "s" 1 2)

test2 = TestCase (assertEqual "s" 1 2)

tests =
  TestList
    [ TestLabel "test1" test1
    ]

main = runTestTT tests

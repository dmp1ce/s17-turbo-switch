import S17TurboSwitch
import Test.Tasty ( defaultMain, TestTree, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parseTests, workModeTests]

parseTests :: TestTree
parseTests = testGroup "parse tests"
  [ testCase "parse bitmain-work-mode value" $
    parseWorkModeValue  ["\"bitmain-work-mode\" : \"2\",\n" ]  @?= Just 2
  , testCase "parse bitmain-work-mode value with more values" $
    parseWorkModeValue  ["\"bitmain-work-mode\" : \"0\",\n","something","more"]  @?= Just 0
  , testCase "parse bad bitmain-work-mode value" $
    parseWorkModeValue  ["#\"bitmain-work-mode\" : \"2\",\n"]  @?= Nothing
  ]

workModeTests :: TestTree
workModeTests = testGroup "work mode tests"
  [ testCase "Turbo mode = 2" $ fromEnum Turbo @?= 2
  , testCase "Normal mode = 1" $ fromEnum Normal @?= 1
  , testCase "LowPower mode = 0" $ fromEnum LowPower @?= 0
  ]

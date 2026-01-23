module Main where

import Test.Tasty
import Test.Tasty.HUnit

-- 1. 定义具体的测试用例
-- 这是一个测试用例，判断 1 + 1 是否等于 2
testCase1 :: TestCase
testCase1 = "2 + 2 equals 4" ~? (2 + 2 == 4)

-- 这是另一个测试用例，演示列表长度
testCase2 :: TestCase
testCase2 = testCase "List length" $     length [1, 2, 3] @?= 3

-- 2. 将测试用例组织成测试树
main :: IO ()
main = defaultMain $ testGroup "My Test Suite"
    [ testCase1
    , testCase2
    ]

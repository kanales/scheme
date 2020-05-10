import Test.HUnit
import Data.Monoid
import Lib


testVector :: Test
testVector = 
    let
        l =  Right (Vector [Number 1, Number 2])
        r = parse "#(1 2 3)"
     in TestCase $ assertEqual "Should return Right vector" l r

testBool :: Test
testBool =
    let 
        l = Right [Bool True, Bool False]
        r = traverse parse ["#t", "#f"]
    in TestCase $ assertEqual "Should parse #t -> True and #f -> False" l r

testChar :: Test
testChar =
    let 
        l = Right [Character 'a', Character ' ', Character '\n']
        r = traverse parse ["#\\a", "#\\space", "#\\newline"]
    in TestCase $ assertEqual "Should parse #\\c, #\\space #\\newline as chars" l r

main :: IO Counts
main = runTestTT $ TestList [testVector, testBool, testChar]

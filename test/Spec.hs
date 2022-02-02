import Data.Char (isDigit)
import Test.QuickCheck

isAllDigits :: String -> Bool
isAllDigits = all ((== True) . (\x -> x `elem` ['1', '2', '3', '4', '5', '6', '7', '8', '9']))

testIsAllDigits :: String -> Bool
testIsAllDigits val =
  if isAllDigits val || val == ""
    then onlyDigit == length val
    else onlyDigit /= length val
  where
    onlyDigit = length $ filter isDigit val

testSum :: Int -> Int -> Bool
testSum x y = x + y

main :: IO ()
main = do
  quickCheck testIsAllDigits
  quickCheck
  putStrLn "Done"

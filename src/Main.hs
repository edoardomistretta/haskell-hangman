module Main (main) where

import Control.Monad (forever)
import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.List (intersperse)
import System.Exit (exitSuccess)
import System.Random (randomRIO)

newtype WordList = WordList [String]

minWordLength :: Int
minWordLength = 4
maxWordLength :: Int
maxWordLength = 8
maxFailedAttempts :: Int
maxFailedAttempts = maxWordLength - 2

allWords :: IO WordList
allWords = do
  fileContent <- readFile "data/dict.txt"
  return $ WordList $ lines fileContent

isInBound :: Int -> Int -> Int -> Bool
isInBound minB maxB i
  | i >= minB && i <= maxB  = True
  | otherwise               = False

gameWords :: IO WordList
gameWords = do
  (WordList aw) <- allWords
  return $ WordList $ filter (isInBound minWordLength maxWordLength . length) aw

randomWord :: WordList -> IO String
randomWord (WordList wl) = do
  i <- randomRIO (0, length wl - 1)
  return $ wl !! i

randomWord' :: IO String
-- randomWord' = do
--   gw <- gameWords
--   randomWord gw
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char] [Char]

-- instance Show Puzzle where
--   show (Puzzle _ discovered guessed) =
--     (intersperse ' ' $ fmap renderPuzzleChar discovered) ++
--     " Guessed so far: " ++ guessed

instance Show Puzzle where
  show (Puzzle _ discovered guessed missed) =
    intersperse ' ' (map (maybe '_' id) discovered) ++ "\n" ++ "Guessed so far: " ++ guessed ++ "\n" ++ "Missed so far: " ++ missed ++ "\n" ++ show (maxFailedAttempts - length missed) ++ " failing attempts left" ++ "\n"

freshPuzzle :: String -> Puzzle
freshPuzzle s = Puzzle s (map (const Nothing) s) "" ""

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle s _ _ _) = flip elem s

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed _) = flip elem guessed

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle s discovered guessed missed) c = Puzzle s newDiscovered newGuessed newMissed
  where
    newGuessed = if foundSomething then c:guessed else guessed
    newMissed = if foundSomething then missed else c:missed
    foundSomething = discovered /= newDiscovered
    newDiscovered = zipWith (\wc m -> if wc == c then Just c else m) s discovered

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess p c = do
  putStrLn $ "Your guess was: " ++ [c]
  case (charInWord p c, alreadyGuessed p c) of
    (_, True) -> do
      putStrLn "You already guessed that character, pick something else!"
      return p
    (True, _) ->  do
      putStrLn "This character was in the word, filling in the word accordingly"
      return $ fillInCharacter p c
    (False, _) -> do
      putStrLn "This character wasn't in the word, try again."
      return $ fillInCharacter p c

gameOver :: Puzzle -> IO ()
gameOver (Puzzle wordToGuess _ _ missed) =
  if length missed >= maxFailedAttempts then
    do
      putStrLn "You lose!"
      putStrLn $ "The word was: " ++ wordToGuess
      exitSuccess
  else
    return ()

gameWin :: Puzzle -> IO ()
gameWin (Puzzle w discovered _ _) =
  if all isJust discovered then
    do
      putStrLn "You win!"
      putStrLn $ "The word was " ++ w
      exitSuccess
  else
    return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
  gameOver puzzle
  gameWin puzzle
  putStrLn "------------------"
  putStrLn $ "Current puzzle is: " ++ show puzzle
  putStr "Guess a letter: "
  guess <- getLine
  case guess of
    [c] -> handleGuess puzzle c >>= runGame
    _ -> putStrLn "Your guess must be a single character"

main :: IO ()
main = do
  w <- randomWord'
  runGame $ freshPuzzle $ fmap toLower w

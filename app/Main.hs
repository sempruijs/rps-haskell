module Main where

import System.Random (randomRIO)
import System.IO
import Debug.Trace

main :: IO ()
main = do
  putStrLn "Welcome to Rock paper scissors."
  game <- playGame initGame
  putStrLn $ case (resultMessage game) of
    Just s -> s
    Nothing -> "Could not make result message based on game"

data Option
  = Rock
  | Paper
  | Scissors
  deriving (Show, Eq, Bounded, Enum)

getRandomOption :: IO Option
getRandomOption = do
  n <- randomRIO (fromEnum (minBound :: Option), fromEnum (maxBound :: Option))
  return (toEnum n)

data RoundResult
  = Win
  | Lose
  | Draw
  deriving (Eq)

instance Show RoundResult where
  show Draw = "It is draw"
  show Win = "Round won!"
  show Lose = "Round lost"
  
data Round = Round
  { player :: Option
  , bot :: Option
  } deriving (Show)

data Game = Game
  { winScore :: Int
  , rounds :: [Round]
  } deriving (Show)

initGame :: Game
initGame = Game 
  { winScore = 3
  , rounds = []
  }

playerScore :: Game -> Int
playerScore g = length $ filter (\r -> Win == roundResult r) (rounds g)

botScore :: Game -> Int
botScore g = length $ filter (\r -> Lose == roundResult r) (rounds g)

showScore :: Game -> String
showScore g = "you: " ++ (show $ playerScore g) ++ ", bot: " ++ (show $ botScore g)

showGameState :: Game -> String
showGameState g = let
  round = show $ roundResult $ head (rounds g)
  in round ++ " | " ++ (showScore g)

playGame :: Game -> IO Game
playGame g = 
  case resultMessage g of
    Just _ -> return g
    Nothing -> do
      putStr "(r)ock, (p)aper, (s)cissors: "
      hFlush stdout
      input <- getLine
      botOption <- getRandomOption
      let
        Just playerOption = charToOption $ head input
        round = Round { player = playerOption, bot = botOption}
        newGame = g { rounds = round:(rounds g) }
        in do
          putStrLn $ showGameState newGame
          hFlush stdout
          playGame newGame

resultMessage :: Game -> Maybe String
resultMessage g
  | maxAmountInGame g Lose = Just "You lost"
  | maxAmountInGame g Win = Just "You won"
  | otherwise = Nothing
  where
    maxAmountInGame :: Game -> RoundResult -> Bool
    maxAmountInGame g r = winScore g == (length $ filter (\x -> (roundResult x) == r) (rounds g))

roundResult :: Round -> RoundResult
roundResult r
  | player r == bot r = Draw
  | win r = Win
  | otherwise = Lose
  where
    win :: Round -> Bool
    win r = (player r == Rock && bot r == Scissors) || (player r == Paper && bot r == Rock) || (player r == Scissors && bot r == Paper)

charToOption :: Char -> Maybe Option
charToOption c = case c of
  'r' -> Just Rock
  'p' -> Just Paper
  's' -> Just Scissors
  _ -> trace ("invalid character \"" ++ [c] ++ "\"") Nothing


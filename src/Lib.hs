module Lib where

import Control.Monad.State
import Control.Monad.Except
import System.Random
import Data.List

data Suit = Hearts | Diamonds | Clubs | Spades deriving (Show, Read, Eq, Ord, Enum)
data Card = Card Int Suit deriving (Eq, Ord)
data BurstError = PlayerBurst | DealerBurst deriving Show
type Game a = StateT CardState (ExceptT BurstError IO) a
instance Show Card where
  show (Card i Hearts)   = "H " ++ show i
  show (Card i Diamonds) = "D " ++ show i
  show (Card i Clubs)    = "C " ++ show i
  show (Card i Spades)   = "S " ++ show i

data CardState = CardState
 { handsA :: [Card]
 , handsB :: [Card]
 , deck   :: [Card]
 } deriving Show
data Player = Player | Dealer  deriving (Show, Eq)
 
 
allDeck :: [Card]
allDeck = [Card n s | n <- [1 .. 13], s <- [Hearts .. Spades]]

shuffleDeck :: RandomGen g => [Card] -> g -> [Card]
shuffleDeck deck gen =
  let cardSize = length deck
      orderNums = take cardSize . nub $ randomRs (1, cardSize) gen
  in map fst . sortOn snd $ zip deck orderNums

points :: Card -> [Int]
points (Card num _)
  | num == 1 = [1, 11]
  | 2 <= num && num <= 10 = [num]
  | otherwise = [10]

sumEach :: [Int] -> [Int] -> [Int]
sumEach xs ys = [ m + n | m <- xs, n <- ys]

evalPoints :: [Card] -> [Int]
evalPoints hands = filter (<= 21) . foldl sumEach [0] $ map points hands

pickCard :: Game Card
pickCard = do
  now <- get
  let draw = head . deck $ now
      modifyState = now {deck = tail . deck $ now}
  put modifyState
  return draw
  

updateHands :: Player ->  Game ()
updateHands player = do
  draw <- pickCard
  modify $ \s ->
    case player of
     Player -> s {handsA = handsA s ++ [draw]}
     Dealer -> s {handsB = handsB s ++ [draw]}
  

playerTurn :: Game Int
playerTurn = do
  nowPoints <- gets (evalPoints . handsA)
  case nowPoints of
    [] ->  throwError PlayerBurst
    xs -> do
      liftIO . putStrLn $ "Hit(h) or Stand(s)"
      c <- liftIO getChar
      liftIO . putStrLn $ ""
      case c of
        'h' -> do
          updateHands Player
          gets handsA >>= liftIO . print
          playerTurn
        's' -> return . maximum $ nowPoints
        _ -> (liftIO . print  $ "Please Input h or s !!") >> playerTurn
  

dealerTurn :: Game Int
dealerTurn = do
  nowPoints <- gets (evalPoints . handsB)
  case nowPoints of
    [] -> gets handsB >>= liftIO . print >> throwError DealerBurst
    xs -> do
      let dealerPoint = maximum nowPoints
      if dealerPoint < 17
        then updateHands Dealer >> dealerTurn
        else gets handsB >>= liftIO . print >> return dealerPoint
              

initCardState :: Game ()
initCardState =
  replicateM_ 2 $ updateHands Player >> updateHands Dealer
  
runGame :: CardState -> IO ()
runGame state = do
  result <- runExceptT . (`evalStateT` state) $ do
    initCardState
    dealerHands <- gets handsB
    liftIO . putStrLn $ "dealer hands is:"
    liftIO . print . head $ dealerHands
    playerHands <- gets handsA
    liftIO . putStrLn $ "player hands is:"
    liftIO . print $ playerHands
    pp <- playerTurn
    dp <- dealerTurn
    liftIO . print $ "Player point is: " <> show pp
    liftIO . print $ "Dealer point is: " <> show dp
    return (pp, dp)
  case result of
    Left PlayerBurst -> print "player is burned !! dealer won"
    Left DealerBurst -> print "dealer is burned !! player won"
    Right (p, d) ->
      if p > d
        then print "player won"
        else print "dealer won"


main :: IO ()
main = do
  putStrLn "welcome to Black Jack"
  shuffled <- shuffleDeck allDeck <$> newStdGen
  runGame $ CardState [] [] shuffled


      
testCardState :: CardState
testCardState = CardState  [Card 10 Hearts, Card 6 Hearts] [] [Card 2 Hearts, Card 9 Hearts]

test = runExceptT . (evalStateT  playerTurn) $ testCardState

someFunc :: IO ()
someFunc = putStrLn "someFunc"

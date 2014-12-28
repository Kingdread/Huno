{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where

import           Cards
import           Game
import           Player

import           Control.Monad.State

import           System.IO
import           Text.Read           (readMaybe)


-- | Returns a Move that the current Player would do. Randomly picks one for
-- the AI, asks the Player in case of a HumanPlayer
getMove :: (Monad m, MonadIO m, MonadState Game m) => Bool -> m Move
getMove s = gets (head . players) >>= getMove_ s

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0..]

formatMoves :: [Move] -> String
formatMoves = foldl (\a (i, m) -> a ++ show i ++ ".: " ++ show m ++ "\n") "" . enumerate

getInputBelow :: Int -> IO Int
getInputBelow limit = do
  putStr "Input a number: "
  hFlush stdout
  inp <- getLine
  case (readMaybe inp :: Maybe Int) of
   Just x -> if x < limit
             then return x
             else getInputBelow limit
   Nothing -> getInputBelow limit

getMove_ :: (Monad m, MonadIO m, MonadState Game m) => Bool -> Player -> m Move
getMove_ snd_pss (Player Ai _ hand)       = do
  game <- get
  case validMoves game hand of
   (x:_) -> return (Play x)
   [] -> return . drawOrPass $ snd_pss
getMove_ snd_pss (Player Human name hand) = do
  game <- get
  moves <- liftIO $ do
    putStrLn ("Player " ++ name ++ ":")
    putStrLn ("Your hand is: " ++ show hand)
    let m = drawOrPass snd_pss : (map Play . validMoves game $ hand)
    putStr . formatMoves $ m
    return m
  answer <- liftIO $ getInputBelow (length moves)
  return (moves !! answer)

drawOrPass :: Bool -> Move
drawOrPass False = Draw
drawOrPass True  = Pass

-- | Takes a list of "skeleton players" and deals the (shuffled) cards.
makeGame :: [Player] -> IO Game
makeGame participants = liftM (makeGame' participants) $ shuffled deck

makeGame' :: [Player] -> Deck -> Game
makeGame' participants d =
  let
    (pl, nd) = deal participants [] d
    (top:rest) = nd
  in
   Game { topCard = top
        , players = pl
        , stack   = rest
        , ntake   = 0
        , nskip   = False
        }

deal :: [Player] -> [Player] -> Hand -> ([Player], Hand)
deal [] pls cards     = (pls, cards)
deal (p:ps) pls cards = let (player_cards, remainder) = splitAt 7 cards
                        in deal ps (withCards p player_cards : pls) remainder
  where
    withCards (Player t n _)    = Player t n

filterOne :: (a -> Bool) -> [a] -> [a]
filterOne _ []     = []
filterOne f (x:xs) = if f x
                     then x : filterOne f xs
                     else xs

draw :: Int -> Game -> Game
draw n g = let (taken, st) = splitAt n (stack g)
           in g { players = updateFirstPlayer (++taken) (players g)
                , stack = st
                }

updateFirstPlayer :: (Hand -> Hand) -> [Player] -> [Player]
updateFirstPlayer f p = let (pl:rst) = p
                        in applyHand f pl : rst

cname :: Game -> String
cname = getName . head . players

nextPlayer :: Game -> Game
nextPlayer g = g { players = rotate . players $ g }

-- | Performs a single game round, usually called in a loop
gameRound :: (Monad m, MonadIO m, MonadState Game m) => m ()
gameRound = do
    initial <- get
    let pname = getName . head . players $ initial
    let pcards = length . getCards . head . players $ initial
    liftIO $ do
      putStrLn ("\n\nThe top card is " ++ (show . topCard $ initial))
      putStrLn ("It's " ++ pname ++ "'s turn, " ++ show pcards ++ " cards left")
    game <- get
    if nskip game then do
        liftIO $ putStrLn (pname ++ " skips the round")
        put $ game { nskip = False }
        modify nextPlayer
        return ()
    else do
        move <- getMove False
        if move == Draw then do
            let nt = ntake game `max` 1
            liftIO $ putStrLn (pname ++ " " ++ takeLine nt)
            modify $ draw nt
            modify $ \g -> g { ntake = 0 }
            getMove True >>= doMove
        else
            doMove move

takeLine :: Int -> String
takeLine 1 = "takes a single card"
takeLine n = "takes " ++ show n ++ " cards"

pickColor :: (Monad m, MonadIO m, MonadState Game m) => m Color
pickColor = do
  g <- get
  liftIO $ do
    c <- getColor . head . players $ g
    putStrLn ("The picked color is " ++ show c)
    return c

nextTopCard :: (Monad m, MonadIO m, MonadState Game m) => Card -> m Card
nextTopCard card = case card of
  Pick  -> liftM Picked pickColor
  Pick4 -> liftM Picked pickColor
  _     -> return card

nextPlayersModifier :: Card -> [Player] -> [Player]
nextPlayersModifier card = case card of
  Card _ Reverse -> reverse
  _              -> rotate

nextTake :: Card -> Int -> Int
nextTake card = case card of
  Pick4        -> const 4
  Card _ Take2 -> (+ 2)
  _            -> id

nextSkip :: Card -> Bool
nextSkip card = case card of
  Card _ Skip -> True
  _           -> False

doMoveUpdate :: Card -> Card -> Game -> Game
doMoveUpdate card nextTop g = g {
    topCard = nextTop
  , ntake = nextTake card . ntake $ g
  , nskip = nextSkip card
  , players = nextPlayersModifier card . updateFirstPlayer (filterOne (/= card)) . players $ g
  }

doMove :: (Monad m, MonadIO m, MonadState Game m) => Move -> m ()
doMove (Play card) = do
  game <- get
  liftIO $ putStrLn (cname game ++ " decides to play " ++ show card)
  nTopCard <- nextTopCard card
  modify $ doMoveUpdate card nTopCard
doMove Pass = do
  g <- get
  liftIO $ putStrLn (cname g ++ " can't play a card")
  modify nextPlayer
  return ()
doMove _ = error "You shouldn't be able to do that move here"

-- | Returns a color that the player picks
getColor :: Player -> IO Color
getColor (Player Ai _ hand) = let getCol [] = Blue
                                  getCol (h:hs) = case h of
                                                   Card col _ -> col
                                                   _ -> getCol hs
                               in return . getCol $ hand
getColor h@(Player Human _ _) = do
    putStr "Input a color (Red, Blue, Yellow, Green): "
    hFlush stdout
    inp <- getLine
    case (readMaybe inp :: Maybe Color) of
        Just c -> return c
        Nothing -> getColor h

-- | "Cycles" a list by putting the first element at the end
rotate :: [a] -> [a]
rotate []     = []
rotate (x:xs) = xs ++ [x]


-- | Returns True if a game is won
won :: Game -> Bool
won = any (null . getCards) . players

gameLoop :: (Monad m, MonadIO m, MonadState Game m) => m ()
gameLoop = do
  w <- gets won
  if w
  then do
    winner <- gets $ last . players
    liftIO $ putStrLn (getName winner ++ " has won the game")
  else do
    gameRound
    gameLoop
  return ()

-- | Run the main UNO game
main :: IO ()
main = do
    game <- makeGame [Player Human "Homo Sapiens" [], Player Ai "Elisa" []]
    evalStateT gameLoop game

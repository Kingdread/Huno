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
getMove :: (Monad m, MonadIO m, MonadState Game m) => m Move
getMove = gets (last . players) >>= getMove_

formatMoves :: [Move] -> String
formatMoves m = fM m (0 :: Int)
  where
   fM (x:xs) i = show i ++ ".: " ++ show x ++ "\n" ++ fM xs (i + 1)
   fM [] _     = ""

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

getMove_ :: (Monad m, MonadIO m, MonadState Game m) => Player -> m Move
getMove_ (Player Ai _ hand)       = do
  game <- get
  case validMoves game hand of
   (x:_) -> return (Play x)
   [] -> return Draw
getMove_ (Player Human name hand) = do
  game <- get
  moves <- liftIO $ do
    putStrLn ("Player " ++ name ++ ":")
    putStrLn ("Your hand is: " ++ show hand)
    let m = Draw : (map Play . validMoves game $ hand)
    putStr . formatMoves $ m
    return m
  answer <- liftIO $ getInputBelow (length moves)
  return (moves !! answer)

-- | Takes a list of "skeleton players" and deals the (shuffled) cards.
makeGame :: [Player] -> IO Game
makeGame participants = do
    d <- shuffled deck
    let (pl, nd) = deal participants [] d
    let (top:rest) = nd
    return Game { topCard = top
                , players = pl
                , stack   = rest
                , ntake   = 0
                , nskip   = False
                }
    where
    deal [] pls cards     = (pls, cards)
    deal (p:ps) pls cards = let (player_cards, remainder) = splitAt 7 cards
                            in deal ps (withCards p player_cards : pls) remainder
    withCards (Player t n _)    = Player t n

filterOne :: (a -> Bool) -> [a] -> [a]
filterOne _ []     = []
filterOne f (x:xs) = if f x
                     then x : filterOne f xs
                     else xs

draw :: Int -> Game -> Game
draw n g = let (taken, st) = splitAt n (stack g)
           in g { players = updateLastPlayer (++taken) (players g)
                , stack = st
                }

updateLastPlayer :: (Hand -> Hand) -> [Player] -> [Player]
updateLastPlayer f p = let (pls, lst:_) = splitAt (length p - 1) p
                       in pls ++ [applyHand f lst]

cname :: Game -> String
cname = getName . last . players


-- | Performs a single game round, usually called in a loop
gameRound :: (Monad m, MonadIO m, MonadState Game m) => m ()
gameRound = do
    initial <- get
    let pname = getName . head . players $ initial
    let pcards = length . getCards . head . players $ initial
    liftIO $ do
      putStrLn ("\n\nThe top card is " ++ (show . topCard $ initial))
      putStrLn ("It's " ++ pname ++ " turn, " ++ show pcards ++ " cards left")
    -- Advance to the next player
    modify (\g -> g { players = rotate . players $ g })
    game <- get
    if nskip game then do
        liftIO $ putStrLn (pname ++ " skips the round")
        put $ game { nskip = False }
        return ()
    else do
        move <- getMove
        if move == Draw then do
            let nt = ntake game
            if nt == 0 then do
                liftIO $ putStrLn (pname ++ " takes a single card")
                modify (draw 1)
            else do
                liftIO $ putStrLn (pname ++ " takes " ++ show nt ++ " cards")
                modify (draw nt)
                modify (\g -> g { ntake = 0 })
            getMove >>= doMove
        else
            doMove move

pickColor :: (Monad m, MonadIO m, MonadState Game m) => m Color
pickColor = do
  g <- get
  liftIO $ do
    c <- getColor . last . players $ g
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
  _              -> id

nextTake :: Card -> Int -> Int
nextTake card = case card of
  Pick4        -> const 4
  Card _ Take2 -> (+ 2)
  _            -> id

nextSkip :: Card -> Bool
nextSkip card = case card of
  Card _ Skip -> True
  _           -> False

doMove :: (Monad m, MonadIO m, MonadState Game m) => Move -> m ()
doMove (Play card) = do
  game <- get
  liftIO $ putStrLn (cname game ++ " decides to play " ++ show card)
  nTopCard <- nextTopCard card
  modify (\g -> g { topCard = nTopCard
                  , ntake = nextTake card . ntake $ g
                  , nskip = nextSkip card
                  , players = nextPlayersModifier card . updateLastPlayer (filterOne (/= card)) . players $ g
                  })
doMove Draw = do
  g <- get
  liftIO $ putStrLn (cname g ++ " can't play a card")
  return ()

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

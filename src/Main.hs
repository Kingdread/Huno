module Main where
import Cards
import Data.IORef
import System.IO
import Text.Read

-- | Returns a Move that the current Player would do. Randomly picks one for
-- the AI, asks the Player in case of a HumanPlayer
getMove :: Game -> IO Move
getMove g = getMove_ g (last . players $ g)

getMove_ :: Game -> Player -> IO Move
getMove_ game (AiPlayer _ hand)       = case validMoves game hand of
                                           (x:_) -> return (Play x)
                                           [] -> return Draw
getMove_ game (HumanPlayer name hand) = do
    putStrLn ("Player " ++ name ++ ":")
    putStrLn ("Your hand is: " ++ show hand)
    let moves = Draw : (map Play . validMoves game $ hand)
    putStr . formatMoves $ moves
    answer <- getInputBelow (length moves)
    return (moves !! answer)
    where
    formatMoves :: [Move] -> String
    formatMoves m = fM m 0
    fM :: [Move] -> Integer -> String
    fM (x:xs) i = show i ++ ".: " ++ show x ++ "\n" ++ fM xs (i + 1)
    fM [] _     = ""
    getInputBelow limit = do
        putStr "Input a number: "
        hFlush stdout
        inp <- getLine
        case (readMaybe inp :: Maybe Int) of
            Just x -> if x < limit then return x
                      else getInputBelow limit
            Nothing -> getInputBelow limit


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
    withCards (AiPlayer n _)    = AiPlayer n
    withCards (HumanPlayer n _) = HumanPlayer n


-- | Performs a single game round, usually called in a loop
gameRound :: Game -> IO Game
gameRound initial = do
    let pname = getName . head . players $ initial
    let pcards = length . getCards . head . players $ initial
    putStrLn ("\n\nThe top card is " ++ (show . topCard $ initial))
    putStrLn ("It's " ++ pname ++ " turn, " ++ show pcards ++ " cards left")
    ludus <- newIORef initial
    -- Advance to the next player
    modifyIORef' ludus (\g -> g { players = rotate . players $ g })
    game <- readIORef ludus
    if nskip game then do
        putStrLn (pname ++ " skips the round")
        return game { nskip = False }
    else do
        move <- getMove game
        if move == Draw then do
            let nt = ntake game
            if nt == 0 then do
                putStrLn (pname ++ " takes a single card")
                modifyIORef ludus (draw 1)
            else do 
                putStrLn (pname ++ " takes " ++ show nt ++ " cards")
                modifyIORef ludus (draw nt)
                modifyIORef ludus (\g -> g { ntake = 0 })
            game <- readIORef ludus
            move <- getMove game
            doMove move game
        else
            doMove move game
    where
    filterOne :: (a -> Bool) -> [a] -> [a]
    filterOne _ []     = []
    filterOne f (x:xs) = if f x then x : filterOne f xs
                         else xs
    draw :: Int -> Game -> Game
    draw n g = let (taken, st) = splitAt n (stack g)
               in g { players = updateLastPlayer (++taken) (players g)
                    , stack = st
                    }
    updateLastPlayer :: (Hand -> Hand) -> [Player] -> [Player]
    updateLastPlayer f p = let (pls, l:_) = splitAt (length p - 1) p
                           in pls ++ [applyHand f l]
    cname :: Game -> String
    cname = getName . last . players
    doMove :: Move -> Game -> IO Game
    doMove (Play card) g = do
        putStrLn (cname g ++ " decides to play " ++ show card)
        case card of
            Pick -> do
                color <- getColor . last . players $ g
                putStrLn ("The picked color is " ++ show color)
                return g { topCard = Picked color
                         , players = updateLastPlayer (filterOne (/= card)) (players g)
                         }
            Pick4 -> do
                color <- getColor . last . players $ g
                putStrLn ("The picked color is " ++ show color)
                return g { topCard = Picked color
                         , ntake = 4
                         , players = updateLastPlayer (filterOne (/= card)) (players g)
                         }
            Card _ Reverse ->
                return g { topCard = card
                         , players = reverse . updateLastPlayer (filterOne (/= card)) . players $ g
                         }
            Card _ Take2 ->
                return g { topCard = card
                         , players = updateLastPlayer (filterOne (/= card)) (players g)
                         , ntake = ntake g + 2
                         }
            Card _ Skip ->
                return g { topCard = card
                         , players = updateLastPlayer (filterOne (/= card)) (players g)
                         , nskip = True
                         }
            Card _ _ ->
                return g { topCard = card
                         , players = updateLastPlayer (filterOne (/= card)) (players g)
                         }
            Picked _ -> error "Cheater, how did you get that card?"
    doMove Draw g = do
        putStrLn (cname g ++ " can't play a card")
        return g


-- | Returns a color that the player picks
getColor :: Player -> IO Color
getColor (AiPlayer _ hand) = let getCol [] = Blue
                                 getCol (h:hs) = case h of
                                                       Card col _ -> col
                                                       _ -> getCol hs
                               in return . getCol $ hand
getColor h@(HumanPlayer _ _) = do
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

-- | Run the main UNO game
main :: IO ()
main = do
    game <- makeGame [HumanPlayer "Homo Sapiens" [], AiPlayer "Elisa" []]
    gameLoop game
    where
    gameLoop :: Game -> IO ()
    gameLoop g = if won g then do
                    let winner = last . players $ g
                    putStrLn (getName winner ++ " has won the game")
                 else gameRound g >>= gameLoop

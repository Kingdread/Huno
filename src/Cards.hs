module Cards where

import System.Random

data Color = Red | Blue | Green | Yellow deriving (Eq, Show, Read, Enum)
data Value = One | Two | Three | Four | Five | Six | Seven | Eight |
             Nine | Zero | Take2 | Skip | Reverse deriving (Eq, Show, Read, Enum)
data Card = Card Color Value | Pick | Pick4 | Picked Color deriving (Eq, Show, Read)

type Deck = [Card]
type Hand = [Card]

data Player = AiPlayer String Hand | HumanPlayer String Hand deriving (Show)

data Game = Game { topCard :: Card
                 , players :: [Player]
                 , stack   :: Deck
                 , ntake   :: Int
                 , nskip   :: Bool
                 } deriving (Show)

data Move = Play Card | Draw deriving (Eq, Show)

getName :: Player -> String
getName (AiPlayer n _)    = n
getName (HumanPlayer n _) = n

getCards :: Player -> Hand
getCards (AiPlayer _ c)    = c
getCards (HumanPlayer _ c) = c

addCards :: [Card] -> Player -> Player
addCards a (AiPlayer n c)    = AiPlayer n (a ++ c)
addCards a (HumanPlayer n c) = HumanPlayer n (a ++ c)

applyHand :: (Hand -> Hand) -> Player -> Player
applyHand f (AiPlayer n c)    = AiPlayer n (f c)
applyHand f (HumanPlayer n c) = HumanPlayer n (f c)


-- | Returns whether whether card2 can be played on top of card1
isValidMove :: Game -> Card -> Bool
isValidMove game card = if ntake game == 0 then isValidMove_ (topCard game) card
                        else case card of
                            (Card _ Take2) -> not (isPick4 . topCard $ game)
                            _ -> False
                        where
                        isPick4 (Picked _) = True
                        isPick4 _          = False

isValidMove_ :: Card -> Card -> Bool
-- Normal cards, either color matches or value matches
isValidMove_ (Card c1 v1) (Card c2 v2) = c1 == c2 || v1 == v2
-- You can put the black ones on top of everything
isValidMove_ _ Pick  = True
isValidMove_ _ Pick4 = True
-- Color picked
isValidMove_ (Picked c1) (Card c2 _) = c1 == c2
-- If this case happens, there is some logic flaw in the function that
-- puts cards onto the stack. A Pick card should always be replaced by
-- a Picked Color card.
isValidMove_ Pick _       = error "Some dork ruined the game logic, fix pls"
isValidMove_ Pick4 _      = error "No really, someone messed up the logic"
-- And nobody should every try to place a Picked Color card, they are only
-- used internally
isValidMove_ _ (Picked _) = error "Meeep, still broken, it won't fix itself"


-- | Takes a Hand and returns a list of every card that could be played
validMoves :: Game -> Hand -> [Card]
validMoves = filter . isValidMove


-- | An unshuffled deck of cards as they are available in the game
deck :: Deck
deck = [Card c Zero | c <- [Red ..]]
    ++ dup [Card c v | c <- [Red ..], v <- [One ..]]
    ++ quad [Pick, Pick4]
    where
    dup = concatMap (replicate 2)
    quad = dup . dup

-- | Returns a randomly shuffled deck. Algorithm very inefficient
shuffled :: Deck -> IO Deck
shuffled [] = return []
shuffled d  = do
    index <- getStdRandom (randomR (0, length d - 1))
    newdeck <- shuffled . takeAway d $ index
    return ((d !! index) : newdeck)
    where
    takeAway [] _         = []
    takeAway (x:xs) index = if index == 0 then xs
                            else x : takeAway xs (index - 1)


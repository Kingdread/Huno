module Cards where

import           System.Random

data Color = Red | Blue | Green | Yellow deriving (Eq, Show, Read, Enum)
data Value = One | Two | Three | Four | Five | Six | Seven | Eight |
             Nine | Zero | Take2 | Skip | Reverse deriving (Eq, Show, Read, Enum)
data Card = Card Color Value | Pick | Pick4 | Picked Color deriving (Eq, Show, Read)

type Deck = [Card]
type Hand = [Card]

isValidCard :: Card -> Card -> Bool
-- Normal cards, either color matches or value matches
isValidCard (Card c1 v1) (Card c2 v2) = c1 == c2 || v1 == v2
-- You can put the black ones on top of everything
isValidCard _ Pick  = True
isValidCard _ Pick4 = True
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


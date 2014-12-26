module Cards where

data Color = Red | Blue | Green | Yellow deriving (Eq, Show, Read, Enum)
data Value = One | Two | Three | Four | Five | Six | Seven | Eight |
             Nine | Zero | Take2 | Skip | Reverse deriving (Eq, Show, Read, Enum)
data Card = Card Color Value | Pick | Pick4 | Picked Color deriving (Eq, Show, Read)

type Deck = [Card]
type Hand = [Card]


-- | Returns whether whether card2 can be played on top of card1
isValidMove :: Card -> Card -> Bool
-- Normal cards, either color matches or value matches
isValidMove (Card c1 v1) (Card c2 v2) = c1 == c2 || v1 == v2
-- You can put the black ones on top of everything
isValidMove _ Pick  = True
isValidMove _ Pick4 = True
-- Color picked
isValidMove (Picked c1) (Card c2 _) = c1 == c2


-- | Takes a Hand and returns a list of every card that could be played
validMoves :: Card -> Hand -> [Card]
validMoves = filter . isValidMove


-- | An unshuffled deck of cards as they are available in the game
deck :: Deck
deck = [(Card c Zero) | c <- [Red ..]]
    ++ dup [(Card c v) | c <- [Red ..], v <- [One ..]]
    ++ quad [Pick, Pick4]
    where
    dup = concatMap (replicate 2)
    quad = dup . dup

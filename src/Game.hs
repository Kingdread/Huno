module Game where

import           Cards
import           Player

data Game = Game { topCard :: Card
                 , players :: [Player]
                 , stack   :: Deck
                 , ntake   :: Int
                 , nskip   :: Bool
                 } deriving (Show)

data Move = Play Card | Draw | Pass deriving (Eq, Show)

-- | Returns whether whether card2 can be played on top of card1
isValidMove :: Game -> Card -> Bool
isValidMove game card = if ntake game == 0 then isValidCard (topCard game) card
                        else case card of
                            (Card _ Take2) -> not (isPick4 . topCard $ game)
                            _ -> False
                        where
                        isPick4 (Picked _) = True
                        isPick4 _          = False

-- | Takes a Hand and returns a list of every card that could be played
validMoves :: Game -> Hand -> [Card]
validMoves = filter . isValidMove

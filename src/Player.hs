module Player where

import           Cards

data PlayerType = Ai | Human deriving (Eq, Show)

data Player = Player {
    getType  :: PlayerType
  , getName  :: String
  , getCards :: Hand
  } deriving (Show)

addCards :: [Card] -> Player -> Player
addCards a (Player t n c)    = Player t n (a ++ c)

applyHand :: (Hand -> Hand) -> Player -> Player
applyHand f (Player t n c)    = Player t n (f c)

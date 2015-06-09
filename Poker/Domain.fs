namespace Poker.Types

type Suit =
    | Clubs
    | Hearts
    | Spades
    | Diamonds

type Rank =
    | Ace
    | King
    | Queen
    | Jack
    | Ten
    | Nine
    | Eight
    | Seven
    | Six
    | Five
    | Four
    | Three
    | Two

type Card =
    { Rank: Rank
      Suit: Suit }

type Hand = Card list

type HandRank =
    | RoyalFlush
    | StraightFlush of Rank
    | FourOfAKind of Rank * kicker: Rank
    | FullHouse of Rank * over: Rank
    | Flush of ranks: (Rank * Rank * Rank * Rank * Rank)
    | Straight of Rank
    | ThreeOfAKind of Rank * kickers: (Rank * Rank)
    | TwoPair of Rank * over: Rank * kicker: Rank
    | OnePair of Rank * kickers: (Rank * Rank * Rank)
    | High of Rank * kickers: (Rank * Rank * Rank * Rank)

type Classifier = Hand -> HandRank

type Player =
  { Name: string
    Hand: Hand }

type Result =
    | Winner of Player
    | SplitPot of Player list

type Game = Player list -> Result

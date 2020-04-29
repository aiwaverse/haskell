module Store where

type FirstName = String

type LastName = String

type MiddleName = String

data Name
  = Name FirstName LastName
  | NameWithMiddle FirstName MiddleName LastName
  | TwoInitialsWithLast Char Char LastName
  deriving (Show)

data Creator
  = AuthorCreator Author
  | ArtistCreator Artist
  deriving (Show)

newtype Author =
  Author Name

instance Show Author where
  show (Author n) = show n

data Artist
  = Person Name
  | Band String
  deriving (Show)

data Book = Book { author    :: Creator
                 , isbn      :: String
                 , bookTitle :: String
                 , bookYear  :: Int
                 , bookPrice :: Double
                 } deriving (Show)

data VinylRecord =
  VinylRecord { artist      :: Creator
              , recordTitle :: String
              , recordYear  :: Int
              , recordPrice :: Double
              }
              deriving (Show)

data CollectibleToy =
  CollectibleToy { name           :: String
                 , toyDescription :: String
                 , toyPrice       :: Double
                 }
                 deriving (Show)



data StoreItem
  = BookItem Book
  | RecordItem VinylRecord
  | ToyItem CollectibleToy
  deriving (Show)

data Pamphlet =
  Pamphlet { title               :: String
           , pamphletDescription :: String
           , contact             :: String
           }

hpLovecraft :: Creator
hpLovecraft = AuthorCreator $ Author (TwoInitialsWithLast 'H' 'P' "Lovecraft")

price :: StoreItem -> Double
price (BookItem   b) = bookPrice b
price (RecordItem r) = recordPrice r
price (ToyItem    t) = toyPrice t

madeBy :: StoreItem -> String
madeBy (BookItem   b) = show $ author b
madeBy (RecordItem r) = show $ artist r
madeBy _              = "unkown"

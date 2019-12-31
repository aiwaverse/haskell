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

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author (TwoInitialsWithLast 'H' 'P' "Lovecraft"))

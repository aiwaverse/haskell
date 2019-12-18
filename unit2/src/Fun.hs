module Fun where

--module that basically does everything the unit tells me to, this is on stack JUST to
--make hie not be annoying with me
type FirstName = String

type LastName = String

type MiddleName = String

data Name
   = Name
        { firstN :: FirstName
        , lastN  :: LastName
        }
   | NameWithMiddle
        { firstN  :: FirstName
        , middleN :: MiddleName
        , lastN   :: LastName
        }
   deriving (Show)

data Sex
   = Male
   | Female
   | NonBinary
   deriving (Show)

sexInitial :: Sex -> Char
sexInitial Male      = 'M'
sexInitial Female    = 'F'
sexInitial NonBinary = 'N'

data RhType
   = Pos
   | Neg
   deriving (Show)

data ABOType
   = A
   | B
   | AB
   | O
   deriving (Show)

data BloodType =
   BloodType ABOType RhType
   deriving (Show)

data Patient =
   Patient
      { name   :: Name
      , sex    :: Sex
      , age    :: Integer
      , height :: Integer
      , weight :: Integer
      , blood  :: BloodType
      }
   deriving (Show)

patientInfo :: Patient -> String
patientInfo pt = fullName ++ " " ++ ageHeigth
  where
    fullName = (firstN . name $ pt) ++ ", " ++ (lastN . name $ pt)
    ageHeigth =
       "(" ++ (show . age $ pt) ++ "yrs. " ++ (show . height $ pt) ++ "cm.)"

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _               = True
canDonateTo _ (BloodType AB _)              = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _                             = False

--with creator type

type FirstName = String
type LastName = String
type MiddleName = String

data Author = Author Name

data Artist = Person Name | Band String

data Creator = AuthorCreator Author | ArtistCreator Artist

data Name = Name FirstName LastName
    | NameWithMiddle FirstName MiddleName LastName
    | TwoInitialsWithLast Char Char LastName
    | FirstNameWithTwoInits FirstName Char Char

data Book = Book {
    author :: Creator
    , isbn :: String
    , bookTitle :: String
    , bookYear :: Int
    , bookPrice :: Double
    }

data VinylRecord = VinylRecord {
    artist :: Creator
    , recordTitle :: String
    , recordYear :: Int
    , recordPrice :: Double
    }

bookPrice :: Book -> Double
bookPrice (Book _ _ _ _ val) = val
recordPrice :: VinylRecord -> Double
recordPrice (VinylRecord _ _ _ _ val) = val

data CollectibleToy = CollectibleToy {
    name :: String
    , description :: String
    , toyPrice :: Double
    }

data StoreItem = BookItem Book
    | RecordItem VinylRecord
    | ToyItem CollectibleToy

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy

main = do
    print("Test")
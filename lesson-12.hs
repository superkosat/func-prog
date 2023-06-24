-- Functional Programming in Haskell 
-- Lesson 12
-- Creating your own types

-- introducing the first version of the type for patientInfo

patientInfoV1 :: String -> String -> Int -> Int -> String
patientInfoV1 fname lname age height = name ++ " " ++ ageHeight
 where name = lname ++ ", " ++ fname
       ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

-- second and improved version of the type for patientInfo using type synonyms

type FirstName = String
type LastName = String
type Age = Int
type Height = Int

patientInfoV2 :: FirstName -> LastName -> Age -> Height -> String
patientInfoV2 fname lname age height = name ++ " " ++ ageHeight
 where name = lname ++ ", " ++ fname
       ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

{-

GHCi> patientInfo "John" "Doe" 43 74
"Doe, John (43yrs. 74in.)"
GHCi> patientInfo "Jane" "Smith" 25 62
"Smith, Jane (25yrs. 62in.)"

-}

-- using a type synonym PatientName for (String,String)

type PatientName = (String,String)

-- accessing PatientName values using the helper functions firstName and secondName

firstName :: PatientName -> String
firstName patient = fst patient

lastName :: PatientName -> String
lastName patient = snd patient

{-

GHCi> firstName testPatient
"John"
GHCi> lastName testPatient
"Doe"

-}

-- another version of patientInfo using PatientName as input

patientInfoV3 :: PatientName -> Int -> Int -> String
patientInfoV3 (fname,lname) age height = name ++ " " ++ ageHeight
 where name = lname ++ ", " ++ fname
       ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

-- creating the type for two genders

data Gender = Male | Female

-- producing the initial (a character) of each of the genders

genderInitial :: Gender -> Char
genderInitial Male = 'M'
genderInitial Female = 'F'

-- defining the type RhType

data RhType = Pos | Neg

-- defining the type ABOtype

data ABOType = A | B | AB | O

-- combining ABOType and RhType to create BloodType

data BloodType = BloodType ABOType RhType

-- creating patients of type BloodType

patient1BT :: BloodType
patient1BT = BloodType A Pos

patient2BT :: BloodType
patient2BT = BloodType O Neg

patient3BT :: BloodType
patient3BT = BloodType AB Pos

-- displaying the values of (your) types RhType, ABOType, BloodType

showRh :: RhType -> String
showRh Pos = "+"
showRh Neg = "-"
showABO :: ABOType -> String
showABO A = "A"
showABO B = "B"
showABO AB = "AB"
showABO O = "O"
showBloodType :: BloodType -> String
showBloodType (BloodType abo rh)  = showABO abo ++ showRh rh

-- defining the canDonate function

canDonateTo :: BloodType -> BloodType -> Bool
canDonateTo (BloodType O _) _ = True
canDonateTo _ (BloodType AB _) = True
canDonateTo (BloodType A _) (BloodType A _) = True
canDonateTo (BloodType B _) (BloodType B _) = True
canDonateTo _ _ = False --otherwise

{-

GHCi> canDonateTo patient1BT patient2BT
False
GHCi> canDonateTo patient2BT patient1BT
True
GHCi> canDonateTo patient2BT patient3BT
True
GHCi> canDonateTo patient1BT patient3BT
True
GHCi> canDonateTo patient3BT patient1BT
False

-}

-- supporting two different name formats

type MiddleName = String
data Name = Name FirstName LastName
          | NameWithMiddle FirstName MiddleName LastName

-- displaying multiple constructors

showName :: Name -> String
showName (Name f l) = f ++ " " ++ l
showName (NameWithMiddle f m l) = f ++ " " ++ m ++ " " ++ l

-- creating a couple of names with different formats

name1 = Name "Jerome" "Salinger"
name2 = NameWithMiddle "Jerome" "David" "Salinger"

{-
GHCi> showName name1
"Jerome Salinger"
GHCi> showName name2
"Jerome David Salinger"
-}

{- how we did at the beginning of the lesson

patientInfo :: String -> String -> Int -> Int -> String
patientInfo fname lname age height = name ++ " " ++ ageHeight
 where name = lname ++ ", " ++ fname
       ageHeight = "(" ++ show age ++ "yrs. " ++ show height ++ "in.)"

-}

data PatientV1 = PatientV1 Name Gender Int Int Int BloodType

johnDoe :: PatientV1
johnDoe = PatientV1 (Name "John" "Doe") Male 30 74 200 (BloodType AB Pos)

janeESmith :: PatientV1
janeESmith = PatientV1 (NameWithMiddle "Jane" "Elizabeth" "Smith")
                       Female 28 62 140
                       (BloodType O Neg)

getName :: PatientV1 -> Name
getName (PatientV1 n _ _ _ _ _) = n

getAge :: PatientV1 -> Int
getAge (PatientV1  _ _ a _ _ _) = a

getBloodType :: PatientV1 -> BloodType
getBloodType (PatientV1 _ _ _ _ _ bt) = bt

-- same type Patient but using this time the syntax for record types

data PatientV2 = PatientV2 { name :: Name
                           , gender :: Gender
                           , age :: Int
                           , height :: Int
                           , weight :: Int
                           , bloodType :: BloodType }

jackieSmith :: PatientV2
jackieSmith = PatientV2 {name = Name "Jackie" "Smith"
                       , age = 43
                       , gender = Female
                       , height = 62
                       , weight = 115
                       , bloodType = BloodType O Neg }

-- one main benefit of the record type syntax is 
-- that the helper functions are easily defined

{-
GHCi> height jackieSmith
62
GHCi> showBloodType (bloodType jackieSmith)
"O-"
-}

-- in the same way as we did with flowers in Lesson 10
-- we can define new values of type Patient from older ones

jackieSmithUpdated = jackieSmith { age = 44 }

{-
GHCi> age jackieSmith
43
GHCi> age jackieSmithUpdated
44
-}
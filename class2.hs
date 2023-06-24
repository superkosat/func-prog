import Data.List
--Look into ackermann function

y = 1
x y = y * 3
doubleIt x = x * 2


state = "New York"
--streetAddress = number ++ street ++ city
--addressBuilder = streetAddress ++ state

ack 0 n = n + 1
ack n 0 = ack (n - 1) 1
ack m n = ack (m - 1) (ack m (n - 1))

gcd_1 a b = if b == 0
            then a
            else gcd_1 b (a `mod` b)

--Message passing and object creation with coffee cup
cup flOz = \message -> message flOz
coffeeCup = cup 12
getOz aCup = aCup (\flOz -> flOz)

drink aCup ozDrank = (flOz - ozDrank)
    where flOz = getOz aCup


--Flower objects--
flower (name,color,price) = \message -> message (name,color,price)
tulip = flower ("Tulip","Blue",80)
dahlia = flower ("Dahlia","Red",120)

--helper functions and getter functions
name (n,_,_) = n
color (_,c,_) = c
price (_,_,p) = p

getName aFlower = aFlower name
getColor aFlower = aFlower color
getPrice aFlower = aFlower price

--setter functions (creates new object each time)
setName aFlower newName = aFlower (\(n,c,p) -> flower (newName,c,p))
setColor aFlower newColor = aFlower (\(n,c,p) -> flower (n,newColor,p))
setPrice aFlower newPrice = aFlower (\(n,c,p) -> flower (n,c,newPrice))

--printing a flower object
printFlower aFlower = aFlower (\(n,c,p) -> n ++
                                " color:" ++ c
                                " price:" ++ (show n))









main :: IO ()
main = do
    print (x 5)
    print (gcd_1 12 18)
type Events = [String]
type Probs = [Double]

data PTable = PTable Events Probs

tableOfProbs :: Probs
tableOfProbs = [1, 1, 1, 1]

tableOfEvents :: Events
tableOfEvents = ["1st", "2nd", "3rd", "4th"]

createPTable :: Events -> Probs -> PTable
createPTable events probs = PTable events normalizedProbs
  where totalProbs = sum probs
        normalizedProbs = map (\x -> x/totalProbs) probs

showPair :: String -> Double -> String
showPair event prob = mconcat [event,"|",show prob,"\n"]

instance Show PTable where
   show (PTable events probs) = mconcat pairs
    where pairs = zipWith showPair events probs

-- the most interesting function of this code

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func l1 l2 = zipWith func newL1 cycledL2
  where nToAdd = length l2
        repeatedL1 = map (take nToAdd . repeat) l1
        newL1 = mconcat repeatedL1
        cycledL2 = cycle l2

-- the two functions are deduced from cartCombine

combineEvents :: Events -> Events -> Events
combineEvents e1 e2 = cartCombine combiner e1 e2
  where combiner = (\x y -> mconcat [x,"-",y])

combineProbs :: Probs -> Probs -> Probs
combineProbs p1 p2 = cartCombine (*) p1 p2

instance Semigroup PTable where
    (<>) ptable1 (PTable [] []) = ptable1
    (<>) (PTable [] []) ptable2 = ptable2
    (<>) (PTable e1 p1) (PTable e2 p2) = createPTable newEvents newProbs
       where newEvents = combineEvents e1 e2
             newProbs = combineProbs p1 p2 

instance Monoid PTable where
    mempty = PTable [] []
    mappend = (<>)

coin :: PTable
coin = createPTable ["heads","tails"] [1,1]

spinner :: PTable
spinner = createPTable ["red","blue","green"] [1,2,7]

biasedcoin :: PTable
biasedcoin = createPTable ["heads","tails"] [1,2]

dice :: PTable
dice = createPTable ["one","two","three","four","five","six"] [1,1,1,1,1,1]

biaseddice :: PTable
biaseddice = createPTable ["one","two","three","four","five","six"] [1,1,1,1,1,2]

main = do
  
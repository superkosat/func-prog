--Exercise 1
--take an example pair, apply fst and snd as below to obtain the individual elements
pair = (("hello",8),"world")
third = snd pair
second = snd (fst pair)
first = fst (fst pair)


--Exercise 2
--part a: haskell expressions for two, three as Church numerals
zero = \s z -> z
one = \s z -> s z

two = \s z -> s (s z)
three = \s z -> s (s (s z))


--part b: Church decoder
churchDecode num = num (+1) 0


--part c: Church print
churchPrint num = (num (++ "succ ") ("")) ++ "zero"


--part d: Church parity
churchOddEven m = if ((churchDecode m) `mod` 2 == 0)
                then "0"
                else "1"


--part e: Church increment
churchInc n = \s z -> s (n s z)


--part f: Church addition
churchAdd a b = \s z -> b s (a s z)


--part g: Church multiplication
churchMult a b = \s z -> a (b s) z


--part h: Church exponents
churchPower m n = n m



main = do
    print "Hello World!"
    print (churchPrint (churchInc one))
    print (churchPrint (churchAdd two two))
    print (churchPrint (churchMult two three))
    print (churchPrint (churchPower two three))
import Text.ParserCombinators.Parsec

val [] = 0
val ('M':xs) = 1000 + val xs
val ('D':xs) = 500 + val xs
val ('C':'D':xs) = 400 + val xs
val ('C':'M':xs) = 900 + val xs
val ('C':xs) = 100 + val xs
val ('L':xs) = 50 + val xs
val ('X':'L':xs) = 40 + val xs
val ('X':'C':xs) = 90 + val xs
val ('X':xs) = 10 + val xs
val ('V':xs) = 5 + val xs
val ('I':'V':xs) = 4 + val xs
val ('I':'X':xs) = 9 + val xs
val ('I':xs) = 1 + val xs
val _ = 0

toRN n = replicate m 'M' ++ rnC c ++ rnX x ++ rnI i
         where (m,rm) = divMod n 1000
               (c,rc) = divMod rm 100
               (x,i)  = divMod rc 10

rnC n = ["","C","CC","CCC","CD","D","DC","DCC","DCCC","CM"] !! n
rnX n = ["","X","XX","XXX","XL","L","LX","LXX","LXXX","XC"] !! n
rnI n = ["","I","II","III","IV","V","VI","VII","VIII","IX"] !! n

--8107 minimal
--8850 nonmin

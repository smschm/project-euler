import Common

a = [n | n <- [2,4..1000000], isPrime (n^2 + 1),
                              isPrime (n^2 + 3),
                              isPrime (n^2 + 7),
                              isPrime (n^2 + 9),
                              isPrime (n^2 + 13),
                              isPrime (n^2 + 27)]

import Common
import Data.List

oc n = filter (\(_,x) -> x >= 5) . occurrencesOfM . map (sort . (digitsBaseN 10) . (^3)) $ [1..n]

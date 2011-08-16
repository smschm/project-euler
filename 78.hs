import Common

cands = filter ((divides 10000) . snd) $
        zip [0..] partList

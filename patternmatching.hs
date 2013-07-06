lucky :: (Integral a) => a -> String
lucky 7 = "WOO"
lucky x = "Sorry"

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (pred n)
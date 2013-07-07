main = do
  putStrLn "Yo, what's your name?"
  name <- getLine
  putStrLn $ "Cool " ++ name ++ ", cool"

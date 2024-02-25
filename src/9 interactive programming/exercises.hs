promptForStr :: IO()
promptForStr = do   putStr "enter a word"
                    word <- getLine
                    putStr "you entered: "
                    putStr (show word)
                    
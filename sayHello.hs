--Created: February 23, 2018
--by David Osorio

--My first function: Takes a string as a parameter and prints a hello message
sayHello :: String -> IO ()
sayHello x = putStrLn ("Hello, " ++ x ++ "!")
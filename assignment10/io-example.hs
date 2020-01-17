
check :: String -> String -> IO Bool
check q a = putStrLn (q ++ " Wait...") >>
            return (a == "42")

main :: IO ()
main = 
  putStrLn "Please give a question:" >>
  getLine >>= \q ->
  putStrLn "And an answer:" >>
  getLine >>= \a ->
  if last q == '?' 
    then check q a >>= \res ->
         putStrLn $ "Your answer is " ++ show res
    else putStrLn "Your question should end with an ?. Retry!" >>
         main
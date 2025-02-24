module Main (main) where
  
import Matrix
import RrefTransformer(rref)

main :: IO ()
main = do
    random <- randomMatrix 5 6
    putStrLn ""
    putStrLn $ show random
    putStrLn "Performing RREF"
    putStrLn ""
    putStrLn $ show $ rref random

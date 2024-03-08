main :: IO ()
main = do
    putStrLn "Coloque o limite para os triplos pitagoricas: "
    limit <- readLn :: IO Int

    triples <- return (ternasPitagoricas limit)

    if null triples
        then putStrLn "Os triplos pitagoricas não encontrada dentro do limite"
        else do
            putStrLn "Triplos pitagoricas:"
            mapM_ printTriples triples

printTriples :: (Int, Int, Int) -> IO ()
printTriples (a, b, c) = putStrLn ("(a, b, c) = (" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")")

ternasPitagoricas :: (Integral a) => a -> [(a, a, a)]
ternasPitagoricas limite = [(a, b, c) | a <- [1..limite], b <- [1..limite-1], c <- [1..limite], pitagoras a b c]

pitagoras :: (Integral a) => a -> a -> a -> Bool
pitagoras a b c = (a^2 + b^2) == c^2


import Math

logBase :: Double -> Double -> Double
logBase b x = log x / log b

verificarIdentidade :: Double -> Double -> Bool
verificarIdentidade x y = abs (logBase b (x * y) - (logBase b x + logBase b y)) < epsilon

let b = 10.0

let epsilon = 1e-10

main = do
  let x = 2.5
  let y = 3.2

  if verificarIdentidade x y
    then putStrLn "A identidade log xy = log x + log y é válida para x = " ++ show x ++ " e y = " ++ show y
    else putStrLn "A identidade log xy = log x + log y não é válida para x = " ++ show x ++ " e y = " ++ show y
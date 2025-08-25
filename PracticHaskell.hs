remData :: [Int] -> Int -> Int -> [Int]
remData [] _ _ = []
remData (x:xs) a b
    | x >= a && x <= b = x : remData xs a b
    | otherwise        = remData xs a b

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

potencia :: Double -> Int -> Double
potencia _ 0 = 1
potencia b e = b * potencia b (e-1)

coseno :: Double -> Int -> Double
coseno x n = cosAux x n 0
  where
    cosAux _ 0 _ = 0
    cosAux x n k = term k + cosAux x (n-1) (k+1)
      where
        term j = ((-1) ** fromIntegral j) * (potencia x (2*j)) / fromIntegral (factorial (2*j))

dct :: [Double] -> [Double]
dct xs = [ coef k | k <- [0..n-1] ]
  where
    n = length xs
    coef k = a k * sum [ xs!!m * cos ( (fromIntegral m + 0.5) * pi * fromIntegral k / fromIntegral n ) | m <- [0..n-1] ]
    a k = if k == 0 then sqrt (1 / fromIntegral n) else sqrt (2 / fromIntegral n)

order_desc :: (Ord a) => [a] -> [a]  
order_desc [] = []   
order_desc (x:xs) = order_desc [y | y <- xs, y > x] ++ [x] ++ order_desc [y | y <- xs, y <= x] 

factorial2 :: Integer -> Integer  
factorial2 0 = 1    
factorial2 n = n * factorial2(n-1) 

power :: Double -> Integer -> Double  
power _ 0 = 1 
power base expo = base * power base (expo - 1)

abs_func :: (Num a, Ord a) => a -> a  
abs_func x = if x < 0 then -x else x

exp_func :: Double -> Integer -> Double 
exp_func 0 _ = 1.0 
exp_func _ 0 = 1.0  
exp_func x n = power x n / fromIntegral(factorial2 n) + exp_func x (n-1)

natural_log ::Double -> Integer -> Double 
natural_log x n
    | abs_func x >= 1 = error "La entrada 'x' debe estar en el rango (-1, 1)"
natural_log _ 0 = 0.0 
natural_log x n =
    let termino_n = (power (-1) (n+1) / fromIntegral n) * (power x n)
    in termino_n + natural_log x (n - 1)

main :: IO()
main = do
    putStrLn "=== remData ==="
    print (remData [3, 7, -2, 10, 5] 0 8)

    putStrLn "\n=== coseno ==="
    print (coseno (pi/4) 10)

    putStrLn "\n=== DCT ==="
    print (dct [2, 4, 6, 8])

    putStrLn "\n=== order_desc ==="
    print (order_desc [1, 25, 5,-4])

    putStrLn "\n=== factorial ==="
    print (factorial2 5)

    putStrLn "\n=== power ==="
    print (power 3 2)

    putStrLn "\n=== exp_func ==="
    print (exp_func 1 10)
    print (exp 1.0)

    putStrLn "\n=== natural_log ==="
    print (natural_log 0.5 20)
    print (log (1 + 0.5))

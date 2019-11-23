--RSA 

--Inmporto libreria che mi permetterà di generare i numeri casuali

import System.Random

foo _ = 
    return (p, q)
    where
        p = randomRIO (1, 10)
        q = p

main = do

  putStrLn "RSA"

--System.Random mi serve per estrarre degli interi tra m e n 
randomInteger :: Integer -> Integer -> IO Integer
randomInteger m n = randomRIO (m, n)

putStrLn m 
putStrLn n

-- Esegue "a mod b" con il supporto per un primo argomento negativo, 
-- ovvero (-3 mod 4) valuterà 1 invece di rimanere a -3 come farebbe `mod`
fullMod :: Integer -> Integer -> Integer
fullMod a b
    | a >= 0 = a `mod` b
    | otherwise = fullMod (a + b) b 

-- Molte volte, l'ultimo controllo ("guards" in Haskell) in una funzione è otherwise, che cattura tutto.
-- Se tutti i controlli in una funzione valutano False, e noi non abbiamo
-- fornito un controllo allora si utilizza "otherwise".
 
-- Avvia la serie di controlli nel Rabin Test ed esegue il primo controllo
rabinFirstCheck :: Integer -> Integer -> Integer -> IO Bool
rabinFirstCheck n d s =
    do
        a <- randomInteger 2 (n - 1)
        if (fullMod (a^d) n == 1) || (fullMod (a^d) n == (n - 1))
            then return True
            else return (rabinSecondCheck n d s (fullMod ((a^d)^2) n))
               

-- Si continui il Rabin Test eseguendo il secondo controllo 
rabinSecondCheck :: Integer -> Integer -> Integer -> Integer -> Bool
rabinSecondCheck n d s x
    | s <= 1 = False
    | x == 1 = False
    | x == (n - 1) = True
    | otherwise = rabinSecondCheck n d (s - 1) (x^2)

-- Performs the Test di primalità di Miller-Rabin con probabilità 4^-k 
rabinTest :: Integer -> Integer -> IO Bool
rabinTest n k = 
    rabinTestH n k True 
rabinTestH n k res
    | not res = return res
    | k <= 0 = return res
    | otherwise = 
        rabinFirstCheck n (fst ds) (snd ds)
        where
           ds = getRabinFactors n

-- Generare un numero primo random usando il Miller-Rabin Test con probabilità 4^-k
getRandomPrime :: Integer -> Integer -> Integer -> IO Integer
getRandomPrime m n k = 
    do
        x <- randomInteger m n
        r <- rabinTest x k
        if r 
            then return x
            else getRandomPrime m n k

            -- Calcola il massimo comune denominatore di aeb usando l'algoritmo di Euclide
egcd :: Integer -> Integer -> Integer
egcd a b 
    | b == 0 = a
    | otherwise = gcd b (fullMod a b)

-- Ottiene l'esponente pubblico per la coppia di chiavi RSA utilizzando i numeri primi seed p, q
getPublicExponent :: Integer -> Integer -> IO Integer
getPublicExponent p q =
    do
        e <- randomInteger 2 ((p - 1) * (q - 1) - 1)
        if gcd e ((p - 1) * (q - 1)) == 1
            then return e
            else getPublicExponent p q

-- Esegue l'algoritmo esteso di Euclide su a e b
eea :: Integer -> Integer -> (Integer, Integer)
eea a b = eeaH a b 0 1 b 1 0 a
eeaH a b s t r s' t' r' 
    | r == 0 = (s', t')
    | otherwise = eeaH a b (s' - q * s) (t' - q * t) (r' - q * r) s t r
    where
        q = quot r' r

-- Ottiene l'esponente privato per RSA utilizzando l'esponente pubblico e e i primi semi p, q
getPrivateExponent :: Integer -> Integer -> Integer -> IO Integer
getPrivateExponent e p q = 
        return (fullMod (fst (eea e ((p - 1) * (q - 1)))) 
          ((p - 1) * (q - 1)))

-- Genera coppia di chiavi pubblica (e, n ') e coppia privata (d, n') 
-- con numeri primi nell'intervallo da m a n (incluso) con precisione 4 ^ -k
getRSAKeyPairs :: Integer -> Integer -> Integer -> IO ((Integer, Integer), (Integer, Integer))
getRSAKeyPairs m n k =
    do
        p <- getRandomPrime m n k
        q <- getRandomPrime m n k
        e <- getPublicExponent p q
        d <- getPrivateExponent e p q
        return ((e, (p * q)), (d, (p * q)))

putStrLn ("Chiave pubblica: (" ++ e ++ ", " ++ (p*q) ++ "), Chiave privata: (" ++ d ++ ", " ++ (p*q) ++ ")"

--Metodo per criptare e decriptare

--Si può chiamare get KeyPair per ottenere le coppie di tasti e quindi utilizzare le seguenti funzioni.

-- Crittografa un messaggio m utilizzando la coppia di chiavi pubbliche pbk = (e, n)
  encrypt :: Integer -> (Integer, Integer) -> Integer
  encrypt m (e, n) = fullMod (m^e) n

-- Decodifica un messaggio c usando la coppia di chiavi private prk = (d, n)
  decrypt :: Integer -> (Integer, Integer) -> Integer
  decrypt c (d, n) = fullMod (c^d) n


{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Concurrent (getNumCapabilities)
import GHC.Conc (setNumCapabilities)
import Control.Concurrent.Async (mapConcurrently)
import Text.Printf (printf)
import System.IO (hFlush, stdout)
import System.Clock (Clock(Monotonic), getTime, toNanoSecs)
import Data.List (intercalate)
import Control.Exception (evaluate)
import Data.Char (isSpace, toLower)
import Control.Monad (unless)


prompt :: String -> IO String
prompt s = do
  putStr s
  putStr " "
  hFlush stdout
  line <- getLine
  pure (trim line)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

readDouble :: String -> IO Double
readDouble msg = do
  s <- prompt msg
  case reads s of
    [(x,"")] -> pure x
    _        -> putStrLn "Невірний ввід. Спробуйте ще раз." >> readDouble msg

readInt :: String -> IO Int
readInt msg = do
  s <- prompt msg
  case reads s of
    [(x::Int,"")] | x > 0 -> pure x
    _ -> putStrLn "Потрібне додатнє ціле число. Спробуйте ще раз." >> readInt msg

readEps :: String -> IO Double
readEps msg = do
  e <- readDouble msg
  if e > 0 then pure e else putStrLn "ε має бути > 0." >> readEps msg

askYesNo :: String -> IO Bool
askYesNo msg = do
  s <- prompt (msg ++ " [y/n]:")
  case map toLower s of
    "y" -> pure True
    "n" -> pure False
    _   -> putStrLn "Введіть 'y' або 'n'." >> askYesNo msg


chooseFunction :: IO (Double -> Double, String)
chooseFunction = do
  putStrLn "Оберіть функцію f(x):"
  putStrLn "  1) f(x) = sin(x)"
  putStrLn "  2) f(x) = cos(x)"
  putStrLn "  3) f(x) = exp(-x^2)"
  putStrLn "  4) f(x) = x^3 - 2x + 1"
  putStrLn "  5) Поліном користувача: f(x) = a0 + a1*x + ... + an*x^n"
  n <- readInt "Ваш вибір (1-5):"
  case n of
    1 -> pure (sin, "sin(x)")
    2 -> pure (cos, "cos(x)")
    3 -> pure (\x -> exp (-(x*x)), "exp(-x^2)")
    4 -> pure (\x -> x*x*x - 2*x + 1, "x^3 - 2x + 1")
    5 -> do
      deg <- readInt "Ступінь полінома n (наприклад, 3 означає a0..a3):"
      coeffs <- mapM (\i -> readDouble (printf "a%d = " i)) [0..deg]
      let f x = sum [a * (x ** fromIntegral k) | (k,a) <- zip [0..] coeffs]
          name = "poly(" ++ intercalate ", " (map (printf "%.6g") coeffs) ++ ")"
      pure (f, name)
    _ -> putStrLn "Невірний пункт меню." >> chooseFunction


nowNs :: IO Double
nowNs = do
  t <- getTime Monotonic
  pure (fromIntegral (toNanoSecs t) :: Double)

timeIt :: IO a -> IO (a, Double)
timeIt act = do
  t1 <- nowNs
  v  <- act
  t2 <- nowNs
  pure (v, (t2 - t1) * 1e-9)

simpsonSequential :: (Double -> Double) -> Double -> Double -> Int -> IO Double
simpsonSequential f a b n0 = do
  let n = if even n0 then n0 else n0 + 1
      h = (b - a) / fromIntegral n
      fa = f a
      fb = f b
      sumOdd = sum [ f (a + h * fromIntegral k) | k <- [1,3..(n-1)] ]
      sumEven = sum [ f (a + h * fromIntegral k) | k <- [2,4..(n-2)] ]
      res = (h/3) * (fa + fb + 4*sumOdd + 2*sumEven)
  evaluate res

mkChunks :: Int -> Int -> [(Int, Int)]
mkChunks n chunks =
  let total = n - 1
      csize = max 1 (total `div` chunks)
      go s acc
        | s > total = reverse acc
        | otherwise =
            let e = min total (s + csize - 1)
            in go (e + 1) ((s,e):acc)
  in if total <= 0 then [] else go 1 []

chunkSumSimpson :: (Double -> Double) -> Double -> Double -> Int -> (Int,Int) -> Double
chunkSumSimpson f a h n (i0, j0) =
  let s = sum [ w k * f (a + h * fromIntegral k) | k <- [i0..j0] ]
  in s
  where
    w k
      | k == 0 || k == n = error "k=0 or k=n must not occur in chunk"
      | odd k            = 4.0
      | otherwise        = 2.0

simpsonParallel :: (Double -> Double) -> Double -> Double -> Int -> Int -> IO Double
simpsonParallel f a b n0 threads = do
  let n = if even n0 then n0 else n0 + 1
      h = (b - a) / fromIntegral n
      fa = f a
      fb = f b
      chunks = max 1 threads
      ranges = mkChunks n chunks
      worker r = pure $! chunkSumSimpson f a h n r
  partials <- mapConcurrently worker ranges
  let sMid = sum partials
      res  = (h/3) * (fa + fb + sMid)
  evaluate res


convergeSimpson :: (Int -> IO Double) -> Double -> Int -> Int -> IO (Double, Int, Double)
convergeSimpson runN eps n0 nMax = loop (if even n0 then n0 else n0 + 1)
  where
    loop n
      | n > nMax = do
          v <- runN n
          pure (v, n, 1/0)
      | otherwise = do
          iN   <- runN n
          i2N  <- runN (2*n)
          let err = abs (i2N - iN) / 15.0
          if err <= eps
            then pure (i2N, 2*n, err)
            else loop (2*n)


printHeader :: IO ()
printHeader = do
  putStrLn "=============================================================="
  putStrLn "      ПАРАЛЕЛЬНЕ ОБЧИСЛЕННЯ ВИЗНАЧЕНОГО ІНТЕГРАЛУ"
  putStrLn "              (метод складеного Сімпсона)"
  putStrLn "=============================================================="
  putStrLn ""

printSummary :: String -> Double -> Double -> Double -> Int
             -> Double -> Int -> Double
             -> Double -> Int -> Double -> Double
             -> IO ()
printSummary fname a b eps threads seqVal seqN tSeq parVal parN tPar parErr = do
  putStrLn "------------------------- ПІДСУМКИ ---------------------------"
  putStrLn $ "f(x)       : " ++ fname
  putStrLn $ printf "Інтервал   : [%.8f, %.8f]" a b
  putStrLn $ printf "Точність ε : %.3g" eps
  putStrLn $ printf "Потоки     : %d" threads
  putStrLn "--------------------------------------------------------------"
  putStrLn $ printf "SEQUENTIAL : I ≈ %.12f   N = %-9d   time = %.6f s" seqVal seqN tSeq
  putStrLn $ printf "PARALLEL   : I ≈ %.12f   N = %-9d   time = %.6f s   err_est ≈ %.3g"
                       parVal parN tPar parErr
  let speedup = tSeq / tPar
      eff     = speedup / fromIntegral threads * 100
  putStrLn "--------------------------------------------------------------"
  putStrLn $ printf "Speedup    : %.3fx" speedup
  putStrLn $ printf "Efficiency : %.1f%%" eff
  putStrLn "--------------------------------------------------------------"
  putStrLn ""

main :: IO ()
main = do
  printHeader

  let startLoop = do
        (f, fname) <- chooseFunction
        a <- readDouble "Ліва межа A:"
        b <- readDouble "Права межа B:"
        let (aa, bb) = if a <= b then (a,b) else (b,a)
        eps <- readEps "Бажана точність ε (наприклад, 1e-8):"

        threads <- readInt "Кількість потоків (1..):"
        setNumCapabilities threads
        caps <- getNumCapabilities
        putStrLn $ printf "Використовую потоки/ядра: %d" caps
        putStrLn ""

        let startN   = 65536
            maxN     = 134217728
            runSeq n = simpsonSequential f aa bb n
            runPar n = simpsonParallel  f aa bb n caps

        putStrLn "Обчислення (SEQUENTIAL)..."
        ((seqVal, seqN, _), tSeq) <- timeIt (convergeSimpson runSeq eps startN maxN)

        putStrLn "Обчислення (PARALLEL)...."
        ((parVal, parN, parErr), tPar) <- timeIt (convergeSimpson runPar eps startN maxN)

        printSummary fname aa bb eps caps seqVal seqN tSeq parVal parN tPar parErr

        askYesNo "Хочете обчислити ще один інтеграл?"

  let loop = do
        shouldContinue <- startLoop
        unless shouldContinue $ putStrLn "Дякую! Гарного дня."
        if shouldContinue then loop else pure ()

  loop

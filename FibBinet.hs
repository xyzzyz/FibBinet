import System.Environment

data QSqrt5 = QS5 Rational Rational
              deriving (Show)

instance Num QSqrt5 where
  (QS5 p q) + (QS5 p' q') = (QS5 (p + p') (q + q'))
  (QS5 p q) * (QS5 p' q') = (QS5 (p*p' + 5*q*q') (p * q' + p' * q))
  x - y = x + negate y
  negate (QS5 p q) = (QS5 (-p) (-q))
  signum (QS5 p 0) = (QS5 (signum p) 0)
  signum x@(QS5 p q) | p > 0 && q > 0 = 1
                     | p < 0 && q < 0 = -1
                     | q > 0 = QS5 (signum (-p*p/q*q + 5)) 0
                     | otherwise = negate (signum (negate x))
  abs x = x * signum x
  fromInteger n = QS5 (fromInteger n) 0

instance Fractional QSqrt5 where
  fromRational r = QS5 r 0
  recip (QS5 p q) = invNorm * QS5 p (-q)
    where invNorm :: QSqrt5
          invNorm = fromRational . recip $ p*p - 5*q*q

rationalPart :: QSqrt5 -> Rational
rationalPart (QS5 a _) = a

binetFib' :: Int -> QSqrt5
binetFib' n = recip (QS5 0 1) * (phi^n - psi^n)
  where phi = 1/2 * (1 + QS5 0 1)
        psi = 1/2 * (1 - QS5 0 1)

binetFib :: Int -> Integer
binetFib = floor . rationalPart . binetFib'

main = do
  ns <- getArgs
  mapM_ (putStrLn . show . binetFib . read) ns
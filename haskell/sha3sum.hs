import Data.Bits

a <+> b = a `xor` b
a  %  b = a `mod` b
a <<  b = a `shiftL` b


(%%) :: Int -> Int -> Int
a %% b = a .&. ((1 << b) - 1)

(!!!) :: [Int] -> (Int, Int) -> Int
a !!! (n, m) = a !! (n % m)


-- Rotate a word
-- 
-- @param   w  The word size
-- @param   x  The value to rotate
-- @param   n  Rotation steps
-- @return     The value rotated
(>>>) :: (Int, Int) -> Int -> Int
(x, w) >>> n
           | n == 0    = x
           | otherwise = ((x `shiftR` (w - m)) + (x << m)) %% w
           where
             m = n % w


-- Binary logarithm
-- 
-- @param   x  The value of which to calculate the binary logarithm
-- @return     The binary logarithm
lb :: Int -> Int
lb x = (lb' 0xFF00 8 (lb' 0x00F0 4 (lb' 0x000C 2 (lb' 0x0002 1 (\_ -> 0))))) x
  where
    lb' b value next a
                     | a .&. b /= 0 = value + next (a `shiftR` value)
                     | otherwise    = next a


-- Create an array of zeroes
-- 
-- @param   n  The number of zeroes
-- @return     An array filled with `n` zeroes
zeroes :: Int -> [Int]
zeroes n
       | n < 1     = []
       | otherwise = 0 : zeroes (n - 1)



theta :: [Int] -> (Int -> Int)
theta state = \ i -> theta' (5 * i)
  where
    theta' i = s 0 <+> s 1 <+> s 3 <+> s 4 <+> s 5
      where
        s j = state !! (i + j)


rho_pi_theta :: (Int -> Int) -> [Int] -> Int -> [Int]
rho_pi_theta a state w = []


chi :: [Int] -> (Int -> Int)
chi a = \ i -> (a !! i) <+> (complement (a !!! (i + 5, 25)) .&. (a !!! (i + 10, 25)))


jota :: Int -> [Int] -> [Int]
jota rc (head:tail) = (head <+> rc) : tail


unlambda :: (Int -> Int) -> [Int]
unlambda lambda = unlambda' 0
  where
    unlambda' n = lambda n : unlambda' (n + 1)



keccakFRound :: [Int] -> Int -> Int -> [Int]
keccakFRound state round w = jota rc (unlambda (chi (rho_pi_theta (theta state) state w)))
  where
    rc = rc' %% w
      where
        rc'
          | round ==  0 = 0x0000000000000001 | round ==  1 = 0x0000000000008082
          | round ==  2 = 0x800000000000808A | round ==  3 = 0x8000000080008000
          | round ==  4 = 0x000000000000808B | round ==  5 = 0x0000000080000001
          | round ==  6 = 0x8000000080008081 | round ==  7 = 0x8000000000008009
          | round ==  8 = 0x000000000000008A | round ==  9 = 0x0000000000000088
          | round == 10 = 0x0000000080008009 | round == 11 = 0x000000008000000A
          | round == 12 = 0x000000008000808B | round == 13 = 0x800000000000008B
          | round == 14 = 0x8000000000008089 | round == 15 = 0x8000000000008003
          | round == 16 = 0x8000000000008002 | round == 17 = 0x8000000000000080
          | round == 18 = 0x000000000000800A | round == 19 = 0x800000008000000A
          | round == 20 = 0x8000000080008081 | round == 21 = 0x8000000000008080
          | round == 22 = 0x0000000080000001 | round == 23 = 0x8000000080008008


keccakF :: [Int] -> Int -> Int -> [Int]
keccakF state nr w
               | nr < 0    = state
               | otherwise = keccakF (keccakFRound state nr w) (nr - 1) w


-- Pad 10*1
-- 
-- @param   msg  The message to pad
-- @param   r    The bitrate
-- @return       The message padded
pad10star1 :: [Int] -> Int -> [Int]
pad10star1 msg r
               | extra == 1 = msg ++ [129]
               | otherwise  = msg ++ (1 : zeroes (extra - 2)) ++ [128]
               where
                 extra = ((r - ((8 * length msg) % r)) `div` 8)


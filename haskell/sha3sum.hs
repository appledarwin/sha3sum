import Data.Bits

a <+> b = a `xor` b
a  %  b = a `mod` b


-- create an array of zeroes
-- 
-- @param   n  The number of zeroes
-- @return     An array filled with `n` zeroes
zeroes :: Int -> [Int]
zeroes n
       | n < 1     = []
       | otherwise = 0 : zeroes (n - 1)


-- pad 10*1
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


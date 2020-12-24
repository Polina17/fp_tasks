module Part1
  ( prob1
  , prob2
  , prob3
  , prob4
  , prob5
  ) where

------------------------------------------------------------
-- PROBLEM #1
--
-- Реализовать функцию, которая возвращает остаток от
-- деления на 65537 суммы утроенного своего аргумента
-- и числа 123
--
-- На вход функции подаются неотрицательные числа
gf x = x*3+123
prob1 :: Int -> Int
prob1 x = gf x `mod` 65537 


------------------------------------------------------------
-- PROBLEM #2
--
-- Реализовать функцию, которая:
-- * нечётные числа увеличивает втрое и добавляет единицу
-- * чётные числа делит на два
prob2 :: Integer -> Integer
prob2 x = if (x `mod` 2 == 1) then (x*3+1)
		else (x `div` 2)

------------------------------------------------------------
-- PROBLEM #3
--
-- Реализовать функцию, которая принимает функцию step,
-- положительное число n и пока текущее число не станет
-- равно единице:
-- * вызывает step с текущим числом для получения
--   следующего числа
-- * если текущее число -- единица, возвращает количество
--   выполненных шагов
--
-- Например, если в качестве step используется уменьшение
-- на единицу, а в качестве n передать 5, то должно быть
-- возвращено 4, поскольку последовательность будет такой:
--    5 -> 4 -> 3 -> 2 -> 1
--
-- Если в качестве step передать решение prob2, а n == 3,
-- то ответ 7, а последовательность такая:
--    3 -> 10 -> 5 -> 16 -> 8 -> 4 -> 2 -> 1
--
-- Для любой функции step и n == 1 ответом будет 0.
--stepcount x = x+1
prob3 :: (Integer -> Integer) -> Integer -> Integer

func3 step n acc = if (step n == 1) then (acc+1)
                   else (func3 step (step n) (acc+1))

prob3 step 1 = 0
prob3 step n = func3 step n 0
--prob3 step n = error "Implement me!"


------------------------------------------------------------
-- PROBLEM #4
--
-- Реализовать функцию, возвращающую n-е число Фибоначчи.
-- Нулевое число равно 1, первое тоже 1. Каждое последующее
-- равно сумме двух предыдущих.
--
-- Число n может быть отрицательным, последовательность
-- продолжается естественным образом: (-1)-е число равно 0,
-- далее (-2)-е равно 1, (-3)-е равно (-1), (-4)-е равно 2
-- и т.д. -- сохраняется свойство, что последующие числа
-- равны сумме двух предыдущих.
--
-- Число n по модулю не превосходит 10^5
prob4 :: Integer -> Integer
prob4 n
  | n == (-1) = 0
  | n < 0 = prob4 (-n - 2) * (if even n then 1 else -1)
  | otherwise = prob4iter n 0 1

prob4iter :: Integer -> Integer -> Integer -> Integer
prob4iter 0 a b = b
prob4iter i a b = prob4iter (i - 1) b (a + b)


------------------------------------------------------------
-- PROBLEM #5
--
-- Написать функцию, возвращающую True, если все простые
-- делители первого аргумента n меньше второго аргумента k
--
-- Числа n и k положительны и не превосходят 10^8.
-- Число 1 не считается простым числом

isPrime :: Integer -> Bool
primes :: [Integer]

isPrime n | n < 2 = False
isPrime n = all (\p -> n `mod` p /= 0) . takeWhile ((<= n) . (^ 2)) $ primes
primes = 2 : filter isPrime [3..]

primeFactors :: Integer -> [Integer]
primeFactors n = iter n primes where
    iter n (p:_) | n < p^2 = [n | n > 1]
    iter n ps@(p:ps') =
        let (d, r) = n `divMod` p
        in if r == 0 then p : iter d ps else iter n ps'

prob5 :: Integer -> Integer -> Bool
prob5 n k = all (< k) (getPrimeDivisors n)
    where
        getPrimeDivisors :: Integer -> [Integer]
        getPrimeDivisors = getDivisorsWithCurrent 2

        getDivisorsWithCurrent :: Integer -> Integer -> [Integer]
        getDivisorsWithCurrent _ 1 = []
        getDivisorsWithCurrent divisor number
            | divisor * divisor > number = [number]
            | number `mod` divisor == 0 = divisor : getDivisorsWithCurrent divisor (number `div` divisor)
            | otherwise = getDivisorsWithCurrent (divisor + 1) number

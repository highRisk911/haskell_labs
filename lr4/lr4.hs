-- Лабораторна робота 4
-- Виконав студент групи КН-31 Іщенко Дмитро Романович
-- 2 варіант

--1.2

-- без вбудованих функцій
class Palindrome a where
  palindrome :: a -> Bool

instance Palindrome Integer where
  palindrome _ = True

instance (Eq a, Palindrome a) => Palindrome [a] where
  palindrome xs = xs == reverse xs && all palindrome xs


-- з вбудованими функціями
isPalindrome xs = and $ zipWith (==) xs (reverse xs)


--2.2

-- з вбудованими функціями
shifts xs = take (length xs) $ iterate shift xs
  where shift (x:xs) = xs ++ [x]

-- без вбудованих функцій
mixLists [] ys = ys
mixLists xs [] = xs
mixLists (x : xs) (y : ys) = ?
mixLists (x : xs) (y : ys) = x : y : ?
mixLists (x : xs) (y : ys) = x : y : mixLists xs ys

-- Висновок
-- В ході лабораторної роботи ознайомилися з особливостями декларування функцій. 
-- Написали та протестували функції для обчислення у сфері планіметрії.
-- Отримали досвід роботи з інтерпретатором GHCI.
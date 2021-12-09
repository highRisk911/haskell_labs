-- Лабораторна робота 2
-- Виконав студент групи КН-31 Іщенко Дмитро Романович
-- 2 варіант

--1.2

-- без вбудованих функцій
secondByLast (x : _ : []) = x
secondByLast (_ : xs)     = secondByLast xs


-- з вбудованими функціями
secondByLastWithTail :: [Int] -> Maybe Int
secondByLastWithTail (x:xs) | ((null xs) || null (tail xs)) = Just x
                 | otherwise                     = secondByLastWithTail(xs)
secondByLastWithTail []                                     = secondByLastWithTail

--2.2

-- з вбудованими функціями
shifts xs = take (length xs) $ iterate shift xs
  where shift (x:xs) = xs ++ [x]

-- без вбудованих функцій
shiftList :: Int -> [a] -> [a]
shiftList n [] = []
shiftList 0 x  = x
shiftList n (x:xs) = shiftList (n - 1) xs++[x]

-- Висновок
-- В ході лабораторної роботи ознайомилися з особливостями декларування функцій. 
-- Написали та протестували функції для обчислення у сфері планіметрії.
-- Отримали досвід роботи з інтерпретатором GHCI.
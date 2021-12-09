-- Лабораторна робота 1
-- Виконав студент групи КН-31 Іщенко Дмитро Романович
-- 2 варіант
-- Перша частина


-- [[(Integer,[Bool])]]
second = [[(1, [True, False, 1 == 1, 2 > 3])]]

-- Друга частина


-- Функцiя за довжиною трьох вiдрiзкiв визначає, чи можна на них побудувати прямокутний трикутник.


checkPifagorCondition :: [Double] -> Bool
checkPifagorCondition sides = (length sides == 3)
                                                && (head sides^2 + sides !! 1 ^2 == last sides^2)
                                                || (head sides^2 + last sides^2 == sides !! 1^2)
                                                || (sides !! 1^2 + last sides^2 == head sides^2)

checkPifagorConditionOrder :: Integer -> Integer -> Integer -> Bool
checkPifagorConditionOrder a b c = a^2 + b^2 == c^2 || c^2 + b^2 == a^2 || a^2 + c^2 == b^2


-- Висновок
-- В ході лабораторної роботи ознайомилися з особливостями декларування функцій. 
-- Написали та протестували функції для обчислення у сфері планіметрії.
-- Отримали досвід роботи з інтерпретатором GHCI.
-- Лабораторна робота 5
-- Виконав студент групи КН-31 Іщенко Дмитро Романович
-- 2 варіант

import Data.List
import System.IO


--Реалiзувати та скомпiлювати одну з програм, розроблених у лабора-торнiй роботi No 3 для Вашого варiанта з введенням даних: 
--а) з клавiатури, 
--б) з файлу та виведенням результатiв: 
--в) на екран, 
--г) у файл.


firstArray = [1, 2, 3, 4, 5]
secondArray = [1, 2, 3, 2, 1]

str1 = ['a'..'d']
str2 = ['a'..'d']

mergeStrings :: String -> String -> String
mergeStrings xs [] = []
mergeStrings [] ys = []
mergeStrings (x:xs) (y:ys) = if ys == [] then x:y:[]
    else if xs == [] then x:y:[]
        else (x:y:[]) ++ (mergeStrings xs ys)

ioFunc :: IO (String, String)
ioFunc = do
  putStrLn "Input first String"
  first <- getLine
  putStrLn "Input second String"
  second <- getLine
  return (first, second)


main = do
    putStrLn "put 2 strings"
    (input1, input2) <- ioFunc
    let bur = mergeStrings input1 input2
    putStrLn "Result: "
    putStrLn bur
    putStrLn "Press any key to exit"
    aaa <- getLine
    putStrLn "end"

--ghc --make lab5_1.hs


--Висновок 
--Було розроблено програмне забезпечення з метою
--Забезпечення вводу та виводу даних з вже написаних функцій
--Було опановано нові можливості непроцедурної мови

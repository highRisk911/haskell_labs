--Виконав студент групи КН-31 Іщенко Дмитро Романович
--Варіант 2

--Завдання
--Фiгури на площинi.
--Використовуються такi фiгури, як коло (центр та радiус),
--прямокутник (координати лiвої верхньої та правої нижньої точок),
--трикутник (координати вершин) та
--мiтка — label (координати лiвої нижньої точки, шрифт та рядок).
--Доступнi шрифти — Consolas, Lucida Console та Source Code Pro.

--Визначне функцiї для:
--отримання списку фiгур вказаного типу;

data Font = Consolas | LucidaConsole | SourceCodePro deriving (Eq, Show)

data Shape
  = Circle Int Int Int
  | Rectangle Int Int Int Int
  | Triangle Int Int Int Int Int Int
  | Label Int Int Font String
  deriving (Eq, Show)

getRectangles :: [Shape] -> [Shape]
getRectangles [] = []
getRectangles ((Shape x1 y1 x2 y2) : fs) = Shape x1 y1 x2 y2 : getRectangles fs
getRectangles ((Circle {}) : fs) = getRectangles fs
getRectangles ((Triangle {}) : fs) = getRectangles fs
getRectangles ((Label {}) : fs) = getRectangles fs

getCircles :: [Shape] -> [Shape]
getCircles [] = []
getCircles ((Circle x y r) : fs) = Circle x y r : getCircles fs
getCircles ((Rectangle {}) : fs) = getCircles fs
getCircles ((Triangle {}) : fs) = getCircles fs
getCircles ((Label {}) : fs) = getCircles fs

getTriangles :: [Shape] -> [Shape]
getTriangles [] = []
getTriangles ((Triangle x1 y1 x2 y2 x3 y3) : fs) = Triangle x1 y1 x2 y2 x3 y3 : getTriangles fs
getTriangles ((Rectangle {}) : fs) = getTriangles fs
getTriangles ((Circle {}) : fs) = getTriangles fs
getTriangles ((Label {}) : fs) = getTriangles fs

getLabels :: [Shape] -> [Shape]
getLabels [] = []
getLabels ((Label x y f s) : fs) = Label x y f s : getLabels fs
getLabels ((Rectangle {}) : fs) = getLabels fs
getLabels ((Triangle {}) : fs) = getLabels fs
getLabels ((Circle {}) : fs) = getLabels fs

getFigures :: String -> [Shape] -> [Shape]
getFigures str array
  | str == "Rectangle" = getRectangles array
  | str == "Circle" = getCircles array
  | str == "Triangle" = getTriangles array
  | str == "Label" = getLabels array
  | otherwise = []

--Тестування
--getFigures "Label" [(Circle 1 2 3),(Rectangle 1 1 4 4),(Triangle 1 1 4 4 9 9),(Rectangle 1 1 4 5), (Label 0 0 Consolas "hello")]
--[Label 0.0 0.0 Consolas "hello"]
--getFigures "Rectangle" [(Circle 1 2 3),(Rectangle 1 1 4 4),(Triangle 1 1 4 4 9 9),(Rectangle 1 1 4 5), (Label 0 0 Consolas "hello")]
--[Rectangle 1.0 1.0 4.0 4.0,Rectangle 1.0 1.0 4.0 5.0]
--getFigures "Triangle" [(Circle 1 2 3),(Rectangle 1 1 4 4),(Triangle 1 1 4 4 9 9),(Rectangle 1 1 4 5), (Label 0 0 Consolas "hello")]
--[Triangle 1.0 1.0 4.0 4.0 9.0 9.0]
--getFigures "Circle" [(Circle 1 2 3),(Rectangle 1 1 4 4),(Triangle 1 1 4 4 9 9),(Rectangle 1 1 4 5), (Label 0 0 Consolas "hello")]
--[Circle 1.0 2.0 3.0]

--Додаткове завдання
--перемiщення фiгури на вказаний вектор.
move :: Shape -> Int -> Int -> Shape
move (Rectangle x1 y1 x2 y2) v1 v2 = Rectangle (x1 + v1) (y1 + v2) (x2 + v1) (y2 + v2)
move (Circle x y r) v1 v2 = Circle (x + v1) (y + v2) r
move (Label x y f s) v1 v2 = Label (x + v1) (y + v2) f s
move (Triangle x1 y1 x2 y2 x3 y3) v1 v2 = Triangle (x1 + v1) (y1 + v2) (x2 + v1) (y2 + v2) (x3 + v1) (y3 + v2)

--Тестування
--move (Triangle 1 1 4 4 9 9) 2 2
--Triangle 3 3 6 6 11 11
--move (Circle 1 2 3) 3 1
--Circle 4 3 3
--move (Rectangle 1 1 4 4) 5 6
--Rectangle 6 7 9 10
--move (Label 2 4 Consolas "hello") (-1) (-2)
--Label 1 2 Consolas "hello"


--Висновки
--Було створено класи та методи для роботи з класами. Ознайомлені зі стандартними класами Haskell.
--Створювали колекції та працювали з їх елементами

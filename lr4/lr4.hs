-- ����������� ������ 4
-- ������� ������� ����� ��-31 ������ ������ ���������
-- 2 �������

--1.2

-- ��� ���������� �������
class Palindrome a where
  palindrome :: a -> Bool

instance Palindrome Integer where
  palindrome _ = True

instance (Eq a, Palindrome a) => Palindrome [a] where
  palindrome xs = xs == reverse xs && all palindrome xs


-- � ����������� ���������
isPalindrome xs = and $ zipWith (==) xs (reverse xs)


--2.2

-- � ����������� ���������
shifts xs = take (length xs) $ iterate shift xs
  where shift (x:xs) = xs ++ [x]

-- ��� ���������� �������
mixLists [] ys = ys
mixLists xs [] = xs
mixLists (x : xs) (y : ys) = ?
mixLists (x : xs) (y : ys) = x : y : ?
mixLists (x : xs) (y : ys) = x : y : mixLists xs ys

-- ��������
-- � ���� ������������ ������ ������������ � ������������� ������������ �������. 
-- �������� �� ������������ ������� ��� ���������� � ����� �����������.
-- �������� ������ ������ � ��������������� GHCI.
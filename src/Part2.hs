module Part2 where

import Part2.Types
import Data.Maybe
import Data.Function ((&))

------------------------------------------------------------
-- PROBLEM #6
--
-- Написать функцию, которая преобразует значение типа
-- ColorLetter в символ, равный первой букве значения
prob6 :: ColorLetter -> Char
prob6 RED = 'R'
prob6 GREEN = 'G'
prob6 BLUE = 'B'

------------------------------------------------------------
-- PROBLEM #7
--
-- Написать функцию, которая проверяет, что значения
-- находятся в диапазоне от 0 до 255 (границы входят)
prob7 :: ColorPart -> Bool
prob7 (Red a) = if (a <= 255) && (a >= 0) then True else False
prob7 (Green a) = if (a <= 255) && (a >= 0) then True else False
prob7 (Blue a) = if (a <= 255) && (a >= 0) then True else False

------------------------------------------------------------
-- PROBLEM #8
--
-- Написать функцию, которая добавляет в соответствующее
-- поле значения Color значение из ColorPart
prob8 :: Color -> ColorPart -> Color
prob8 (Color x y z) (Red xi) = (Color (x+xi) y z)
prob8 (Color x y z) (Green yi) = (Color x (y+yi) z)
prob8 (Color x y z) (Blue zi) = (Color x y (z+zi))

------------------------------------------------------------
-- PROBLEM #9
--
-- Написать функцию, которая возвращает значение из
-- ColorPart
prob9 :: ColorPart -> Int
prob9 (Red a) = a
prob9 (Blue a) = a
prob9 (Green a) = a

------------------------------------------------------------
-- PROBLEM #10
--
-- Написать функцию, которая возвращает компонент Color, у
-- которого наибольшее значение (если такой единственный)
prob10 :: Color -> Maybe ColorPart
prob10 (Color x y z) | (x > y) && (x > z) = Just (Red x)
                     | (y > x) && (y > z) = Just (Green x)
                     | (z > y) && (z > x) = Just (Blue x)
                     | otherwise = Nothing

------------------------------------------------------------
-- PROBLEM #11
--
-- Найти сумму элементов дерева
prob11 :: Num a => Tree a -> a
--prob11 = error "Implement me!"
prob11 (Tree left el right) = el + 
		(if isJust left then (prob11 $ fromJust left) else 0) + 
		(if isJust right then (prob11 $ fromJust right) else 0)


------------------------------------------------------------
-- PROBLEM #12
--
-- Проверить, что дерево является деревом поиска
-- (в дереве поиска для каждого узла выполняется, что все
-- элементы левого поддерева узла меньше элемента в узле,
-- а все элементы правого поддерева -- не меньше элемента
-- в узле)
getRoot (Tree left el right) = el

prob12 :: Ord a => Tree a -> Bool

prob12 (Tree left el right) | (isJust left && isJust right) = if (getRoot (fromJust left)) < el && (getRoot (fromJust right)) > el then 
				(prob12 (fromJust left) && prob12 (fromJust right)) else False
                    | (isJust left && isNothing right) = if (getRoot (fromJust left)) < el then prob12 (fromJust left) else False
                    | (isNothing left && isJust right) = if (getRoot (fromJust right)) > el then prob12 (fromJust right) else False
                    | (isNothing left && isNothing right) = True

------------------------------------------------------------
-- PROBLEM #13
--
-- На вход подаётся значение и дерево поиска. Вернуть то
-- поддерево, в корне которого находится значение, если оно
-- есть в дереве поиска; если его нет - вернуть Nothing
getLeft  (Tree left el right) = left
getRight (Tree left el right) = right

prob13 :: Ord a => a -> Tree a -> Maybe (Tree a)
prob13 x tree | getRoot tree == x = Just tree
              | not (getRoot tree == x) && (isNothing (getLeft tree)) && (isNothing (getRight tree)) = Nothing
              | isJust (getRight tree) && isNothing (getLeft tree) = if (not ((prob13 x right) == Nothing)) then (prob13 x right) else Nothing
              | isNothing (getRight tree) && isJust (getLeft tree) = if (not ((prob13 x left) == Nothing)) then (prob13 x left) else Nothing
              | isJust (getRight tree) && isJust (getLeft tree) = if (not ((prob13 x left) == Nothing)) then (prob13 x left) else if (not ((prob13 x right) == Nothing)) then (prob13 x right) else Nothing
                      where
                         left  = fromJust (getLeft  tree)
                         right = fromJust (getRight tree)

------------------------------------------------------------
-- PROBLEM #14
--
-- Заменить () на числа в порядке обхода "правый, левый,
-- корень", начиная с 1
prob14 :: Tree () -> Tree Int
prob14 t = case enumerate (Just t) 1 of
    (Just enumerated, _) -> enumerated

enumerate :: Maybe (Tree ()) -> Int -> (Maybe (Tree Int), Int)
enumerate Nothing i = (Nothing, i)
enumerate (Just (Tree l () r)) i = (Just $ Tree l' current r', current + 1)
    where
        (r', afterRight) = enumerate r i
        (l', afterLeft) = enumerate l afterRight
        current = afterLeft

------------------------------------------------------------
-- PROBLEM #15
--
-- Выполнить вращение дерева влево относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob15 :: Tree a -> Tree a
prob15 tree = maybe tree leftRotation $ tree & right
    where
        leftRotation rightSubTree = rightSubTree { left = Just oldRoot }
            where
                oldRoot = tree { right = rightSubTree & left }

------------------------------------------------------------
-- PROBLEM #16
--
-- Выполнить вращение дерева вправо относительно корня
-- (https://en.wikipedia.org/wiki/Tree_rotation)
prob16 :: Tree a -> Tree a
prob16 tree = maybe tree rightRotation $ tree & left
    where
        rightRotation leftSubTree = leftSubTree { right = Just oldRoot }
            where
                oldRoot = tree { left = leftSubTree & right }

------------------------------------------------------------
-- PROBLEM #17
--
-- Сбалансировать дерево поиска так, чтобы для любого узла
-- разница высот поддеревьев не превосходила по модулю 1
-- (например, преобразовать в полное бинарное дерево)
prob17 :: Tree a -> Tree a
prob17 = error "Implement me!"

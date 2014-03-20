module World where

-- main = putStrLn "Hello World"

import Creatures

data Point = Point Int Int
 deriving(Show,Eq)

-- надо решить, что будет наш Мир.
-- Ну координаты это понятно, но что ещё?
-- По идее там ещё будут объекты...хм

data Distance = Distance Int
 deriving(Show,Eq)

data World = World [(Maybe Human,Point)]
 deriving(Show,Eq)

world = World [(Just Human, Point 1 1), (Just Human, Point 4 4), (Nothing, Point 3 3)]
module Weapon where

{- 
Так, попробуем нарисовать картину этого мира.

Устройство мира.

Сама по себе карта, есть тайлы. Пока ограничусь одним уровнем. Позже можно будет добавить третью координату.
Тайлам нужна функция проверки - занят ли он.
Каждому объекту - координаты.
Но где хранить эту информацию?
В каждый объект её встраивать видимо придется.

Мысля: на карте мира хранить только существ, не умеющих двигаться, с их координатами.

Так, виды тайлов пока будут просто трава, земля, вода, воздух и лава:).

Вспомогательные типы данных:
Имя
Масса
Размер (Рост)
Координаты (точка)



Классы типов:
Movable
Существа
	Люди
	и много других, но сперва опробую этих.
Оружие
Свитки
Заклинания

Одежда
	для рук
	для ног
	торс
	голова
	эм... штаны)
Кольца
Амулеты



-}

data Tile = Water | Ground | Air | Lava
 deriving(Eq)

instance Show Tile where
	show Water = "w"
	show Ground = "."
	show Air = "O"
	show Lava = "l"


data Name = Name String
 deriving(Show,Eq)



data Human = Human Name Coordinate
 deriving(Show,Eq)

instance HumanRace Human where
	func = undefined

instance Movable Human where
	move (Human n c) cn = Human n cn

instance Creature Human where
	attack a b = undefined

class Movable a where
	move::a->Coordinate->a -- кто, откуда и куда двигается.


class Creature a where -- класс существ
	-- move::a->Point -- кто и куда двигается.
	attack::(Weapon b)=>a->b->Point -- по идее существо которое бьёт хотя зачем..., чем бьём, куда бьём 

class (Creature a,Movable a)=>HumanRace a where
	func::a


main = putStrLn "Hello World"

data Point = Point Int Int
 deriving(Show,Eq)

data Coordinate = Coordinate Point
 deriving(Show,Eq)


data Distance = Distance Int
 deriving(Show,Eq)

data World = World [(Tile,Coordinate)]
 deriving(Eq)

instance Show World where
	show (World []) = ""
	show (World x) = (show t) ++ (show $World $tail x)
		where t = fst$ head x

worldMap::World
worldMap = World [(Water,Coordinate (Point 0 0)),(Water,Coordinate (Point 0 1)), (Ground,Coordinate (Point 0 2)),
	 (Ground,Coordinate (Point 1 0)),(Ground,Coordinate (Point 1 1)),(Ground,Coordinate (Point 1 2))]

data Damage = Damage Int
 deriving(Show,Eq,Ord)
data Weight = Weight Double
 deriving(Show,Eq,Ord)



class Weapon a where -- класс оружия
	hit :: a ->Damage -- им можно бить, возвращает нанесенный урон
	throw:: a->Distance -- его можно метать, возвращает как далеко удалось бросить.

data Knife = Knife Damage Weight
 deriving(Show)

data Sword = Sword Damage Weight
 deriving(Show)

instance Weapon Knife where
	hit (Knife d _) =  d 
	throw (Knife _ w) = Distance $15


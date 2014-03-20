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
А вот координаты двигающихся существ будут храниться у них самих.
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

data Health = Health Int
 deriving(Show,Eq)

data Human = Human Name Health Coordinate
 deriving(Eq)

instance Show Human where
	show (Human (Name s) _ _) = show $ head s

instance HumanRace Human where
	func = undefined

instance Movable Human where
	move (Human n h c) cn = Human n h cn

instance Creature Human where
	attack a b = undefined

class Movable a where
	move::a->Coordinate->a -- кто, откуда и куда двигается.


class Creature a where -- класс существ
	-- move::a->Point -- кто и куда двигается.
	attack::(Weapon b)=>a->b->Coordinate->Bool -- по идее существо которое бьёт хотя зачем..., чем бьём, куда бьём 
	attack a b (Coordinate (Point x y)) = undefined -- getDamage b

class (Creature a,Movable a)=>HumanRace a where
	func::a

action = attack h1 knife (Coordinate (Point 0 3))

knife = Knife (Damage 10) (Weight 0.1)

main = putStrLn "Hello World"

data Point = Point Int Int
 deriving(Show,Eq)

data Coordinate = Coordinate Point
 deriving(Show,Eq)

h1=Human (Name "Vaska") (Health 100) (Coordinate (Point 0 2))
h2=Human (Name "Petka") (Health 100) (Coordinate (Point 0 3))

creatures = [h1, h2]
{-
findCreatureByCoordinate c creatures = findCreatureByCoordinateHelper c creatures 0

findCreatureByCoordinateHelper (Coordinate (Point x y)) [] position = Nothing
findCreatureByCoordinateHelper (Coordinate (Point x y)) creatures position = 
	if ((x,y) == getCoordinate $head creatures)
	then Just position
	else findCreatureByCoordinateHelper (Coordinate(Point x y)) (tail creatures) (position+1)


getCoordinate::Human->(Int,Int)
getCoordinate Human _ _ (Coordinate (Point x y)) = (x,y)
-}
data Distance = Distance Int
 deriving(Show,Eq)

data World = World [(Tile,Coordinate)]
 deriving(Eq)

instance Show World where
	show (World []) = ""
	show (World x) = (show t) ++ (show $World $tail x)
		where t = fst$ head x

emptyMap width = concat $replicate width "X"
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
	getDamage::a->Int -- возвращает величину урона

data Knife = Knife Damage Weight
 deriving(Show)

data Sword = Sword Damage Weight
 deriving(Show)

instance Weapon Knife where
	hit (Knife d _) =  d 
	throw (Knife _ w) = Distance $15
	getDamage (Knife (Damage d) _) = d


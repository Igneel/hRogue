module Creatures where

import Weapon

data Human = Human
 deriving(Show,Eq)

class Creature a where -- класс существ
	move::a->Point -- кто и куда двигается.
	attack::(Weapon b)=>a->b->Point -- по идее существо которое бьёт хотя зачем..., чем бьём, куда бьём 
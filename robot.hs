robot :: (String, Integer, Integer) -> ((String, Integer, Integer) -> t) -> t
robot (name, attack, hp) function = function (name, attack, hp)

name :: (String, Integer, Integer) -> String
name (n, _, _) = n

attack :: (String, Integer, Integer) -> Integer
attack (_, a, _) = a

--hp :: (String, Integer, Integer) -> Integer
hp (_, _, hp) = hp

getName :: (((String, Integer, Integer) -> String) -> String) -> String
getName aRobot = aRobot name

getAttack :: (((String, Integer, Integer) -> Integer) -> Integer) -> Integer
getAttack aRobot = aRobot attack

--getHp :: (((String, Integer, Integer) -> Integer) -> Integer) -> Integer
getHp aRobot = aRobot hp

geeRobot :: ((String, Integer, Integer) -> t) -> t
geeRobot = robot ("Gee", 10, 100)
mariaRobot :: ((String, Integer, Integer) -> t) -> t
mariaRobot = robot ("Maria", 20, 60)
theaRobot :: ((String, Integer, Integer) -> t) -> t
theaRobot = robot ("Thea", 8, 140)
aiwaRobot :: ((String, Integer, Integer) -> t) -> t
aiwaRobot = robot ("Aiwa", 5, 200)
ireneRobot :: ((String, Integer, Integer) -> t) -> t
ireneRobot = robot ("Irene", 15, 80)
gigiRobot :: ((String, Integer, Integer) -> t) -> t
gigiRobot = robot ("Gigi", 25, 50)
louieRobot :: ((String, Integer, Integer) -> t) -> t
louieRobot = robot ("Louie", 12, 90)

setName
    :: (  (  (String, Integer, Integer)
         -> ((String, Integer, Integer) -> t1)
         -> t1
         )
       -> t2
       )
    -> String
    -> t2
setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))

setAttack
    :: (  (  (String, Integer, Integer)
         -> ((String, Integer, Integer) -> t1)
         -> t1
         )
       -> t2
       )
    -> Integer
    -> t2
setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))

setHp
    :: (  (  (String, Integer, Integer)
         -> ((String, Integer, Integer) -> t1)
         -> t1
         )
       -> t2
       )
    -> Integer
    -> t2
setHp aRobot newHp = aRobot (\(n, a, h) -> robot (n, a, newHp))

printRobot :: (((String, Integer, Integer) -> String) -> String) -> String
printRobot aRobot = aRobot
    (\(n, a, h) -> "Name: " ++ n ++ " ,Attack: " ++ show a ++ " ,HP: " ++ show h
    )

damage aRobot damageDealt = setHp aRobot (remainingHealth (getHp aRobot) damageDealt)

remainingHealth :: Integer -> Integer -> Integer
remainingHealth health damageDealt | damageDealt >= health = 0
                                   | otherwise = health - damageDealt

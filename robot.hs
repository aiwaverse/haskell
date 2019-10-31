type RobotInfo = (String, Integer, Integer)

robot :: RobotInfo -> (RobotInfo -> t) -> t
robot (name, attack, hp) function = function (name, attack, hp)

name :: RobotInfo -> String
name (n, _, _) = n

attack :: RobotInfo -> Integer
attack (_, a, _) = a

hp :: RobotInfo -> Integer
hp (_, _, hp) = hp

getName :: ((RobotInfo -> String) -> String) -> String
getName aRobot = aRobot name

getAttack :: ((RobotInfo -> Integer) -> Integer) -> Integer
getAttack aRobot = aRobot attack

getHp :: ((RobotInfo -> Integer) -> Integer) -> Integer
getHp aRobot = aRobot hp

geeRobot :: (RobotInfo -> t) -> t
geeRobot = robot ("Gee", 10, 100)
mariaRobot :: (RobotInfo -> t) -> t
mariaRobot = robot ("Maria", 20, 60)
theaRobot :: (RobotInfo -> t) -> t
theaRobot = robot ("Thea", 8, 140)
aiwaRobot :: (RobotInfo -> t) -> t
aiwaRobot = robot ("Aiwa", 5, 200)
ireneRobot :: (RobotInfo -> t) -> t
ireneRobot = robot ("Irene", 15, 80)
gigiRobot :: (RobotInfo -> t) -> t
gigiRobot = robot ("Gigi", 25, 50)
louieRobot :: (RobotInfo -> t) -> t
louieRobot = robot ("Louie", 12, 90)

setName :: ((RobotInfo -> (RobotInfo -> t1) -> t1) -> t2) -> String -> t2
setName aRobot newName = aRobot (\(n, a, h) -> robot (newName, a, h))

setAttack :: ((RobotInfo -> (RobotInfo -> t1) -> t1) -> t2) -> Integer -> t2
setAttack aRobot newAttack = aRobot (\(n, a, h) -> robot (n, newAttack, h))

setHp :: ((RobotInfo -> (RobotInfo -> t1) -> t1) -> t2) -> Integer -> t2
setHp aRobot newHp = aRobot (\(n, a, h) -> robot (n, a, newHp))

printRobot :: ((RobotInfo -> String) -> String) -> String
printRobot aRobot = aRobot
    (\(n, a, h) -> "Name: " ++ n ++ " ,Attack: " ++ show a ++ " ,HP: " ++ show h)

compareHealth :: ((RobotInfo -> Integer) -> Integer) -> Integer -> Ordering
compareHealth aRobot = compare (getHp aRobot)


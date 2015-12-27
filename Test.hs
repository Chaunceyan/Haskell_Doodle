import Data.List.Split

data Identity = Teacher | Student | Administrator deriving (Show, Eq)

data User = User { login :: String, token :: String, identity :: Identity } deriving (Show, Eq)

checkLogin loginInfo users = any (userLoginTest (splitOn " " $ loginInfo)) users
 
userLoginTest [myLogin, myToken] user = if (login user) == myLogin && (token user) == myToken
                                then True
                                else False

module Simulacro where

validacionDual :: (String, String) -> (String, String) -> Bool
validacionDual (a,b) (x, y) = a/=b && (a/=x || b/=y) && (a/=y || b/=x) 

notContiene :: [(String, String)] -> (String, String) -> Bool
notContiene [] (a, b) = a/=b
notContiene ((a,b):x) (n, m) = validacionDual (a,b) (n,m) && notContiene x (n, m)

relacionesValidas :: [(String, String)] -> Bool
relacionesValidas [] = True
relacionesValidas ((a,b):x) = notContiene x (a,b) && relacionesValidas x

-----------------------------------------------------------------------------

isIn :: (Eq t) => [(t,t)] -> t -> Bool
isIn [] n = False
isIn ((a,b):xs) n = a==n || b==n || isIn xs n 


personas :: [(String, String)] -> [String]
personas [] = []
personas ((a,b):x)| x == [] = [a,b]
                  | isIn x a && isIn x b  = personas x
                  | isIn x a = personas x ++ [b]
                  | isIn x b = personas x ++ [a]
                  | otherwise = personas x ++ [a,b]


amigosDe :: String -> [(String, String)] -> [String]
amigosDe "nadie" [] = ["nadie"]
-- esta mal. lo tenes que corregir

personaConMasAmigos :: [(String, String)] -> String
personaConMasAmigos [] = "yo"
--comentario de test
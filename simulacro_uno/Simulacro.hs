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


-----------------------------------------------------------------
--devuelve el nombre distinto a n 
noN :: (String, String) -> String -> String
noN (a,b) n | a==n = b 
            | otherwise = a


amigosDe :: String -> [(String, String)] -> [String]
amigosDe n [] = []
amigosDe n ((a,b) : x) | isIn [(a,b)] n = amigosDe n x ++ [noN (a,b) n]
                       | isIn x n = amigosDe n x
                       | otherwise = []
----------------------------------------------------------------


tamañoLista :: [t] -> Integer    
tamañoLista [] = 0
tamañoLista (x:xs) = 1 + tamañoLista xs


cantidadDeAmigos :: String -> [(String, String)] -> Integer
cantidadDeAmigos n x = tamañoLista (amigosDe n x)

esMasAmigableQue :: String -> String -> [(String, String)] -> Bool
esMasAmigableQue a b x = cantidadDeAmigos a x >= cantidadDeAmigos b x 


masAmigosAux :: [(String, String)] -> [(String, String)] -> String -> String
masAmigosAux x [] masAmigable = masAmigable
masAmigosAux  x ((a,b):cola) masAmigable | esMasAmigableQue masAmigable a x && esMasAmigableQue masAmigable b x = masAmigosAux x cola masAmigable
                                         | esMasAmigableQue b a x = masAmigosAux x cola b
                                         | otherwise = masAmigosAux x cola a
 
personaConMasAmigos :: [(String, String)] -> String
--personaConMasAmigos ((a,b):x) = masAmigosAux ((a,b):x) ((a,b):x) a
personaConMasAmigos x = masAmigosAux x x (fst (head x))
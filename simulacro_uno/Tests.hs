import Test.HUnit
import Simulacro

test1 = test [
    "test sintaxis" ~: (relacionesValidas [] || not (relacionesValidas [])) ~?= True,
    "relaciones vacias" ~: relacionesValidas [] ~?= True,
    "componentes duplicadas 1" ~: relacionesValidas duplicados1 ~?= False,
    "componentes duplicadas 2" ~: relacionesValidas duplicados2 ~?= False,
    "validas de 1 elemento" ~: relacionesValidas validas1 ~?= True,
    "validas de n elementos" ~: relacionesValidas validas2 ~?= True
    ]

test2 = test [
    "vacio" ~: personas [] ~?= [],
    "personas 1 elemento" ~:  sonIguales_hunit (personas validas1) ["X", "Z"] ~?= True, -- en algun orden
    "personas n elementos" ~: sonIguales_hunit (personas validas2) ["X", "Z", "P", "Q", "A", "B"] ~?= True,
    "personas n elementos repetidos" ~: sonIguales_hunit (personas validas3) ["X", "Z", "Q", "B"] ~?= True
    ]

test3 = test [
    "vacio" ~: amigosDe "A" [] ~?= [],
    "sin amigos" ~: amigosDe "A" validas1 ~?= [],
    "amigosDe 1 elemento" ~: sonIguales_hunit (amigosDe "A" validas2) ["B"] ~?= True,
    "amigosDe n elementos" ~: sonIguales_hunit (amigosDe "A" validas3) ["B", "X", "Z"] ~?= True
    ]

test4 = test [
    "personaConMasAmigos 1 elemento" ~: pertenece_hunit (personaConMasAmigos validas1) (aplanar_hunit validas1) ~?= True,
    "personaConMasAmigos empate" ~: pertenece_hunit (personaConMasAmigos validas2) (aplanar_hunit validas2) ~?= True,
    "personaConMasAmigos" ~: pertenece_hunit (personaConMasAmigos validas3) (aplanar_hunit validas3) ~?= True,
    "personaConMasAmigos2" ~: personaConMasAmigos validas4 ~?= "A"
    ]


--Formulas
quitar_hunit :: (Eq t) => t -> [t] -> [t]
quitar_hunit x (y:ys) | x == y = ys
                | otherwise = y : quitar_hunit x ys


incluido_hunit :: (Eq t) => [t] -> [t] -> Bool
incluido_hunit [] l = True
incluido_hunit (x:c) l = elem x l && incluido_hunit c (quitar_hunit x l)


sonIguales_hunit :: (Eq t) => [t] -> [t] -> Bool
sonIguales_hunit xs ys = incluido_hunit xs ys && incluido_hunit ys xs


pertenece_hunit :: (Eq t) => t -> [t] -> Bool
pertenece_hunit _ [] = False
pertenece_hunit x (y:ys) | x == y = True
                   | otherwise = pertenece_hunit x ys


aplanar_hunit :: [(t, t)] -> [t]
aplanar_hunit [] = []
aplanar_hunit ((a, b):xs) = a:(b:(aplanar_hunit xs))


--Listas

duplicados1 = [("X", "Z"), ("P", "Q"), ("A", "A")]
duplicados2 = [("X", "Z"), ("P", "Q"), ("A", "B"), ("Z", "X")]
validas1 = [("X", "Z")]
validas2 = [("X", "Z"), ("P", "Q"), ("A", "B")]

validas3 = [("X", "Z"), ("Z", "Q"), ("Q", "B")]

validas4 = [("A", "B"), ("X", "A"), ("Z", "A"), ("X", "Z"), ("P", "Q")]


-- -- EJEMPLOS

-- USUARIO1 = "JUAN"
-- USUARIO2 = "NATALIA"
-- USUARIO3 = "PEDRO"

-- RELACION1_2 = (USUARIO1, USUARIO2)
-- RELACION1_1 = (USUARIO1, USUARIO1)
-- RELACION1_3 = (USUARIO1, USUARIO3)


-- -- FUNCIONES PARA TESTING, NO BORRAR
-- -- EXPECTANY PERMITE SABER SI EL ELEMENOT QUE DEVUELVE LA FUNCIÓN ES ALGUNO DE LOS ESPERADOS
-- EXPECTANY ACTUAL EXPECTED = ELEM ACTUAL EXPECTED ~? ("EXPECTED ANY OF: " ++ SHOW EXPECTED ++ "\N BUT GOT: " ++ SHOW ACTUAL)


-- -- SONIGUALES PERMITE VER QUE DOS LISTAS SEAN IGUALES SI NO IMPORTA EL ORDEN
-- QUITAR :: (EQ T) => T -> [T] -> [T]
-- -- REQUIERE X PERTENECE A Y
-- QUITAR X (Y:YS)
-- | X == Y = YS
-- | OTHERWISE = Y : QUITAR X YS

-- INCLUIDO :: (EQ T) => [T] -> [T] -> BOOL
-- INCLUIDO [] L = TRUE
-- INCLUIDO (X:C) L = ELEM X L && INCLUIDO C (QUITAR X L)

-- SONIGUALES :: (EQ T) => [T] -> [T] -> BOOL
-- SONIGUALES XS YS = INCLUIDO XS YS && INCLUIDO YS XS 

import Test.HUnit
import Simulacro

runTestRelacionesValidas = runTestTT testsRelacionesValidas

-- "nombre" ~: (funcion parametros) ~?= resultado_esperado

testsRelacionesValidas = test [  
  "test dual 4 " ~: (validacionDual ("ana", "pedro") ("ana", "pedro")) ~?= False,
  "test dual 1 " ~: (validacionDual ("ana", "pedro") ("ana", "pedro")) ~?= False,
  "componentes repetidas" ~: (relacionesValidas [("ana", "ana")]) ~?= False,
  "tupla repetida" ~: (relacionesValidas [("ana", "pedro"), ("ana", "pedro")]) ~?= False,
  "test relaciones 1" ~: (relacionesValidas [("ana", "pedro"), ("ana", "juan")]) ~?= True,
  "test relaciones 2" ~: (relacionesValidas [("ana", "pedro"), ("pepe", "pedro"), ("juan", "pedro"), ("ana", "pepe")]) ~?= True,
  "test relaciones 3" ~: (relacionesValidas [("ana", "pedro"), ("pepe", "pedro"), ("juan", "pedro"), ("pedro", "ana")]) ~?= False,
  "test relaciones 3" ~: (relacionesValidas [("ana", "pedro"), ("pepe", "pedro"), ("juan", "pedro"), ("pedro", "ana")]) ~?= False,
  "test relaciones 3" ~: (relacionesValidas [("ana", "pedro"), ("pepe", "pedro"), ("olga", "olga"), ("olga", "ana")]) ~?= False,
  "test relaciones 3" ~: (relacionesValidas [("ana", "pedro"), ("pepe", "pedro"), ("olga", "juan"), ("olga", "ana")]) ~?= True,
  "tupla repetida invertida" ~: (relacionesValidas [("ana", "pedro"), ("pedro", "ana")]) ~?= False,
  "todas diferentes" ~: (relacionesValidas [("ana", "pedro"), ("ana", "carlos")]) ~?= True
  ]

list_relaciones1 = [("ana", "pedro"), ("pepe", "pedro"), ("juan", "pedro"), ("ana", "pepe")]

test1 = TestCase (assertBool "test Bool Assert " )

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

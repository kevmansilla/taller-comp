module Main where

import Lab


{- test 1-}
prog1 = Assign "x" (Const 8)
ejemplo1 = eval ["x"] prog1 eIniTest

{- test 2 -}
{- Debe devolver 4 en "x" y 5 en "y" -}
prog2 = Seq
  (Seq
    (Assign "x" (Const 3))
    (Assign "y" (Const 5))
  )
  (Assign "x" (Div (Plus (Var "x") (Var "y")) (Const 2)))

ejemplo2 = eval ["x", "y"] prog2 eInicial

{- test 3 -}
{- Este programa debe comportarse como Skip -}
prog3 =
  Catch
    (Local "x" (Const 7) Fail)
    Skip

ejemplo3 = eval ["x"] prog3 eIniTest

{- test 4 -}
{- Division y resto -}
progDivMod =
  If
    (Or
      (Or (Less (Var "y") (Const 0)) (Eq (Var "y") (Const 0)))
      (Less (Var "x") (Const 0))
    )
    Fail
    (Local "q" (Const 0)
      (Local "r" (Var "x")
        (Seq
          (Seq
            (While
              (Not (Less (Var "r") (Var "y")))
              (Seq
                (Assign "q" (Plus (Var "q") (Const 1)))
                (Assign "r" (Dif (Var "r") (Var "y")))
              )
            )
            (Assign "x" (Var "q"))
          )
          (Assign "y" (Var "r"))
        )
      )
    )

{- Ejecuta el programa de división entera a/b con a en "x" y b en "y". Devuelve
	el cociente en "x" y el resto en "y".
    Si "x" < 0 o "y" <= 0, aborta dejando los valores iniciales de "x" e "y".
-}
ejemploDivMod a b = eval ["x", "y"] progDivMod $ 
  update (update eInicial "x" a) "y" b

{- test 5 -}
testLocal =
  Local "x" (Const 10) (
    Seq
      (Assign "x" (Plus (Var "x") (Const 1))) -- x := x + 1
      (Assign "y" (Var "x")) -- y := x
  )

ejemplo5 = eval ["x", "y"] testLocal eIniTest
-- veo el valor de x en local y en global

{- Main test -}
main :: IO ()
main = do
  putStrLn "Ejemplo 1: Asignar x = 8"
  ejemplo1
  putStrLn "\nEjemplo 2: Asignar x = 3, y = 5, luego guarda en x el resultado de (3+5)/2 = 4"
  ejemplo2
  putStrLn "\nEjemplo 3: al inicio x=7 localmente, pero al salir del scope se restaura el valor anterior de x en el estado inicial que es 0"
  ejemplo3
  putStrLn "\nEjemplo DivMod: Dividir x = 10, y = 2 -> x = 5, y = 0"
  ejemploDivMod 10 2
  putStrLn "\nEjemplo DivMod: Dividir x = -10 y = 2 -> aborta (deja valores iniciales)"
  ejemploDivMod (-10) 2
  putStrLn "\nEjemplo: Local: x = 10, luego x = x + 1, y = x -> x = 0, y = 11"
  ejemplo5

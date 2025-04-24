# Laboratorio Lenguajes y Compiladores

Para ver las ecuaciones de la semántica denotacional ver el archivo `doc.pdf` 
del repo

## Usage

Para correr los test del proyecto

```
make run
```

Para limpiar el directorio de trabajo

```
make clean
```

## Test

El programa en el paso 1 asigna 3 a $x$ y 5 a $y$. Luego en el paso 2 asigna a $x$ el resultado de la expresión $((3 + 5) / 2)$, que es 4. 
```
prog2 = Seq
  (Seq
    (Assign "x" (Const 3))
    (Assign "y" (Const 5))
  )
  (Assign "x" (Div (Plus (Var "x") (Var "y")) (Const 2)))
```

$x$ estaba localmente en $\sigma_{1}$ con valor 7, pero al salir del scope se 
restaura el valor anterior de $x$ en $\sigma_{0}$, que es 0.
```
prog3 =
  Catch
    (Local "x" (Const 7) Fail)
    Skip
```

El programa:
- inicialmente $x$ es 0
- luego se le asigna 10 en el scope local
- luego se le asigna $x:= x + 1$ entonce queda en $11$
- guardo el valor de $x$ en $y$
- finalmente se sale del scope y se restaura el valor de $x$ a 0
- imprimo x e y, los valores esperados son 0 y 11 respectivamente
```
testLocal =
  Local "x" (Const 10) (
    Seq
      (Assign "x" (Plus (Var "x") (Const 1))) -- x := x + 1
      (Assign "y" (Var "x")) -- y := x
  )
```

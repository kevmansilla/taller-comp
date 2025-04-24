# Laboratorio Lenguajes y Compiladores

Implementación de la semántica denotacional para el lenguaje imperativo simple 
con fallas (LIS + Fallas).

## Semántica del lenguaje
Se agregan excepciones al lenguaje imperativo simple.
```math
\langle comm \rangle ::= fail \mid catchin \langle comm \rangle with \langle comm \rangle
```
Sea $\Sigma^{'} = \Sigma \cup \{ abort\}$ con orden discreto. La guncion semántica ahora es:
```math
\llbracket\_\,\rrbracket \in \langle comm \rangle \to \Sigma \to \Sigma^{'}_{\bot}
```


Las ecuaciones semánticas son las siguiente:
```math
\begin{align*}
\llbracket Skip \rrbracket_{\sigma} & = \sigma \\
\llbracket Fail\rrbracket_{\sigma} &= \langle abort, \sigma \rangle\\
\llbracket v := e\rrbracket_{\sigma} &= [\sigma\mid v : \llbracket e\rrbracket_{\sigma} ]\\
\llbracket if\,\, b\,\, then\,\, c_{0}\,\, else\,\, c_{1} \rrbracket_{\sigma} &= \begin{cases}
  \llbracket c_{0}\rrbracket_{\sigma} & \text{si}\,\, \llbracket b\rrbracket_{\sigma}\\
  \llbracket c_{1}\rrbracket_{\sigma} & \text{si}\,\, \neg \llbracket b\rrbracket_{\sigma}
\end{cases}\\
\end{align*}
```

Operadores de transferencia de control. Dada $f \in \Sigma \to \Sigma^{'}_{\bot}$,
denotamos por $f_{*}$ la siguiente funcion
```math
\begin{align*}
f_{*} & \in \Sigma^{'}_{\bot} \to \Sigma^{'}_{\bot}\\
f_{*}x &= \begin{cases}
  f \sigma & \text{si}\,\, x = \sigma \in \Sigma\\
  x & \text{si no}
\end{cases}
\end{align*}
```
En este caso, la presencia de una situación obortiva determina que no se 
transfiere el control a $f$ Servira para describir el significado de $c_{0};c_{1}$
ya que si ocurre una situacion de excepción al ejecutar $c_{0}$, en control no es 
transferido a $c_{1}$.

Por otro lado, dado $f\in \Sigma \to \Sigma^{'}_{\bot}$, denotamos $f_{+}$ a la sigueinte extension de $f$ a $\Sigma^{'}_{\bot}$:
```math
\begin{align*}
f_{+} & \in \Sigma^{'}_{\bot} \to \Sigma^{'}_{\bot}\\
f_{*}x &= \begin{cases}
  f \sigma & \text{si}\,\, x = \langle abort, \sigma \rangle \in \{ abort\}\times \Sigma\\
  x & \text{si no}
\end{cases}
\end{align*}
```

En una clara dualidad con la definición de $f_{∗}$, la definición de $f_{+}$ 
determina lo contrario: se transfiere el control a $f$ sólo en caso de excepción. 
Esto corresponderá a $catchin c with c′$.

Finalmente, dada $f\in \Sigma \to \Sigma$, denotamos por $f_{\dagger}$ la siguiente
extension de $f$ a $\Sigma^{'}_{\bot}$:
```math
\begin{align*}
f_{\dagger} & \in \Sigma^{'}_{\bot} \to \Sigma^{'}_{\bot}\\
f_{\dagger}x &= \begin{cases}
  \langle abort, f\sigma \rangle & \text{si}\,\, x = \langle abort, \sigma \rangle \\
  f x & \text{si}\,\, x \in \Sigma\\
  \bot & \text{si no}
\end{cases}
\end{align*}
```

Nose que aui hay una transferencia de control a $f$ en cualquier situacion (abortiva o no). Servirá para restaurar el valor de las variables locales. Entonces las 
demas funcione semanticas
```math
\begin{align*}
\llbracket c_{0};c_{1} \rrbracket_{\sigma} &= \llbracket c_{1}\rrbracket_{*}(\llbracket c_{0}\rrbracket_{\sigma})\\
\llbracket catchin \,\,c_{0}\,\, with \,\,c_{1} \rrbracket_{\sigma} &= \llbracket c_{1}\rrbracket_{+}(\llbracket c_{0}\rrbracket_{\sigma})\\
\llbracket newvar\,\, v := e\,\, in\,\, c \rrbracket_{\sigma} &= (\lambda\sigma^{'} \in \Sigma. 
[\sigma^{'} \mid v : \sigma v])_{\dagger} (\llbracket c\rrbracket[\sigma \mid v : \llbracket e\rrbracket_{\sigma} ])\\
\llbracket while\,\,b\,\,do\,\,c\rrbracket &= \sqcup_{i=0}^{\infty} F^{i} \bot_{\Sigma \to \Sigma_{\bot}} \\
donde \\
F w \sigma &= \begin{cases}
  w_{*}(\llbracket c\rrbracket_{\sigma}) & \text{si}\,\, \llbracket b\rrbracket_{\sigma}\\
  \sigma & \text{si}\,\, \neg \llbracket b\rrbracket_{\sigma}
  \end{cases}
\end{align*}
```


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

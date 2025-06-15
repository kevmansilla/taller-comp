# Laboratorio Lenguajes y Compiladores

## Distribución

Este repositorio contiene la implementación de la semántica 
denotacional del lenjuaje implerativo LIS con fallas. En el 
archivo `Lab.hs` se encuentra la implementación de la semántica
(para más información sobre la misma ver el archivo `doc.pdf`), y 
en el archivo `main.hs` se encuentran los programas de prueba.

## Usage

Para correr los test del proyecto

```
make test
```

Para limpiar el directorio de trabajo

```
make clean
```

## Extra
Como extra, se incluye la extensión del lenguaje con input y output en el 
archivo `Lab2.hs`. Como test de esta extensión se implmento la función factorial 
para que se pueda probar la entrada y salida del programa. Para poder probarla 
se debe correr el comando:

```
make io
```

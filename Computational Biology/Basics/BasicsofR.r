---
title: "BasicosR_A00829837"
author: "Axel Amos - A00829837"
date: "2/4/2021"
output: pdf_document
---
1.   Crear un vector x con los datos 10, 11, 13, 1, 6, 3
```{r}
x <- c(10,11,13,1,6,3)
x
```
2.   Calcula estadísticas simple de x. Calcula la media, la desviación estándar y la varianza. Crear un objeto (vector) con el nombre est.x en el que guardar los 3 estadísticos.
```{r}
#Media
x <- c(10,11,13,1,6,3)
media1 <- mean(x) #mean( ) es la fucnion predeterminada del promedio
media1
#Desviación Estándar
desvE <- sd(x) #sd( ) es la funcion predeterminada de la desviacion estándar
desvE
#Varianza
vari <- var(x) #var( ) es la funcion predeterminada de la varianza, 
#si le hacemos sqrt sale la desvE
vari
#Vector con los datos
est.x <- c(media1,desvE,vari) #vector con los valores obtenidos anteriormente
est.x
```
3.   Escribe un programa R para crear una secuencia de números del 20 al 50 y encuentre la media de los números del 20 al 60 y la suma de los números del 51 al 91. Tip: utiliza las funciones de R.
```{r}
#Creando la secuencia del 20 al 50
secuencia1 <- (20:50) #Se crea una secuencia del 20 al 50 gracias a los :
secuencia1
#Encontrar la media de los numeros del 20 al 60
media2 <- mean(20:60) #se crea una variable que alamacene la media de los
#numeros solicitados y lo evaluamos con la funcion predeterminada mean( ).
media2
#Sumar los numeros del 51 al 91
suma1 <- sum(51:91) #Se crea una variable para almacenar la suma de los numeros,
#evaluandolo con la funcion predeterminada de sum( ).
suma1
```
4.   Escribe un programa R para crear un vector que contenga 10 valores enteros aleatorios entre -100 y +50. Revisa la función sample.
```{r}

#var <- sample(que valores se tomaran, cuantos se mostrarán, replace = 
#(TRUE, si no se quieren elementos repetidos; FALSE, si si se desean repetidos))
vector1000 <- sample(-100:50, 10, replace = TRUE)
vector1000
```

5.   Escribe un programa R para obtener los primeros 10 números de Fibonacci 
```{r}
fufb <- numeric(10) #numeric(n datos)
fufb[1] <- 1 #significa 1 en ese index
fufb[2] <- 1 # Significa 1 en ese index


for(i in 1:10){ #for de i que va del 1 al 10 (el valor de numeric)
    if(i == 1){ #filtro para asignar siempre a fufb[1] el valor de 0
      fufb[i] <- fufb[1] - 1 #asignando el valor de 0 a fbfu[1]
    }
    if (i == 2){ #filtro para asignar siempre a fufb[2] el valor de 1
      fufb[i] <- fufb[2] #asignando el valor de 1. Ya estaba declarado arriba, 
      #pero funcionará por si acaso
    }
    else{
      for(j in 3:10) # Se crea otro for para continuar con los valores que 
        #ahora si dependen de la fórmula. 3:10 representa el index 3 hasta 
        #el 10 (numeric).
    fufb[j] = fufb[j - 1] + fufb[j - 2] #utlizamos la formula mientras aumenta 
      #j para formar la secuencia
      j = j+1 #aumentamos el valor
      i = i+1 #aumentamos el valor
    }
}
fufb #desplegamos la secuencia
```
6.   Escribe un programa R para encontrar el valor máximo y mínimo de un vector dado. Debes probar con:
a.   c(10, 20, 30, 4, 50, -60)
b.   c(10, 20, 30, 4, 50, -60)
```{r}
#vector
vectorA <- c(10,20,30,4,50,-60)
#uso de funciones predeterminadas
max(vectorA) #usamos la funcion definida max( ) para obtener el maximo del 
#vector
min(vectorA) #usamos la funcion definida min( ) para obtener el minimo del 
#vector

```
7. Escribe una función R para multiplicar dos vectores de tipo entero y longitud n, de la misma longitud ambos.
a.   multiplica(c(10, 20), c(3,4)) 
Debe dar como salida: [1] 30 80
```{r}
#función
multivect <- function(x,y){
  el_vector <- c() #crea un vector vacío
  for(i in 1:length(x)){ #utilizamos lenght(x) para dar un valor al rango 
    #final del for
    el_vector <- c(el_vector, x[i]*y[i]) #se adicionan los valores que vna 
    #saliendo por la multiplicacion de vectores
  }
  return(el_vector) #retorna el valor de la multiplicacion por medio de 
  #el_vector
}
#probando la función
a <- multivect(c(10,20), c(3,4)) #asignamos una variable para guardar 
#el resultado
a
```
 8.   Escribe una función R para contar el valor específico en un vector dado.
 a.   cuenta(c(10, 20, 10, 7, 24, 7, 5),7) 
 Debe dar como salida: [1] 2
```{r}
#Funcion
cuenta <- function(a,b){
  contador = 0 #contador para ver cuandos numeros iguales hay
  for(i in 1:length(a)){ #for que va desde 1 hasta la longitud del vector
    if(a[i] == b){ #if para comprobar si el valor de a[i] y b son iguales
      contador = contador + 1 #en caso de que si se aumenta 1 al contador
      i = i+1 #se aumenta i
    }
    else{
      i = i+1 #si no solo se aumenta i
    }
  }
  return(contador) #se regresa el contador
}
#probando la función
b <- cuenta(c(1, 20, 10, 7, 24, 7, 5), 7)
b
```
9.   Escribe una función en R para extraer cada enésimo elemento de un vector dado. Un prueba es:
a.   v <- 1:100   
b.   enesimo(v, 5)    
c.   Salida: [1]  1  6 11 16 21 26 31 36 41 46 51 56 61 66 71 76 81 86 91 96 
```{r}
#funcion
enesimo <- function(v,m){
  vektor <- c() #creamos un vector para guardar los valores
  i = 1 #asignamos el indice 1 de v
  while(i <= length(v)){ #hacemos un while que dure mientras i no arrebase 
    #la longitud de v
    vektor <- c(vektor, v[i]) #agregamos el primer valor de i = 1
    i = i + m #aumentamos a i el valor de m para que en la siguiente iteracion 
    #se agregue al vector
    }
  return(vektor) #retornamos el vector
}
#probando la funcion
v <- 1:100 #creamos v
h <- enesimo(v,5) #probamos la función
h #desplegamos el resultado
```


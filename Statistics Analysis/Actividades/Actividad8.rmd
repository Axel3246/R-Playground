---
title: "Actividad 8 - Proyección de Tendencias"
author: "Axel Amós"
date: "8/6/2021"
output: pdf_document
---

```{r, include= FALSE}
getwd()
setwd("C:/Users/hplax/Desktop/ITESM/SegundoSemestre/Estadistica/Actividades")
```

## El problema de las ventas de televisores

Se trata de hacer predicción del siguiente año (año 5) sobre las ventas trimestrales
de ese año. (Trimestre 1, 2, 3, 4)

## Conociendo los datos
```{r}
t <- seq(1:16)
ventas <- c(4.8, 4.1, 6.0, 6.5, 5.8, 5.2, 6.8, 7.4, 6.0, 5.6, 7.5, 7.8, 6.3, 5.9, 8.0, 8.4) #En miles
```

## Gráfico de dispersión

```{r}
plot(t, ventas, type='o', main = 'Ventas por año', col='red')
```

Se observa en el gráfico una tendencia y ciclos (o estacionalidad).


## Análisis de Tendencia 
```{r}
x = ts(ventas, frequency = 4, start(c(2016,1))) ## Ciclos trimestrales (4), bimestrales(6), semestral(2)
T = decompose(x, type = "m") # multiplicativo (m) o aditivo(a)
T
plot(T, col = "blue")
##start(c(año, trimestre en que empieza))
```

## Hallando el modelo de regresion lineal de la tendencia
# Desestacionalizando las ventas
```{r}
#$seasonal = Son los índices estacionales

#$trend = La tendencia (se refiere al promedio móvil centrado)

x1 = 1:16
y1 = T$x / T$seasonal

plot(x1,y1, type='o', main = 'Gráfico de tendencia de la serie')

regresion = lm(y1 ~ x1)
regresion

abline(regresion, col='red')
text(7,5, "Ventas =  5.108 + 0.1474 x (trimestre)")
```

## Argumentando a favor del modelo lineal

1. Dependencia entre las variables: Si no hubiera dependencia, no sirve el modelo)

$H_0 = \beta_1 = 0$ (significa que X y Y son independientes, el modelo no sirve)

$H_1 = \beta_1 \neq 0$ (sí hay dependencia significativa)

$\alpha = 0.05$

```{r}
S1 = summary(regresion)
S1
```

Como el valor p =  4.25e-09 es menor que $\alpha$, la beta difiere de 0,
por tanto hay dependencia entre las variables, aceptando $H_1$. (Dependencia ente trimestre y ventas)

Por otra parte, el coeficiente de determinación $R^2$ = 0.9208, esto quiere decir que el 92.08% de toda la variación queda explicada por el modelo.

### Gráfica de Residuos

```{r}
plot(regresion$fitted.values, regresion$residuals)
abline(h = 0, col = "blue")
```

No es contundente la variación de residuos con la misma varianza, y tal vez muestra una tendencia cuadrática.

Pero como hay dependencia significativa entre las variables y $R^2$ tiene un valor alto, nos quedamos con el modelo.

Mas argumentos a favor del modelo:

## Prueba de normalidad

$H_0$: Los datos provienen de una distribución (población) normal

$H_1$: Los datos no provienen de una distribución (población) normal

$\alpha$ = 0.05

```{r}
shapiro.test(regresion$residuals)
```

Como el valor $p$ = 0.07307 > $\alpha$ = 0.05, no se rechaza $H_0$.
Por tanto, suponemos y concluimos que los datos provienen de una población normal.

### Prueba de que la media de residuos es cero

$H_0$: La media de residuos es igual a 0

$H_1$: La media de residuos es diferente a 0

$\alpha$ = 0.05

```{r}
t.test(regresion$residuals)
```

Como el valor p de la prueba resultó 1 > $\alpha$ = 0.05, entonces aceptamos $H_0$. Es decir, suponemos que la media de residuos es 0.

Interpretando el intervalo de confianza dado al 95% tenemos que la verdadera media de los residuos está entre -0.1096 y 0.1096.

## El pronóstico para el siguiente año

```{r}
f = function(x) 5.10804 + 0.14738*x

f(17)*T$seasonal[1]*1000 #se multiplica por mil para obtener el valor completo

f(18)*T$seasonal[2]*1000

f(19)*T$seasonal[3]*1000

f(20)*T$seasonal[4]*1000
```

## Gráfica con el pronóstico del quinto año

```{r}
tp = 1:20
ventasp =c(4.8, 4.1, 6.0, 6.5, 5.8, 5.2, 6.8, 7.4, 6.0, 5.6, 7.5, 7.8, 6.3, 5.9, 8.0, 8.4, 7.08, 6.49, 8.63, 9.19)
plot(tp, ventasp, type = "o", col= "red", main = "Ventas con pronóstico año 5")
abline(v = 16,lty = 2, col = "blue")
text(18, 5, " Pronóstico 5 año")
```

## Gráfica de valores reales vs esperados
```{r}
e = NA
g = NA
for(i in 1:16){
g[i] = f(i)*T$seasonal[i]
e[i] = ventas[i]- g[i]
}
CME = mean(e^2, na.rm = TRUE)
CME

plot(t, ventas, col = "red", type = "o", lty = 4)
abline(regresion, col = "brown")
lines(t, g, lty = 3, col = "blue")

```





























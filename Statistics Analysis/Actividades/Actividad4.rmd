---
title: "Actividad_04"
author: "Axel Amós"
date: "25/5/2021"
output: pdf_document
---

## Actividad 4 - Análisis de Residuos

Como primer paso será leer los datos:
```{r, include= FALSE}
getwd()
setwd("C:/Users/hplax/Desktop/ITESM/SegundoSemestre/Estadistica/Actividades")
```

```{r}
M = read.csv("estaturas.csv")
head(M, n = 3)
```

## Matriz de correlaciones
```{r}
cor(M)
```

Como se puede observar, hay una alta correlación entre las estaturas de papás e 
hijos (r = 0.9052)


## Prueba de hipótesis de correlación
$H_0: R = 0$
$H1: R \neq 0$
$\alpha = 0.05$

```{r}
cor.test(M$padre, M$hijo)
```

Como el valor $p = 0.00031 < \alpha = 0.05$, entonces se rechaza $H_0$. Esto quiere
decir que 0.90 es signifcativamente diferente de 0.

## Gráfico de dispersión

```{r}
plot(M, main = "Estaturas", col = "purple")
```

## Regresión Lineal
```{r}
regresion = lm( M$hijo ~ M$padre)
```

Por tanto, la ecuación es:

Estatura Hijo = 0.6958 + 0.6094 Estatura Padre
y = 0.6958 + 0.6094x

## Gráfica
```{r}
plot (M, main = "Estaturas", col = "red")
x = seq(1.68, 1.80, 0.001)
y = 0.6958 + 0.6094*x
lines(x,y, col="blue")
text(1.74, 1.73, "Ehijo = 0.6958 + 0.6094 Epapa")
```

## Analisis de dependencia de X y Y

$H_0 = \beta_1 = 0$ (significa que X y Y son independientes, el modelo no sirve)
$H_1 = \beta_1 \neq 0$ (sí hay dependencia)
$\alpha = 0.05$

```{r}
S = summary(regresion)
S
```
Como el valor p de la fila M$padre$ es 0.000314 < $\alpha$ = 0.05
Entonces se rechaza $H_0$. Es decir, beta1 se separa significativamente del 0. Hay dependencia entre X,Y.

## Intervalos de confianza para beta_0 y beta1

```{r}
confint(regresion)
```
Esto quiere decir que el verdadero valor de $\beta_0$ (de población) debe estar entre 0.292 y 1.099 al 95% de confianza.

El verdadero valor de $\beta_1$ debe estar entre 0.376 y 0.842 al 95% de confianza. 

## Coeficiente de Determinacion
```{r}
#Beta 1 y Beta 0 es la ecuación de regresión lineal
S
```

El coeficiente de determinación de R2 = 0.819 lo que quiere decir que cerca del 82% de variabilidad total queda explicada por el modelo.

## Analisis de Residuos

## Homocedasticidad

Se refiere a si la varianza es aproximadamente igual a lo largo de los valores del eje horizontal.

```{r}
plot(regresion$fitted.values, regresion$residuals)
abline(h = 0, col = "red")
```

Como puede observarse de la gráfica, la variación permanece aproximadamente constante a lo largo del eje horizontal.

## Prueba de normalidad de los residuos

Prueba de Normalidad de Shapiro.Wilk

$H_0$: Los datos provienen de una distribución normal
$H_1$: Los datos no provienen de una distribución normal
$\alpha$ = 0.05

Regla de decisión: Si el valor p es menor que $\alpha$, se rechaza $H_0$

```{r}
shapiro.test(regresion$residuals)
```

Como el valor $p$ = 0.3542 > $\alpha$ = 0.05, no se rechaza $H_0$.
Por tanto, suponemos que los datos provienen de una población normal.

## Prueba Media de Residuos igual a 0

$H_0$: La media de residuos es igual a 0
$H_1$: La media de residuos es diferente a 0
$\alpha$ = 0.05

$\mu_{residuos} = 0$
$\mu_{residuos} \neq 0$
$\alpha = 0.05$

```{r}
t.test(regresion$residuals)
```

Como el valor p de la prueba resultó 1 > $\alpha$ = 0.05, entonces aceptamos $H_0$. Es decir, suponemos que la media de residuos es 0.

Interpretando el intervalo de confianza dado al 95% tenemos que la verdadera media de los residuos está entre -0.0077 y 0.0077. Y como
este intervalo contiene al 0. Entonces, como contiene al 0, no se puede rechazar $H_0$. Si fuera de .1 a .5 no se podría aceptar $H_0$, porque
no tiene el valor de 0.

Por tanto, concluimos que la media de residuos es 0.

```{r}
par(mfrow = c(2,2))
qqnorm(regresion$residuals) #x y te doy un porcentaje de datos debajo de ese valor de x
qqline(regresion$residuals, col="red") #pone una linea en la gráfica de qqnorm
hist(regresion$residuals)# hace un histograma
boxplot(regresion$residuals)# diagrama de caja y bigotes
```

### Prueba de Anderson-Darling

$H_0$: Los datos provienen de una distribución normal
$H_1$: Los datos no provienen de una distribución normal
$\alpha$ = 0.05

Regla de decisión: Si el valor p es menor que $\alpha$, se rechaza $H_0$

```{r}
library(nortest)
ad.test(regresion$residuals)
```

Como el valor p = 0.2628 > alfa = 0.05, no se rechaza $H_0$. Si el valor p hubiera dado menor que $\alpha$, se hubiera rechazado $H_0$.





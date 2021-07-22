---
title: "Actividad03"
author: "Axel Amos"
date: "21/5/2021"
output:
  pdf_document: default
  html_document: default
---

## Actividad 02 - Regresión Lineal

Como primer paso será leer los datos:
```{r, include= FALSE}
getwd()
setwd("C:/Users/hplax/Desktop/ITESM/SegundoSemestre/Estadistica")
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




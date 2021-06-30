---
title: "Actividad1"
author: "Axel Amos"
date: "14/5/2021"
output:
  pdf_document: default
  html_document: default
---
## Actividad 01. Correlación Lineal

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





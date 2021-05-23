---
title: "Untitled"
author: "Axel Amos"
date: "13/4/2021"
output: pdf_document
---

```{r}
library(Biostrings);
getwd()
setwd("C:/Users/hplax/Desktop/ITESM/SegundoSemestre/BT1003")
```

```{r}
virus_dengue <- readDNAStringSet(filepath="C:/Users/hplax/Desktop/ITESM/SegundoSemestre/BT1003/NC_001477.1_Dengue.fasta")
virus_dengue
```
- DESPLIEGUE Y LARGO DE LA SECUENCIA DE LINEAS DE UN .FASTA
```{r}
#Leer cuantas lineas tiene de secuencia
virusD <- readLines(con="NC_001477.1_Dengue.fasta")
length(virusD)
#virusD
```
DESPLIEGUE DE INDEXES EN EL FASTA
```{r}
virusD[1]
virusD[2]
virusD[156]
```
nchar: CUANTOS CARACTERES TIENE LA SECUENCIA EN LA LÍNEA N 
```{r}
nchar(virusD[2])
nchar(virusD[3])
nchar(virusD[155])
```
ESTO NOS PERMITE CONTAR CUANTAS LINEAS DE CODIGO GENÉTICO SON EN REALIDAD - CONTANDO EL STOP O EL 156 EN ESTE CASO
```{r}
length(virusD[2:155])
```
El paquete Biostringstiene unafunción para leer archivos FASTA, que resulta en una estructura de datos parecida a la lista
```{r}
virus_DB <-readDNAStringSet(filepath = "NC_001477.1_Dengue.fasta")
virus_DB
```
TAMBIEN PODEMOS SACAR PORCENTAJES A PARTIR DEL STRING
```{r}
basesADN <- c("A","T","C","G")
letterFrequency(x = virus_DB, letters = basesADN, as.prob = T)
```
Biostrings tiene una funciónpara obtener subsecuencias.
```{r}
subseq(x = virus_DB, start = 1, end = 100)
```
```{r}
subseq(x = virus_DB, start = 10725, end = 10735)
```













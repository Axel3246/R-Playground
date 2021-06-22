---
title: "Evidencia 1 | Análisis Inicial"
author: "Axel Amós Hernández Cárdenas"
date: "21/4/2021"
output:
  html_document: default
  pdf_document: default
---

```{r, include= FALSE}
library(Biostrings);
getwd()
setwd("C:/Users/hplax/Desktop/ITESM/SegundoSemestre/BT1003")
```

__PASO 1__

__1. Investiga, ¿Cuál es la situación actual de COVID-19 a nivel mundial, en México, en tu estado de origen y en tu municipio, alcaldía o colonia? __

Primeramente, en México ha habido un total de 2,031,000 casos del SARS-CoV-2, de los cuales 1,077,000 millones se han recuperado y, desgraciadamente, 213,00 personas han fallecido a causa del virus. (CSSEGISandData, 2021)

Según el portal de Salud Coahuila, el estado de Coahuila de Zaragoza ha tenido un total de 69,650 casos positivos, 6,061 defunciones, 63,107 recuperados y un total de 409,665 estudios realizados. Para mi municipio, Monclova, al día 21 de Abril del 2021, se cuenta con 31 casos activos. (SSA, 2021)

__2. ¿Cuál fue la primera variante del virus que se propagó a todo el mundo?__

La primer variante, segun la OMS, fue la encontrada en Wuhan, China en 2019. Sin embargo, esta variante en Febrero del 2020 sufrió una sustitución en el D614G. Esta sustitución hizo que la variación sustituyera, al cabo de pocos meses, a la primera variante del SARS-CoV-2: la de Wuhan, China. (OMS,2020)

__3. ¿Cuáles son las otras variantes del virus que existen en otras regiones del mundo?__

Existen una variedad de variantes en el mundo. Por ejemplo, las siguientes cinco han sido identificadas en Estados Unidos: (CDC, 2021)

B.1.1.7: esta variante se identificó por primera vez en los EE. UU. en diciembre del 2020. Se detectó inicialmente en el RU.

B.1.35: esta variante se identificó por primera vez en los EE. UU. a finales de enero del 2021. Se detectó inicialmente en Sudáfrica en diciembre del 2020.

P.1: esta variante se detectó por primera vez en los EE. UU. en enero del 2021. La variante P.1 se identificó inicialmente en viajeros provenientes de Brasil, a quienes se les realizó una prueba de detección durante los controles de rutina en un aeropuerto de Japón, a principios de enero.

Europa no es la excepción, en Diciembre del 2020 las autoridades del Reino Unido una variante, que designaron como SARS-CoV-2 VOC 202012/01, con 23 sustituciones de nucleótidos y no relacionada filogenéticamente con el SARS-CoV-2. (OMS, 2020) En África, también, se han encontrado variantes, como la 501Y.V2 en diciembre 2020, que presentaba una mutación N501Y. (OMS, 2020)

Muchas otras variaciones han salido del virus original y de las mismas variaciones y se pueden encontrar en las páginas de interés de la epidemia del SARS-CoV-2, como la de la Organización Mundial de la Salud o la NCBI.


__4. ¿Cómo buscarías información de la variante del virus en tu país?__

Para buscar información de las variantes y del virus en mi país, México, utilizaría los sitios web de la Organización Mundial de la Salud, el National Center for Biotechnology Information (NCBI) o la Secretaría de Salud de mi país o de mi estado.

__5. Imagina que te encuentras en una situación similar a la de Li Wenliang, médico chino que intentó alertar sobre el brote de coronavirus en su país, pero fue detenido por las autoridades y obligado a retractarse, ¿qué harías en su caso? Selecciona un inciso:a) Lo reportas al centro de investigación o la universidad. b) Lo reportas a la prensa. c) Guardas la información. Elige y justifica tu respuesta.__

Yo elegiría la primera opcion (a), lo reportaría al centro de investigación. Conociendo que puede ser un virus peligroso, lo más ético sería empezar la investigación y comenzar a obtener información sobte como combatir al virus. Obviamente, compartir información con otras agencias de investigación es importante para un avance mejor contra el virus. Aunque en el caso de Li, el gobierno se interpuso, el hizo lo mejor que pudo hacer: reportarlo e intentar salvar la mayor cantidad de vidas posibles.

__PASO 2__

1 - Obten variantes del SARS-CoV-2. (Wuhan - B117 - P1 - B1525 - B1526)
```{r}
var_wuhan <- readDNAStringSet(filepath="C:/Users/hplax/Desktop/ITESM/SegundoSemestre/BT1003/NC_045512.2SARSCoV2.fasta")
var_wuhan
```
```{r}
var_B117 <- readDNAStringSet(filepath="C:/Users/hplax/Desktop/ITESM/SegundoSemestre/BT1003/B117COVID2.fasta")
var_B117
```
```{r}
var_P1 <- readDNAStringSet(filepath="C:/Users/hplax/Desktop/ITESM/SegundoSemestre/BT1003/P1COVID2.fasta")
var_P1
```
```{r}
var_B1525 <- readDNAStringSet(filepath="C:/Users/hplax/Desktop/ITESM/SegundoSemestre/BT1003/B1525COVID.fasta")
var_B1525
```
```{r}
var_B1526 <- readDNAStringSet(filepath="C:/Users/hplax/Desktop/ITESM/SegundoSemestre/BT1003/B1526NY.fasta")
var_B1526
```
2. Calcula la longitud de las secuencias de cada variante.
```{r}
#Calculando el tamaño de la secuencia
var_WuhanT <- length(var_wuhan[[1]])
var_B117T <- length(var_B117[[1]])
var_P1T <- length(var_P1[[1]])
var_B1525T <- length(var_B1525[[1]])
var_B1526T <- length(var_B1526[[1]])

#Desplegando el tamaño de la Secuencia
var_WuhanT
var_B117T
var_P1T
var_B1525T
var_B1526T
```
3. Crea una gráfica donde compares las bases de ADN que componen a cada una de las variantes del virus.
```{r}
#Calculando la frecuencia de los nucleotidos
var_WuhanF <- letterFrequency(x = var_wuhan, letters = DNA_BASES, as.prob = T)
var_B117F <- letterFrequency(x = var_B117, letters = DNA_BASES, as.prob = T)
var_P1F <- letterFrequency(x = var_P1, letters = DNA_BASES, as.prob = T)
var_B1525F <- letterFrequency(x = var_B1525, letters = DNA_BASES, as.prob = T)
var_B1526F <- letterFrequency(x = var_B1526, letters = DNA_BASES, as.prob = T)

#Creando el plot


compViruses <-rbind(var_WuhanF, var_B117F, var_P1F, var_B1525F, var_B1526F)
rownames(compViruses) <-c("Wuhan","B117","P1", "B1525", "B1526")
compViruses

```
```{r}
barplot(compViruses, col=1:5, beside=T)
legend("bottom", legend =c("Wuhan", "B117", "P1", "B1525", "B1526"), fill = 1:5, horiz = T, cex=0.6)
```
Como se puede observar en la tabla y en la gráfica, las cantidades de bases de cada variante
del SARS-CoV-2 varían por decimas o centésimas. En la gráfica se muestra una leve desviación
por cada base, lo que hace que el observador malinterprete que no hubo un cambio significante.

Sin embargo, esta ligera variación en las bases de cada variante es lo que las hace muy diferentes
una a otra. No por ser un cambio de centésimas, en este caso, no significa que no suponga un cambio
significativo en las capacidades de la variación del virus.

4. ¿Cuál es el %GC de cada variante?
```{r}
#Obteniendo la frecuencia
var_WuhanGC <-letterFrequency(x = var_wuhan, letters = "GC", as.prob = T)
var_B117GC <-letterFrequency(x = var_B117, letters = "GC", as.prob = T)
var_P1GC <-letterFrequency(x = var_P1, letters = "GC", as.prob = T)
var_B1525GC <-letterFrequency(x = var_B1525, letters = "GC", as.prob = T)
var_B1526GC <-letterFrequency(x = var_B1526, letters = "GC", as.prob = T)

#Desplegando la frecuencia
var_WuhanGC
var_B117GC
var_P1GC
var_B1525GC
var_B1526GC

compVirusesGC <-rbind(var_WuhanGC, var_B117GC, var_P1GC, var_B1525GC, var_B1526GC)
rownames(compVirusesGC) <-c("Wuhan","B117","P1", "B1525", "B1526")
```
```{r}
barplot(compVirusesGC, col=1:5, beside=T)
legend("bottom", legend =c("Wuhan", "B117", "P1", "B1525", "B1526"), fill = 1:5, horiz = T, cex=0.6)
```
Debido a la poca variación en GC, la gráfica parece estar igual, sin embargo, si
vemos la tabla de G|C podemos ver que sí varía por milésimas los porcentajes de G|C.

5. Crea las secuencias contrasentido de cada variante.
```{r}
#Obteniendo los complementos
var_WuhanC <- complement(x = var_wuhan)
virus_B117C <- complement(x = var_B117)
virus_P1C <- complement(x = var_P1)
virus_B1525C <- complement(x = var_B1525)
virus_B1526C <- complement(x = var_B1526)

#Desplegando los complementos
var_WuhanC
virus_B117C
virus_P1C
virus_B1525C
virus_B1526C
```

__Referencias Bibliográficas__

Centros para el Control y la Prevención de Enfermedades. (02/04/2021). Acerca de las variantes que causa el COVID-19. CDC ES: https://espanol.cdc.gov/coronavirus/2019-ncov/transmission/variant.html

CNN. (04/02/2020). Coronavirus whistleblower doctor is online hero in China. Youtube:https://youtu.be/eEUqCxP5Lvc

CSSEGISandData. (21/04/2021). COVID-19. GitHub: https://github.com/CSSEGISandData/COVID-19

National Center for Biotechnology Information. (2021). Página Inicial. NCBI: https://www.ncbi.nlm.nih.gov/

Organización Mundial de la Salud. (30/128/2020). Variantes del SARS-CoV-2 - Brote Epidémico. OMS: https://www.who.int/csr/don/31-december-2020-sars-cov2-variants/es/

Secretaría de Salud. (21/04/2021). COVID 19. Estado de Coahuila - Secretaría de Salud: https://www.saludcoahuila.gob.mx/COVID19/














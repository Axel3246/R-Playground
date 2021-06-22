---
title: "Evidencia 02 | Proyecto Integrador"
author: "Axel Amos"
date: "27/4/2021"
output:
  html_document:
    df_print: paged
---
_PASO 1_

_1.¿Cuáles son los virus “similares” a SARS-CoV-2 en el mundo? Obtén tus referencias de PUBMED._

La actual pandemia en la que todo el mundo está cruzando, fue causada por el SARS-CoV-2. Este virus originario de Wuhan, China fue una 
sorpresa para la comunidad científica y para el mundo. Mencionar que el SARS-CoV-2 es un virus nunca antes visto es válido, sin embargo
otros tipos de virus son muy similares al SARS-CoV-2. A continuación, se enuncian algunos de ellos.

Indagando en el portal de PUBMED, se encontraron dos articulos relevantes que comparan la similitud del SARS-CoV-2 con otros virus.

El primer artículo, "SARS-CoV-2, SARS-CoV, and MERS-COV: A comparative overview" consta de una comparación exahustiva del SARS-CoV-2 con otros virus relacionados. En el abstract se establece que el SARS-CoV-2 tiene una relación y un parecido muy estrecho al SARS y MERS estructuralmente, patogenicamente y en su estructura de proteínas. (Rabaan, 2020) Es de esperarse, pues el SARS-CoV-2 pertenece a la misma familia de coronavirus que el SARS y MERS.

Por otra parte, el "El ozono no consigue la desinfección de los vehículos de emergencias de virus similares al SARS-CoV-2", menciona que el lentivirus (lentivector) es muy similar al SARS-CoV-2. (Birrun, 2020) Este tipo de virus comparten un tamaño similar al SARS-CoV-2, ademas que cuentan con una estructura similar del RNA al SARS-CoV-2.

Entonces, podemos observar que existen muchos virus similares al SARS-CoV-2, ya que comparten muchas cualidades y estructuras similares.

_2.¿Cuáles son los coronavirus reportados en otras especies que pueden ser cercanos_
_al genoma de SARS-CoV-2? Incluye de qué especies son y menciona las referencias de los artículos consultados en PUBMED._

En PUBMED podemos encontrar el artículo "A mouse-adapted SARS-coronavirus causes disease and mortality in BALB/c mice", los ratones tambien sufren del virus del SARS. Como habiamos mencionado anteriormente, el SARS es muy parecido al SARS-CoV-2 y, se reitera, que es por ser de la misma familia de los coronavirus. Los ratones por el SARS desarrollan la enfermedad y luego la infección según el científico encabezado del artículo Anjeanette Roberts. (Roberts,2020)

Otro ejemplo el MERS. En el artículo, "Coronavirus Infections in Companion Animals: Virology, Epidemiology, Clinical and Pathologic Features", el MERS también es un virus que se presenta en los animales y puede ser muy letal. Christine Hakke, la divulgadora del documento, menciona que esta familia de coronavirus tiene mucho potencial de ser zootónica. (Haake, 2020)

Por lo tanto, podemos observar que los virus similares al SARS-CoV-2, que pertenecen a la familia del coronavirus, se encuentran en muchas especies (son muy zootónicas), entre las especies de felinos, caninos, equinos, ferrets, y murciélagos. En sí, son una gran cantidad de especies que comparten virus similares al SARS-CoV-2.

_3.En relación con la situación actual reflexiona, ¿qué propondrías que se deba hacer durante la contingencia del SARS-CoV-2 en_
_comunidades de bajos recursos? Si tu vivieras en una situación de escasos recursos, ¿qué harías? Justifica tu respuesta._

LLevar la vacuna y articulos sanitarios a estas comunidades proporcionados por el gobierno. Si yo viviera en esta situación, buscaría apoyos gubernamentales para que mi comunidad pueda beneficiarse y poder pasar un tiempo en pandemia más seguro. ¿Porqué haría esto? Por que buscaría el beneficio para mi comunidad. El bienestar y la salud para para una comunidad de escasos recursos no debe ser opacada. Al final estas comunidades también forman parte de la población y se deben ayudar como se debe.

_PASO 2_

```{r, results=F,message=FALSE}
library(ape)
library(phytools)
library(Biostrings)
library(seqinr)
library(adegenet)
library(ggtree)
library(DECIPHER)
library(viridis)
library(ggplot2)
library(ggmsa)
```

_CASO 1:Analizar las secuencias de SARS-CoV-2 reportadas en los 20 países con más casos reportados. Y puedes tratar de responder a la pregunta: ¿Son muy diferentes las variantes entre cada país? ¿Es diferente el SARS-CoV-2 entre las diferentes poblaciones: Asiática, Hispana, Europea o Africana?_
```{r}

#MZ021680 - USA
#MW927136 - India
#MW592707 - Brazil
#HG993784 - France
#MW741552 - Russia
#MW308549 - Turkey
#OD906774 - United Kingdom
#MW854297 - Italy
#MW976780 - Spain
#MW822592 - Germany
#MW633891 - Argentina
#MT470219 - Colombia
#HG994158 - Polonia
#MW898809 - Iran
#MW884219 - México
#MW938089 - Peru
#MZ026853 - Indonesia
#MT534285 - Czech Republic (Republica Checa)
#MW981442 - South Africa
#MW309425 - Canada
#MW672138 - Chile

#Guardando las variantes de cada país en orden como se ve arriba
SARSVirus <- c("MZ021680", "MW927136", "MW592707", "HG993784", "MW741552", "MW308549","OD906774","MW854297","MW976780","MW822592","MW633891","MT470219","HG994158","MW898809","MW884219","MW938089","MZ026853","MT534285","MW981442","MW309425")

#Leyendo las secuencias y desplegándolas
SARS_sequences <- read.GenBank(SARSVirus)
str(SARS_sequences)
```

```{r}
#Haciendo el FASTA
write.dna(SARS_sequences,  file ="SARS_secs.fasta", format = "fasta", append =F, nbcol =5, colsep = " ", colw = 20)
#Guardandolo
SARSFastaSecs <- readDNAStringSet("SARS_secs.fasta", format = "fasta")
#Desplegando las cadenas y la longitud
SARSFastaSecs
```
_2. Calcula la longitud de las secuencias que incluyas_
```{r}
for (i in 1:20){
  print(length(SARSFastaSecs[[i]]))
}
```
_3.Crea una sola gráfica donde se comparen el número de bases de ADN que componen todas las variantes del virus._
```{r}
#Obtenemos la frecuendia de ACTG
SARS_FreqLetter <- letterFrequency(x= SARSFastaSecs, letters = DNA_BASES, as.prob = T)
#Creamos la matriz y le damos nombre a cada ROW
compSARS <-rbind(SARS_FreqLetter)
rownames(compSARS) <-c("MZ021680", "MW927136", "MW592707", "HG993784", "MW741552", "MW308549","OD906774","MW854297","MW976780","MW822592","MW633891","MT470219","HG994158","MW898809","MW884219","MW938089","MZ026853","MT534285","MW981442","MW309425")
compSARS
```
```{r}
barplot(compSARS, col=0:256, beside=T)

legend("bottomright", legend =c("MZ021680", "MW927136", "MW592707", "HG993784", "MW741552", "MW308549","OD906774","MW854297","MW976780","MW822592","MW633891","MT470219","HG994158","MW898809","MW884219","MW938089","MZ026853","MT534285","MW981442","MW309425"), fill = 0:256, horiz = F, cex=0.55)
```

A continuación, se interpretará la gráfica.

Para la Adenina:

Como podemos observar, para la mayoría de las variantes del SARS-CoV-2, contienen un porcentaje similar en la base de la Adenina. Sin embargo, dos varientes se pueden observar tener un porcentaje menor, pero similar entre ellas, de esta base. Las variantes 4 (la de Francia) y la 10 (la de Alemania), son las variantes con el procentaje de Adenina más distante de los porcentajes de los demás paises.

Pala la Citosina:

En esta base, parece haber una similitud aun mayor para todas las secuencias como se puede observar en la gráfica. Casi todas las barras de las variantes están iguales a excepcion de las variantes de Francia y Alemania.

Para la Guanina:

Podemos observar, nuevamente, una gran similitud en la mayoría de las variaciones del SARS-CoV-2. Esta vez, la variante de Alemania esta un poco más a la par con las demás, mientras que la de Francia (4), representa la variante con el menor porcentaje de todas.

Para la Timina:

Igualmente, todas las variantes tienen una similitud muy grande en cuanto a su porcentaje de Timina. E, igualmente, las variantes de Francia y Alemania son las menores de entre todas las variantes.


Se concluye, pues, que las variantes del SARS-CoV-2 de las listas de los países son muy similares. Sin embargo, dos variantes son muy distintas. Como ya se discutio, la variante francesa y la variante alemana diferían mucho de las 18 variantes restantes del virus. No obstante, estas dos variantes contienen procentajes similares de las bases de ADN, lo que las hace similares entre ellas.


_4.Agrega un análisis jerárquico global obtenido de las secuencias que se seleccionaron para estudiar._
```{r}
SARSFastaSecs<- OrientNucleotides(SARSFastaSecs)
```

```{r}
SARSAlligned <- AlignSeqs(SARSFastaSecs)
```

```{r}
BrowseSeqs(SARSAlligned, highlight=0)

writeXStringSet(SARSAlligned, file="SARS_secs.fasta")

SARS_alligned <- read.alignment("SARS_secs.fasta", format = "fasta")

matriz_distancia <- dist.alignment(SARS_alligned, matrix = "similarity")

temp <- as.data.frame(as.matrix(matriz_distancia))
table.paint(temp, cleg=0, clabel.row=.5, clabel.col=.5) + scale_color_viridis()
```

Esta matriz de distancia representa la similitud de las variantes del SARS-CoV-2 con las demás variantes. Para una interpretación general, podemos decir que entre más oscuro se denote un recuadro, más distinta será esa variante que con la que se está comparando. Análogamente, mientras más claro se encuentre un recuadro, más similares serán esas variantes entre sí.

Observando la matriz o gráfica de distancia creada, podemos observar que la mayoría de las variantes son muy similares entre sí. Pocos recuadros negros se representaron en la matriz aunque, en su mayoría, los recuadros entre claros y negros no son tampoco muy escasos. Podríamos decir que, por lo tanto, la mayoría de las variantes son muy similares entre sí, a excepcion de algunas las cuales podemos identificar por la casilla de color negro.

_Creando el Árbol Filogenético_
```{r}
SARS_tree <- nj(matriz_distancia)
plot(SARS_tree, cex = 0.7)
title("¿Qué tanto se parecen 20 variantes del SARS-CoV-2?")
```

A partir de la matriz de distancia, se llevó a cabo la construcción de este arbol filogenético. Como se puede observar, la mayoría de las secuencias siguen a un ancestro en común y son, por lo tanto, similares entre sí. Sin embargo, tres variantes destacan por seguir una sola línea en este arbol filogenético.:

La HG994158, de Polonia.
La MW927136, de India.
La M2741552, de Russia.

Estan son las variantes que, aunque parten desde un mismo ancestro en común, son las que no comparten una igualdad con otras variantes, por lo tanto estan muy separadas y aisladas de ellas.

Concluyendo, se puede decir que las variantes del SARS-CoV-2, en su mayoría, representan derivar de muchos ancestros en común y, por ende, son muy similares entre sí, a excepcion de las variantes de Polonia, India y Russia, las cuales son las más aisladas y diferentes de cada una.


```{r}
ggmsa(SARSFastaSecs, 320, 360, color = "Chemistry_AA")
```
La imagen de arriba muestra el alineamiento de las secuencias de las variantes.

```{r}
plot_sars <- ggtree(SARS_tree) + geom_tiplab()

data = tidy_msa(SARSFastaSecs, 164, 213)
```

```{r}
plot_sars + geom_facet(geom = geom_msa, data = data,  panel = 'msa', color = "Chemistry_AA") +
    xlim_tree(1)
```

Por ultimo, aqui se puede encontrar el arbol filogenético con creado a partir de la matriz distancia junto con la alineacion visual de las bases de las variantes del SARS-CoV-2.

_Conclusión_

Contestando a las preguntas del caso: ¿Son muy diferentes las variantes entre cada país? ¿Es diferente el SARS-CoV-2 entre las diferentes poblaciones: Asiática, Hispana, Europea o Africana?

Se puede concluir que, para la primera pregunta, en su mayoría las variantes para cada país en realidad no son tan diferentes, a excepción de algunas. Utilizando los datos recabados en la gráfica de bases, en la matriz de distancia y en el arbol filogenético, se puede asegurar que estas variantes en si son extremadamente similares y comparten la mayoría de las cualidades de las respectivas variantes del SARS-CoV-2 de cada país. Teniendo en cuenta lo anterior, si hubo variantes que, en las graficas de ACTG, distanciamiento o arbol filogenético variaban y diferían en gran medida de las demás. En la gráfica de ACTG teníamos a las de Alemania y Francia. En la matriz de distanciamiento, se tenian ciertas variantes cuyos recuadros si se marcaban en negro al momento de comparar su alineación con las de otros países y, finalmente, en el arbol filogenético, las variantes de Polonia, India y Russia fueron las más aisladas y distantes de las demás variantes.

Aunque si, puede que la mayoría de las veces estas variantes concordaran, no se debe olvidar que cada una es única y puede ser muy diferente de las otras aun y cuando los datos muestren que son extremadamente similares.

Finalmente, para concluir con la segunda pregunta, podríamos decir que no son muy diferentes, hasta cierto punto. Como ya se mencionó, la investgación ha arrojado que la mayoría de las variantes son muy similares entre sí y aunque existieran excepciones, es no quita el hecho de que las variantes provienen de un ancestro en comun y algo las hace similares. Ahora bien, si los datos se analizan a detalle, podríamos decir que las variantes Europeas y Asiáticas son las más diferentes, pues Francia, Alemania y Polonia tuvieron diferencias considerables las graficas de ACTG y del árbol filogenético, mientras que las de Rusia e India fueron distantes en el arbol filogenético. Por lo que podríamos decir que estas variantes son las que más difieren entre la Hispana y Africana.

En conclusión, esta investigación obtuvo que la mayoría de las variantes del SARS-CoV-2 son muy similares. Aunque hubo excepciones en algunos de los análisis, no podemos olvidar que estas variantes son únicas entre sí y que no solo por ser similares deben ser pasadas por desapercibidas. Sin duda, el SARS-CoV-2 es un virus que está dando seguirá dando mucho de que hablar.


```{r}
citation("ape")
citation("phytools")
citation("Biostrings")
citation("seqinr")
citation("adegenet")
citation("ggtree")
citation("DECIPHER")
citation("viridis")
citation("ggplot2")
citation("ggmsa")
```

_Referencias_

_Para las variantes del SARS-CoV-2:_

NCBI Virus. (2021). Nih.gov. https://www.ncbi.nlm.nih.gov/labs/virus/vssi/#/virus?SeqType_s=Nucleotide&VirusLineage_ss=Severe%20acute%20respiratory%20syndrome%20coronavirus%202%20(SARS-CoV-2),%20taxid:2697049

_Sitios Web y Publicaciones de Interés:_

Statista. (2021). Países con más casos de coronavirus | Statista.Statista. https://es.statista.com/estadisticas/1091192/paises-afectados-por-el-coronavirus-de-wuhan-segun-los-casos-confirmados/

National Center for Biotechnology Information. (2021). Página Inicial. NCBI: https://www.ncbi.nlm.nih.gov/

Haake, C., Cook, S., Pusterla, N., & Murphy, B. (2020). Coronavirus Infections in Companion Animals: Virology, Epidemiology, Clinical and Pathologic Features. Viruses, 12(9), 1023. https://doi.org/10.3390/v12091023

Rabaan AA;Al-Ahmed SH;Haque S;Sah R;Tiwari R;Malik YS;Dhama K;Yatoo MI;Bonilla-Aldana DK;Rodriguez-Morales AJ. (2020). SARS-CoV-2, SARS-CoV, and MERS-COV: A comparative overview. Le Infezioni in Medicina, 28(2). https://doi.org/

Roberts, A., Deming, D., Paddock, C. D., Cheng, A., Yount, B., Vogel, L., Herman, B. D., Sheahan, T., Heise, M., Genrich, G. L., Zaki, S. R., Baric, R., & Subbarao, K. (2007). A Mouse-Adapted SARS-Coronavirus Causes Disease and Mortality in BALB/c Mice. PLoS Pathogens, 3(1), e5. https://doi.org/10.1371/journal.ppat.0030005

Biurrun Cía J;García Martínez B;Pérez Montero A;Kochan G;Escors Murugarren D;Crespo Martínez J;Lasa Uzcudun I;Echarri Sucunza A. (2020). Ozone fails to disinfect emergency vehicles contaminated with viruses similar to SARS-CoV-2. Emergencias : Revista de La Sociedad Espanola de Medicina de Emergencias, 32(6). https://doi.org/




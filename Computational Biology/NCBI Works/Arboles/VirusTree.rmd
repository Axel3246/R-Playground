---
title: "Filogenetica con R"
author: "Axel Amos"
date: "26/4/2021"
output:
  html_document:
    df_print: paged
---
R phylogenetics se basa en los paquetes contribuidos para filogenética en R, y hay muchos de estos paquetes. Comencemos hoy instalando algunos paquetes críticos, como ape, phangorn, phytools y geiger. Para obtener la versión CRAN más reciente de estos paquetes, ¡necesitará tener R 3.3.x instalado en su computadora!

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
Podemos verificar que tenemos las mismas versiones de estos paquetes instalados utilizando la función base packageVersion:
```{r}
packageVersion("ape")
```
```{r}
packageVersion("phangorn")
```
```{r}
packageVersion("phytools")
```
```{r}
packageVersion("geiger")
```
_El objeto "phylo" en R_
Ahora hemos instalado paquetes críticos (ape, phangorn, phytools, geiger). El paquete principal más importante para las filogenias en R se llama "ape", que significa Análisis de Filogenética y Evolución en R. El paquete lo puedes consultar en: APE
```{r}
#library(ape)
text.string<-
    "(lemur,humano);"
vert.tree1<-read.tree(text=text.string)
plot(vert.tree1,no.margin=TRUE,edge.width=2)
```
```{r}
text.string<-"(murcielago,(lemur,humano));"
vert.tree2<-read.tree(text=text.string)
plot(vert.tree2,no.margin=TRUE,edge.width=2)
```
```{r}
text.string<-"(((vaca, cerdo),ballena),(murcielago,(lemur,humano)));"
vert.tree3<-read.tree(text=text.string)
plot(vert.tree3,no.margin=TRUE,edge.width=2)
```
_Genera el codigo para el siguiente arbol: Humanos, Chimpances, Bonobos, Gorillas, Orangutanes_
```{r}
text.string<-"((Orangutanes), (Gorilas,((Bonobos,Chimpances),Humanos)));"
vert.tree4<-read.tree(text=text.string)
plot(vert.tree4,no.margin=TRUE,edge.width=2)
```

_Un ejemplo con phytools:_
phytools: Phylogenetic Tools for Comparative Biology (and Other Things) phytools
```{r}
#library(phytools)
roundPhylogram(vert.tree3)
```

Los árboles sin raiz, unrooted , ilustran la relación de los nodos de las hojas sin hacer suposiciones sobre la ascendencia. No requieren que la raíz ancestral sea conocida o inferida. Los árboles no enraizados siempre se pueden generar a partir de los enraizados simplemente omitiendo la raíz. Por el contrario, inferir la raíz de un árbol sin raíz requiere algunos medios para identificar la ascendencia. Esto normalmente se hace incluyendo un grupo externo en los datos de entrada para que la raíz esté necesariamente entre el grupo externo y el resto de los taxones en el árbol, o introduciendo suposiciones adicionales sobre las tasas de evolución relativas en cada rama.
```{r}
plot(unroot(vert.tree3),type="unrooted",no.margin=TRUE,lab4ut="axial",
    edge.width=2)
```

El objeto creado en la memoria cuando simulamos o estimamos una filogenia, o leemos una de un archivo de entrada, es una lista de la clase "filo".

Recuerde, una lista es solo un tipo de objeto personalizable que puede combinar diferentes objetos de diferentes tipos. Por ejemplo, una lista puede tener un vector de números reales (con el modo "numérico") como primer elemento; y luego un vector de cadenas (con el modo "carácter") como su segundo elemento; y así. Asignar nuestro árbol con una clase especial, "phylo", es solo una forma conveniente de indicarle a las funciones especiales en R cómo tratar ese objeto.

Un objeto de la clase "phylo" tiene al menos tres partes. Estos normalmente están ocultos, por ejemplo, simplemente escribir el nombre de su objeto "phylo" no le da la estructura en la memoria, como lo hace para muchos objetos R.
```{r}
vert.tree3
```
```{r}
str(vert.tree3)
```

Para comprender la estructura de un objeto "filo" un poco más claramente, leamos una filogenia mucho más simple, y luego descomponga en sus componentes esenciales:
```{r}
tree<-read.tree(text="(((A,B),(C,D)),E);")
plotTree(tree,offset=1)
tiplabels()
nodelabels()
```

Caso de estudio:

_A Novel Coronavirus from Patients with Pneumonia in China, 2019_

"In December 2019, a cluster of patients with pneumonia of unknown cause was linked to a seafood wholesale market in Wuhan, China. A previously unknown betacoronavirus was discovered through the use of unbiased sequencing in samples from patients with pneumonia. Human airway epithelial cells were used to isolate a novel coronavirus, named 2019-nCoV, which formed a clade within the subgenus sarbecovirus, Orthocoronavirinae subfamily. Different from both MERS-CoV and SARS-CoV, 2019-nCoV is the seventh member of the family of coronaviruses that infect humans. Enhanced surveillance and further investigation are ongoing. (Funded by the National Key Research and Development Program of China and the National Major Project for Control and Prevention of Infectious Disease in China.)"

El trabajo de China Novel Coronavirus Investigating and Research Team

```{r}
virus <- c(  "JX869059", "AY508724", "MN908947", "AY390556", "AY278489", "MN985325","AY485277","MT292571")
#library(biostrings)
#library(seqinr)
#library(ggtree)
#library(ape)
#library(DECHIPHER)
#library(viridis)
#library(ggplot2)
```
_2. Obten las secuencias._
```{r}
virus_sequences <- read.GenBank(virus)
```
_3. Estructura del DNABin._
```{r}
str(virus_sequences)
```
```{r}
attributes(virus_sequences)
```
```{r}
names(virus_sequences)
```
```{r}
attr(virus_sequences, "species")
```
```{r}
write.dna(virus_sequences,  file ="virus_seqs.fasta", format = "fasta", append =
FALSE, nbcol = 6, colsep = " ", colw = 10)
```

```{r}
virus_seq_not_align <- readDNAStringSet("virus_seqs.fasta", format = "fasta")

```
```{r}
virus_seq_not_align
```
_6. Alineamiento de las secuencias: Un alineamiento múltiple de secuencias es un alineamiento de más de dos secuencias. Estas secuencias, como en el caso de los alieamientos por parejas pueden ser ADN, ARN o proteína. Las aplicaciones más habituales de los alineamientos múltiples son:_
-la reconstrucción filogenética,
-el análisis estructural de proteínas,
-la búsqueda de dominios conservados y
-la búsqueda de regiones conservadas en promotores. En todos los casos los algoritmos de alineamiento múltiple asumen que las secuencias que estamos -alineando descienden de un antepasado común y lo que intentamos hacer es alinear las posiciones homólogas.
```{r}
virus_seq_not_align <- OrientNucleotides(virus_seq_not_align)
```
```{r}
virus_seq_align <- AlignSeqs(virus_seq_not_align)
```
_7. Visualizar el resultado del alineamiento:_
```{r}
BrowseSeqs(virus_seq_align, highlight=0)
```
_8. Guardar el resultado:_
```{r}
writeXStringSet(virus_seq_align, file="virus_seq_align.fasta")
```
_9. Obtener el nuevo archivo:_
```{r}
virus_aligned <- read.alignment("virus_seq_align.fasta", format = "fasta")
```
_10. Crear una matriz de distancia_
```{r}
matriz_distancia <- dist.alignment(virus_aligned, matrix = "similarity")
```
_11. Visualiza la matriz de distancia: donde sombras más oscuras de gris significan una mayor distancia_
```{r}
temp <- as.data.frame(as.matrix(matriz_distancia))
table.paint(temp, cleg=0, clabel.row=.5, clabel.col=.5) + scale_color_viridis()
```
_12, Creación del árbol con el paquete ape:_
```{r}
virus_tree <- nj(matriz_distancia)
class(virus_tree) 
```
```{r}
virus_tree <- ladderize(virus_tree)
```
_13. Plot del árbol:_
```{r}
plot(virus_tree, cex = 0.6)
title("A Novel Coronavirus from Patients with Pneumonia in China, 2019")
```

Parte de ggplot
```{r}
ggtree(virus_tree)
```

```{r}
ggtree(virus_tree, layout="slanted") 
```
```{r}
ggtree(virus_tree, layout="circular")
```
```{r}
ggtree(virus_tree, layout="fan", open.angle=120)
```
```{r}
ggtree(virus_tree, layout="rectangular")
```
```{r}
ggtree(virus_tree, branch.length='none')
```
```{r}
ggtree(virus_tree, branch.length='none', layout='circular')
```
```{r}
ggtree(virus_tree ) + geom_tiplab()
```
_15. Visualiza el alineamiento de las secuencias:_
```{r}
#library(ggmsa)
ggmsa(virus_seq_not_align, 320, 360, color = "Chemistry_AA")
```
_16. Combina el árbol filogenético con el alineamiento de las secuencias:_
```{r}
plot_virus <- ggtree(virus_tree ) + geom_tiplab()

data = tidy_msa(virus_seq_not_align, 164, 213)
```
```{r}
plot_virus + geom_facet(geom = geom_msa, data = data,  panel = 'msa', color = "Chemistry_AA") +
    xlim_tree(1)
```
_Citar paquetes de R_

Citar los paquetes, módulos y softwares que usaste para tu análisis es importante, tanto desde una perspectiva de reproducibilidad (las rutinas estadísticas a menudo se implementan de diferentes maneras por diferentes paquetes, lo que podría explicar ligeras discrepancias en los resultados. 

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







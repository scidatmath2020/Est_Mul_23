###################################################################
###################      k vecinos mas cercanos      ##############
###################              knn                 ##############
###################################################################

setwd("C:\\Users\\hp master\\Documents\\SciData\\Est_mult_23")

#install.packages("class")

library(tidyverse)
library(GGally)
library(class)


iris_muestra = read.csv("iris_muestra.csv")

### Dividimos la tabla en las columnas numéricas y la columna
### de los grupos

iris_muestra_X = iris_muestra[,1:4]
iris_muestra_Y = iris_muestra[,5]

### Calculamos número de filas

n = nrow(iris_muestra_X)

### Matriz de nubes de puntos separando por especies

ggpairs(data=iris_muestra,
        mapping = aes(color=Species),
        columns = 1:4
) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_blank(),
        axis.title = element_blank()
  )

### Preparamos los candidatos a número de vecinos k

num_vecinos = 1:(sqrt(n)+1)

### Función que para cada K regresa una lista con tres elementos:
### 1: clasificación que la computadora hace con esos K vecinos
### 2: tabla de contingencia (es decir, cuántos clasificó en cada tipo)
### 3: Número de errores cometidos

vecinos_cercanos = function(K){
set.seed(2020)  
knn.clases = knn.cv(train=iris_muestra_X,
                    cl=iris_muestra_Y,
                    k=K)
knn.contingencias = table(iris_muestra_Y,knn.clases)
knn.errores = n - sum(iris_muestra_Y==knn.clases)

return(list(knn.clases,
            knn.contingencias,
            knn.errores))
}

### APlicamos la función anterior a todos nuestros candidatos a k
resultados = lapply(num_vecinos,vecinos_cercanos)


### Obtenemos los errores cometidos en cada k

errores = c()

for(i in num_vecinos){
  errores = c(errores,resultados[[i]][[3]])
}

which(errores == min(errores))


### Definimos nuestro k óptimo

knn_optimo = 8 
modelo_optimo = resultados[[knn_optimo]][[1]]
resultados[[knn_optimo]][[2]]
resultados[[knn_optimo]][[3]]


### error cometido en %
100*resultados[[knn_optimo]][[3]]/n
 
### Añadimos una columna para indicar si el cálculo se hizo bien
iris_resultados = iris_muestra %>% mutate(clasificador = if_else(Species==modelo_optimo,
                                                 "B","M"))

### Graficamos
ggpairs(data=iris_resultados,
        mapping = aes(color=clasificador),
        columns = 1:4
) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_blank(),
        axis.title = element_blank()
  )











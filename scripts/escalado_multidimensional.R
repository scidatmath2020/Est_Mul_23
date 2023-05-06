###################################################################
######################### Análisis de factores ####################
#########################       EFA            ####################
###################################################################

library(tidyverse)

setwd("C:\\Users\\hp master\\Documents\\SciData\\Est_mult_23")

D = read.csv("distancias_Puebla.csv")

D = D[,-1]

#### Aplicamos el modelo

mds.ciudades = cmdscale(D,eig=TRUE)

#### Preparamos un dataframe con los eigenvalores obtenidos

eigenvalores = data.frame(num_eigenval=1:length(mds.ciudades$eig),
           eigenval = mds.ciudades$eig)

#### Graficamos los eigenvalores obtenidos
ggplot(data=eigenvalores) +
  geom_point(mapping=aes(x=num_eigenval,y=eigenval))

#### Calculamos la precisión del modelo
precision = sum(mds.ciudades$eig[1:2])/sum(abs(mds.ciudades$eig[1:2]))
precision

#### Armamos el dataframe de componentes
Y = as.data.frame(mds.ciudades$points)

names(Y)

#### Graficamos las componentes

ggplot(data = Y) +
  geom_point(mapping=aes(y=V1,x=V2)) 

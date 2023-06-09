library(tidyverse) # funciones esenciales para análisis de datos
library(rayshader) # graficador especial para mapas en 3D
library(ggExtra) # Para construir nubes de puntos con histogramas y densidades
library(GGally) # Para construir matrices de nubes de puntos y coordenadas paralelas
library(scatterplot3d) # Para construir nubes de puntos con tres columnas
library(reshape) # Para pasar tablas de formato ancho a largo y viceversa


#########################################################################
########################   Lectura de archivos   ########################
#########################################################################

setwd("C:\\Users\\hp master\\Documents\\SciData\\Est_mult_23")
iris <- read.csv("iris.csv")

#########################################################################
###############   Histogramas y densidades por columna   ################
#########################################################################

ggplot(data=iris) +
  geom_histogram(mapping=aes(x=Sepal.Width),
                 fill="red",
                 color="white")

ggplot(data=iris) +
  geom_histogram(mapping=aes(x=Sepal.Width,
                             fill=Species),
                 alpha=0.5,
                 color="white")

ggplot(data=iris) +
  geom_histogram(mapping=aes(x=Sepal.Width,
                             y=stat(density)),
                 fill="red",
                 color="black") +
  geom_density(mapping=aes(x=Sepal.Width),
               fill="red",
               color="black",
               alpha=0.5)

ggplot(data=iris) +
  geom_histogram(mapping=aes(x=Sepal.Width,
                             y=stat(density),
                             fill=Species)) +
  geom_density(mapping=aes(x=Sepal.Width,
                           fill=Species),
               color="black",
               alpha=0.5)

#########################################################################
###############   Densidades conjuntas de dos columnas   ################
#########################################################################

########### Curvas de contorno

iris_setos_versi = iris %>% filter(Species=="setosa" | Species=="versicolor")

m <- ggplot(data=iris_setos_versi) +
      stat_density_2d(mapping=aes(x=Sepal.Width,
                                  y=Sepal.Length,
                                  fill=..level..,
                                  group=Species,
                                  color=Species),
                      geom="polygon",
                      show.legend = FALSE) +
      geom_density_2d(mapping=aes(x=Sepal.Width,
                                   y=Sepal.Length,
                                   color=Species),
                      show.legend=FALSE) +
      ylim(4,8.2) +
      theme(panel.background = element_rect("white"),
            panel.grid = element_line(color="darkgrey",size=0.1))
  
m  

########### Tridimensional  

plot_gg(m)

#########################################################################
#########################   Nubes de puntos   ###########################
#########################################################################

########### Nube de puntos sin separar en especies

p = ggplot(data=iris) +
  geom_point(mapping=aes(x=Sepal.Width,y=Sepal.Length),color="red")

########### Nube de puntos sin separar en especies con histogramas y densidades

ggMarginal(p,type="histogram",fill="red")
ggMarginal(p,type="densigram",fill="red")
ggMarginal(p,fill="red")

########### Nube de puntos separando en especies

r = ggplot(data=iris) +
  geom_point(mapping=aes(x=Sepal.Width,y=Sepal.Length,color=Species))

########### Nube de puntos separando en especies con histogramas y densidades

ggMarginal(r,type="histogram",groupColor=TRUE,groupFill = TRUE,alpha=0.5)
ggMarginal(r,type="densigram",groupFill = TRUE,alpha=0.5)
ggMarginal(r,groupFill = TRUE,alpha=0.5)


########### Matriz de nubes de puntos sin información extra

ggpairs(data=iris,
        mapping = aes(color=Species,alpha=0.01),
        columns=1:4,
        upper = list(continuous = "points")) +
  theme(panel.background = element_rect("#202020"),
        panel.grid = element_line("blue"),
        axis.title = element_blank())

########### Matriz de nubes de puntos con información extra

ggpairs(data=iris,
        mapping = aes(color=Species,alpha=0.01),
        columns=1:4) +
  theme(panel.background = element_rect("#202020"),
        panel.grid = element_line("blue"),
        axis.title = element_blank())

########### Nube de puntos para tres columnas

colores = c("blue","green","red")
colores = colores[as.factor(iris$Species)]
scatterplot3d(iris[,c(2,1,3)],pch=19,color=colores)


#########################################################################
#################   Mapa de calor de correlaciones   ####################
#########################################################################


correlaciones = cor(iris[,1:4])
correlaciones

df = melt(correlaciones)

colnames(df) = c("x","y","correlación")

ggplot(df, aes(x = x, y = y, fill = correlación)) +
  geom_tile(color="white") +
  geom_text(aes(label = round(correlación,2)), color = "white", size = 8) +
  scale_fill_gradient2(low="white",
                       mid="orange", high="red",
                       guide=guide_colorbar(ticks=FALSE,
                                            barheight=5, limits=c(-1,1))) 
  


#########################################################################
#######################   Coordenadas paralelas   #######################
#########################################################################

ggparcoord(data = iris,
           columns = c(1,4,3,2),
           #alphaLines = 0.1,
           boxplot = TRUE,
           groupColumn = "Species",           
           showPoints = TRUE)  +
  scale_color_brewer(palette = "Set2") +
  theme(panel.background = element_rect("#202020"),
        panel.grid = element_blank(),
        axis.title = element_blank())


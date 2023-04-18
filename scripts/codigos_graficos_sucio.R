# install.packages("tidyverse")

#install.packages("scatterplot3d")

library(tidyverse) # funciones esenciales para análisis de datos
library(rayshader) # graficador especial para mapas en 3D
library(ggExtra) # Para construir nubes de puntos con histogramas y densidades
library(GGally) # Para construir matrices de nubes de puntos y coordenadas paralelas
library(scatterplot3d) # Para construir nubes de puntos con tres columnas
library(reshape) # Para pasar tablas de formato ancho a largo y viceversa


data(iris)

iris %>% names()

#########################################################################
########################   Lectura de archivos   ########################
#########################################################################

setwd("C:\\Users\\hp master\\Documents\\SciData\\Est_mult_23")
getwd()

iris <- read.csv("iris.csv")

#########################################################################
###############   Histogramas y densidades por columna   ################
#########################################################################

iris %>% names()

ggplot(data=iris) +
  geom_point(mapping=aes(x=Sepal.Width,y=Sepal.Length),
             color="red")

grafico_especies <- ggplot(data=iris) +
  geom_point(mapping=aes(x=Sepal.Width,y=Sepal.Length,color=Species),
             ) 
  
grafico_especies

ggsave("mi_propia_grafica.pdf",device="pdf")

?ggsave()


##################################################

iris %>% names()

sturges = as.integer(1+log2(150))

ggplot(data=iris) +
  geom_histogram(mapping=aes(x=Petal.Length),
                 fill="red",
                 color="darkblue") +
  labs(title = "Mi histograma",
       subtitle = "Basado en Iris") +
  theme(
    panel.background = element_rect("black"), 
    panel.grid = element_blank(),
  )

?theme

ggplot(data=iris) +
  geom_histogram(mapping=aes(x=Sepal.Width,
                             fill=Species),
                 alpha=0.5,
                 color="white") + 
  scale_fill_manual(values=c("darkblue","blue","orange"))

ggplot(data=iris) +
  geom_histogram(mapping=aes(x=Sepal.Width,
                             y=stat(density)),
                 fill="red",
                 color="black") +
  geom_density(mapping=aes(x=Sepal.Width),
               fill="yellow",
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

ggplot(data=iris) +
  geom_density(mapping=aes(x=Sepal.Width,
                           fill=Species),
               color="black",
               alpha=0.5)

##### faceteado

### Convertir la tabla iris a formato ordenado (tidy o largo)
iris_ordenado = iris %>% gather(key="Medida",value="Valor",c(1:4))

ggplot(data=iris_ordenado) +
  geom_histogram(mapping=aes(x=Valor),
                 fill="red",
                 color="darkblue") +
  labs(title = "Mi histograma",
       subtitle = "Basado en Iris") +
  theme(
    panel.background = element_rect("black"), 
    panel.grid = element_blank(),
  ) +
  facet_wrap(~Medida)

########### Histograma de longitud de sépalo utilizando
########### el formato largo

### Esto significa filtrar
iris_ordenado %>% filter(Medida == "Sepal.Length")

iris_ordenado %>% filter(Medida=="Sepal.Length") %>%
ggplot() +
  geom_histogram(mapping=aes(x=Valor),
                 fill="red",
                 color="darkblue") +
  labs(title = "Mi histograma",
       subtitle = "Basado en Iris") +
  theme(
    panel.background = element_rect("black"), 
    panel.grid = element_blank(),
  ) 


#########################################################################
###############   Densidades conjuntas de dos columnas   ################
#########################################################################

########### Curvas de contorno

iris_setos_versi = iris %>% filter(Species=="setosa" | Species=="versicolor")

iris_setos_versi


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


iris_setos_virgi = iris %>% filter(Species=="setosa" | Species=="virginica")

t <- ggplot(data=iris_setos_virgi) +
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

t

########### Tridimensional  

s = plot_gg(m)

s

plot_gg(t)

#########################################################################
#########################   Nubes de puntos   ###########################
#########################################################################

########### Nube de puntos sin separar en especies
# Operadores de asignación: <- y =
p <- ggplot(data=iris) +
  geom_point(mapping=aes(x=Sepal.Width,y=Sepal.Length),color="red")

ggsave("mi_nube.png")

########### Nube de puntos sin separar en especies con histogramas y densidades

ggMarginal(p,type="histogram",fill="red")
ggMarginal(p,type="densigram",fill="red")
ggMarginal(p,fill="red")

r

### Sin nombrar gráficos

ggMarginal(ggplot(data=iris) +
             geom_point(mapping=aes(x=Sepal.Width,y=Sepal.Length),color="darkblue"),
           fill="red")

########### Nube de puntos separando en especies

r = ggplot(data=iris) +
  geom_point(mapping=aes(x=Sepal.Width,y=Sepal.Length,color=Species)) +
  scale_color_manual(values = c("blue","red","orange"))

########### Nube de puntos separando en especies con histogramas y densidades

ggMarginal(r,type="histogram",groupFill = TRUE,alpha=0.5)
ggMarginal(r,type="densigram",groupFill = TRUE,alpha=0.5)
ggMarginal(r,groupFill = TRUE,alpha=0.5)


########### Matriz de nubes de puntos sin información extra

View(iris)

ggpairs(data=iris,
        mapping = aes(color=Species,alpha=0.01),
        columns=1:4,
        upper = list(continuous = "points")) +
  theme(panel.background = element_rect("#202020"),
        panel.grid = element_line("blue"),
        #axis.title = element_blank()
        )

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

scatterplot3d(iris[,c(2,1,3)],pch=19,color=colores,type="h")

scatterplot3d(iris[,c(2,1,3)],pch=25,color=colores,type="h",bg="blue")

scatterplot3d(iris[,c(2,1,3)],pch=19,color="red",type="h")

?scatterplot3d

#########################################################################
#################   Mapa de calor de correlaciones   ####################
#########################################################################


correlaciones = cor(iris[,1:4])
correlaciones


df = melt(correlaciones)
df
colnames(df) = c("x","y","correlación")

df

ggplot(data = df,mapping=aes(x = x, y = y, fill = correlación)) +
  geom_tile(color="white") +
  geom_text(mapping=aes(label = round(correlación,2)), color = "white", size = 8) +
  scale_fill_gradient2(low="white",
                       mid="orange", high="red",
                       guide=guide_colorbar(ticks=FALSE,
                                            barheight=5, limits=c(-1,1))) 

ggplot(data = df) +
  geom_tile(mapping=aes(x = x, y = y, fill = correlación),color="white") +
  geom_text(mapping=aes(x=x, y=y,label = round(correlación,2)), color = "white", size = 8) +
  scale_fill_gradient2(low="white",
                       mid="orange", high="red",
                       guide=guide_colorbar(ticks=FALSE,
                                            barheight=5, limits=c(-1,1))) 


ggplot(data = df) +
  geom_tile(mapping=aes(x = x, y = y, fill = correlación),color="white") +
#  geom_text(mapping=aes(x=x, y=y,label = round(correlación,2)), color = "white", size = 8) +
  scale_fill_gradient2(low="white",
                       mid="orange", high="red",
                       guide=guide_colorbar(ticks=FALSE,
                                            barheight=5, limits=c(-1,1))) 



#########################################################################
#######################   Coordenadas paralelas   #######################
#########################################################################

iris %>% colnames()


ggparcoord(data = iris,
           columns = c(1,4,3,2),
           #alphaLines = 0.1,
           #boxplot = TRUE,
           groupColumn = "Species",           
           showPoints = TRUE) +
  scale_color_brewer(palette = "Set2") +
  theme(panel.background = element_rect("#202020"),
        panel.grid = element_blank(),
        axis.title = element_blank()) 



medias = iris %>% group_by(Species) %>% summarize(Sepal.Length = mean(Sepal.Length),
                                                  Sepal.Width = mean(Sepal.Width),
                                                  Petal.Length = mean(Petal.Length),
                                                  Petal.Width = mean(Petal.Width))

medias


medias = medias[,c(2,3,4,5,1)]

iris = iris %>% mutate(tipo="flor")
medias = medias %>% mutate(tipo="media")

iris %>% names()
medias %>% names()

nva_iris = rbind(iris,medias)

nva_iris


?ggparcoord

ggparcoord(data = nva_iris,
           columns = c(1,4,3,2),
           #alphaLines = 0.1,
           #boxplot = TRUE,
           groupColumn = "tipo",           
           showPoints = TRUE) +
  scale_color_brewer(palette = "Set2") +
  theme(panel.background = element_rect("#202020"),
        panel.grid = element_blank(),
        axis.title = element_blank()) 




iris %>% names()


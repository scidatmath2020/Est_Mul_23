# -*- coding: utf-8 -*-
"""
Created on Tue Apr 18 08:39:48 2023

@author: hp
"""

import pandas as pd
import numpy as np
from siuba import *
from plotnine import *
import seaborn as sns
import matplotlib.pyplot as plt
import plotly.express as px
from pandas.plotting import parallel_coordinates

from plotly.offline import plot # en Jupyter esta línea no es necesaria

#%%

#########################################################################
########################   Lectura de archivos   ########################
#########################################################################

import os

os.chdir("C:\\Users\\hp master\\Documents\\SciData\\Est_mult_23")

iris = pd.read_csv("iris.csv")

#%%

#### Cambiar nombres a las columnas

iris.columns

iris = (iris >> rename(Sepal_Length = "Sepal.Length",
               Sepal_Width = "Sepal.Width",
               Petal_Length = "Petal.Length",
               Petal_Width = "Petal.Width")
)

iris.columns

#%%
#########################################################################
###############   Histogramas y densidades por columna   ################
#########################################################################



#### Histograma absoluto del ancho de sépalo sin distinguir especies

(ggplot(data=iris) +
  geom_histogram(mapping=aes(x="Sepal_Width"),
                 fill="red",
                 color="white")
)

#### Histograma relativo del ancho de sépalo sin distinguir especies
#### incluyendo densidad

(ggplot(data=iris) +
  geom_histogram(mapping=aes(x="Sepal_Width",
                             y="..density.."),
                 fill="red",
                 color="black") +
  geom_density(mapping=aes(x="Sepal_Width"),
               fill="red",
               color="black",
               alpha=0.5)
)

#### Histograma de longitud de sépalo separado por especies

iris_pl_setosa = (iris >> filter(_.Species=="setosa") >> select(_.Petal_Length)).squeeze()
iris_pl_virginica = (iris >> filter(_.Species=="virginica") >> select(_.Petal_Length)).squeeze()
iris_pl_versicolor = (iris >> filter(_.Species=="versicolor") >> select(_.Petal_Length)).squeeze()

plt.hist(iris_pl_setosa,alpha=0.5)
plt.hist(iris_pl_virginica,alpha=0.5)
plt.hist(iris_pl_versicolor,alpha=0.5)

#### Histograma de cada columna sin separar por especies

### Convertir la tabla iris a formato ordenado (tidy o largo)
iris_ordenado = iris >> gather("Medida","Valor",-_["Species"])

(ggplot(data=iris_ordenado) +
      geom_histogram(mapping=aes(x="Valor"),
                     fill="red",
                     color="darkblue",
                     bins=30) +
      labs(title = "Mi histograma",
           subtitle = "Basado en Iris") +
      theme(
        panel_background = element_rect("black"), 
        panel_grid = element_blank(),
      ) +
      facet_wrap("~Medida")
)


#### Densidad de la longitud de pétalo separado por especies

(ggplot(data=iris) +
  geom_density(mapping=aes(x="Petal_Length",
                           fill="Species"),
               color="black",
               alpha=0.5)
)

#### Histograma con densidad de la longitud de pétalo separado por especies
sns.distplot(iris_pl_setosa)
sns.distplot(iris_pl_virginica)
sns.distplot(iris_pl_versicolor)
plt.show()

#%%

#########################################################################
#########################   Nubes de puntos   ###########################
#########################################################################

########### Nube de puntos sin separar en especies

p = (ggplot(data=iris) +
  geom_point(mapping=aes(x="Sepal_Width",y="Sepal_Length"),color="red")
)

p

########### Nube de puntos separando en especies

r = (ggplot(data=iris) +
  geom_point(mapping=aes(x="Sepal_Width",y="Sepal_Length",color="Species")) +
  scale_color_manual(values=["red","orange","blue"])
)

r

########### Nube de puntos sin separar en especies con histogramas 

sns.jointplot(data=iris,x="Sepal_Length",y="Sepal_Width")

########### Nube de puntos separando en especies con densidades

sns.jointplot(data=iris,x="Sepal_Length",y="Sepal_Width",hue="Species")

########### Matriz de nubes de puntos sin información extra

sns.pairplot(iris,hue="Species")

########### Nube de puntos para tres columnas

nube_3d_sin_especies = px.scatter_3d(iris,x="Sepal_Length",y="Sepal_Width",z="Petal_Length")
plot(nube_3d_sin_especies)

nube_3d_con_especies = px.scatter_3d(iris,x="Sepal_Length",y="Sepal_Width",z="Petal_Length",color="Species")
plot(nube_3d_con_especies)

#%%
#########################################################################
###############   Densidades conjuntas de dos columnas   ################
#########################################################################

setosa = iris >> filter(_.Species == "setosa")
virginica = iris >> filter(_.Species == "virginica")

sns.kdeplot(data=setosa, x="Sepal_Width",y="Sepal_Length",
            cmap="Reds",fill=True,thresh=0.05)
sns.kdeplot(data=virginica, x="Sepal_Width",y="Sepal_Length",
            cmap="Blues",fill=True,thresh=0.05)


#########################################################################
#################   Mapa de calor de correlaciones   ####################
#########################################################################

correlaciones = (iris >> select(-_.Species)).corr()

sns.heatmap(data=correlaciones,
            vmax=1,vmin=-1,
            cmap="Reds",
            linecolor="white",linewidth=0.5,
            square=True,
            annot=True,fmt=".2f")

#########################################################################
#######################   Coordenadas paralelas   #######################
#########################################################################

orden = ["Sepal_Length","Petal_Width","Petal_Length","Sepal_Width","Species"]

parallel_coordinates(iris[orden], "Species",colormap="cool")



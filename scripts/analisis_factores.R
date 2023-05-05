library(tidyverse)
library(GGally)
library(reshape)
library(scatterplot3d) 


# Medidas de textura de un alimento tipo pastas.

# Aceite: porcentaje de aceite en la masa
# Densidad: la densidad del producto 
# (cuanto mayor sea el número, más denso será el producto)
# Crujiente: una medida de crujiente, en una escala de 7 a 15, siendo 15 más crujiente.
# Fractura: el ángulo, en grados, a través del cual la pasta 
# se puede doblar lentamente antes de que se rompa.
# Dureza: se utiliza una punta afilada para medir 
# la cantidad de fuerza necesaria antes de que se produzca la rotura.


food <- read.csv("https://userpage.fu-berlin.de/soga/300/30100_data_sets/food-texture.csv",
                 row.names = "X")


mis_analisis_factoriales = list()

analisis_factorial = function(num_factores){
  mis_analisis_factoriales[[num_factores]] = factanal(x=food,
           factors = num_factores,
           scores = "Bartlett",
           rotation = "varimax")
}

N=1
while(analisis_factorial(N)$PVAL <= 0.05){
  N=N+1
}

factores = mis_analisis_factoriales[[N]]
factores

Factores = data.frame("factor_1" = factores$loadings[,1],
                      "factor_2" = factores$loadings[,2])

rownames(Factores) = names(factores$loadings[,1])

ggplot(data = Factores) +
  geom_point(mapping=aes(x=factor_1,y=factor_2,color="red")) +
  geom_text(mapping=aes(x=factor_1,y=factor_2,label=rownames(Factores))) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)
  










# Setup -------------------------------------------------------------------
rm(list = ls())
gc(verbose = T, reset = T, full = T)

# Packages ----------------------------------------------------------------
if(!require(tidyverse)){install.packages("tidyverse")}
if(!require(gganimate)){install.packages("gganimate")}



# Parameters --------------------------------------------------------------
cl = 200; ca = -2


# Functions ---------------------------------------------------------------

# Função Demanda
D <- function(p){
  demand = cl + ca * p
  return(demand)
}

# Função Preço
P <- function(d){
  price = (d - cl) / ca
  return(price)
}

# Pontos em que a curva Demanda cruza os eixos x e y 
D(10); P(D(10))
D(0); P(D(0))

p1 = 90; p2 = 70
df <- data.frame(
  y = c(D(1:100)),
  x = c(1:100),
  fr = 1:100
); df

attach(df)
ggplot(df)+
  geom_segment(aes(x = x[1], y = y[1], xend = x[100], yend = y[100]))+ # Curva Demanda
  geom_rect(aes(xmin = 0, ymin = 0, xmax = x, ymax = y), alpha = 0.3, fill = "steelblue") + # Quadrado principal
  geom_point(aes(x = x, y = y))+
  labs(
    title = "maximum product", 
    caption = "", 
    x = "Quantidade", 
    y= "Preço")+
  transition_time(fr)+
  theme_void()
  

colours()


#######################################
# R Script para preco_otimo_code.R    #
# Autor: Guilherme Viegas             #
# Data: 20200430                      #
#######################################

# Setup -------------------------------------------------------------------
rm(list = ls())
gc(verbose = T, reset = T, full = T)

# Packages ----------------------------------------------------------------

if(!require(devtools)){install.packages("devtools")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(gganimate)){install.packages("gganimate")}
if(!require(gifski)){install.packages("gifski")}
if(!require(magick)){install.packages("magick")}
if(!require(rgdal)){install.packages("rgdal")}
if(!require(sf)){install.packages("sf")}
if(!require(transformr)){devtools::install_github("thomasp85/transformr")}

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

df <- data.frame(
  y = c(D(1:100)),
  x = c(1:100),
  prod = c(D(1:100)) * c(1:100), # Preço * Quantidade
  fr = 1:100,
  gr = "gr"
); df

attach(df)
gg1 <- ggplot(df)+
  geom_segment(aes(x = x[1], y = y[1], xend = x[100], yend = y[100]), size = 2)+ # Curva Demanda
  geom_segment(aes(x = 0, y = y, xend = x, yend = y), linetype = "dashed")+
  geom_segment(aes(x = x, y = 0, xend = x, yend = max(y)), linetype = "dashed")+
  geom_rect(aes(xmin = 0, ymin = 0, xmax = x, ymax = y), alpha = 0.26, fill = "steelblue") + # Quadrado principal
  geom_point(aes(x = x, y = y), size = 5)+
  labs(title = "", 
       caption = "", 
       x = "Quantidade", 
       y= "Preço")+
  transition_time(fr)+
  theme_minimal()+
  theme(axis.text = element_blank(),
        axis.title = element_text(size = 28, face = "bold"))

gg1_gif <- animate(gg1, width = 600, height = 360)

# Lê a animação
gg1_mgif <- image_read(gg1_gif)
gg1_mgif

# Product function --------------------------------------------------------

fd <- function(x){y <- x * D(x); return(y)}
gg2 <- ggplot(df)+
  geom_point(aes(x = x, y = prod), size = 5)+
  geom_segment(aes(x = 0, y = prod, xend = x, yend = prod), linetype = "dashed")+
  geom_segment(aes(x = x, y = 0, xend = x, yend = prod), linetype = "dashed")+
  stat_function(aes(x), fun=fd, size = 2)+
  labs(title = "",
       caption = "",
       x = "Quantidade",
       y= "Receita")+
  transition_time(fr)+
  # theme_classic()+
  theme_minimal()+
  theme(axis.text = element_blank(),
        axis.title = element_text(size = 28, face = "bold"))

gg2_gif <- animate(gg2, width = 600, height = 360)

# Lê a animação
gg2_mgif <- image_read(gg2_gif)
gg2_mgif

# Joining the gifs --------------------------------------------------------

# Juntando tudo num mesmo plot
gg3_mgif <- magick::image_append(c(gg2_mgif[1], gg1_mgif[1]), stack = T)
for(i in 2:100){
  combined <- image_append(c(gg2_mgif[i], gg1_mgif[i]), stack = T)
  gg3_mgif <- c(gg3_mgif, combined)
}; rm(combined)
gg3_mgif

# save_animation(gg3_mgif, "max_receita.gif")
library(devtools)
library(flametree)
library(viridis)
library(ggplot2)

dat <- flametree_grow(seed = 1, time = 5, 
                      scale = 2,
                      split = 5) # data structure
img <- flametree_plot(tree = dat,
                      palette = "viridis::viridis")          # ggplot object


dat2 <- flametree_grow(seed = 1, time = 6, 
                       scale = 2,
                       split = 4) # data structure
img2 <- flametree_plot(tree = dat,
                       palette = "grDevices::rainbow")  

flametree_save(img2, 'img2.png')
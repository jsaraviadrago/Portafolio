# Network of teams World cup


library(data.table)
library(ggplot2)
library(igraph)
library(ggraph)
library(dplyr)
library(tidygraph)



WC <- fread("/Users/home/PycharmProjects/Portafolio/7. World Cup stats (2020)/WC_matches_final.csv")

names(WC)
#head(WC)

WC_nw <- WC %>% 
  select(`Home Team Name`, `Away Team Name`, Year)



WC.graph <- as_tbl_graph(WC_nw, directed = T)

WC.graph <- WC.graph %>% activate(nodes) %>%
  mutate(centrality = centrality_authority()) 

set.seed(1111)
figure <- ggraph(WC.graph, layout = 'mds') + 
  geom_edge_fan(aes(colour = as.factor(Year)))+
  geom_node_point(color = "black", size = 0.5) +
  geom_node_text(aes(label = ifelse(centrality > 0.000000000000000055, as.character(name), NA_character_)),size = 3.5)+
theme_graph() +
  theme(legend.title=element_blank())


ggsave("WC_NETWORK_DATA.png", plot =figure,
       width = 10, height = 10, 
       limitsize = F)

#####


#c('star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
#  'randomly', 'fr', 'kk', 'drl', 'lgl')

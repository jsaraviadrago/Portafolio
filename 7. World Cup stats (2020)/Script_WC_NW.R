# Network of teams World cup


library(data.table)
library(ggplot2)
library(igraph)
library(ggraph)
library(dplyr)
library(tidygraph)



WC <- fread("/Users/home/PycharmProjects/Portafolio/7. World Cup stats (2020)/WC_matches_final.csv")


WC_nw <- WC %>% 
  select(`Home Team Name`, `Away Team Name`, Year)



WC.graph <- as_tbl_graph(WC_nw, directed = T)

WC.graph <- WC.graph %>% activate(nodes) %>%
  mutate(betweenness = centrality_betweenness(),
         pagerank = centrality_pagerank()) 



set.seed(1111)
figure <- ggraph(WC.graph, layout = 'mds') + 
  geom_edge_fan(color = "grey")+
  geom_node_point(aes(colour = betweenness)) +
  geom_node_text(aes(label = dplyr::if_else(betweenness > 350, as.character(name), NA_character_)),
                 size = 2, repel = T)+
theme_graph() +
  theme(legend.title=element_blank())

ggraph(WC.graph, layout = 'kk') + 
  geom_edge_density() + 
  geom_edge_link(alpha = 0.25)

#ggsave("WC_NETWORK_DATA.png", plot =figure,
#       width = 10, height = 10, 
#       limitsize = F)

##### Making clusters ###

#WC.graph %>% 
#mutate(community = as.factor(group_infomap())) %>% 
#ggraph(layout = 'mds') + 
#geom_edge_link(aes(alpha = ..index..), show.legend = FALSE) + 
#geom_node_point(aes(colour = community), size = 3) + 
#theme_graph()


#c('star', 'circle', 'gem', 'dh', 'graphopt', 'grid', 'mds', 
#  'randomly', 'fr', 'kk', 'drl', 'lgl')

## this script create a figure comparing the HMSC derived spp covariance with the ecoCopula derived spp network 

#  ----------  read library ----------# 
library(ggraph)
library(igraph)
library(gridExtra)

# ---------- the function below is adapted from ecoCoupla package utils.R --------- #
## turn a correlation matrix into a graph object 
graph_from_cor <- function(cor){
  diag(cor)<-0
  vertex <- data.frame(name = colnames(cor))
  Edges=igraph::as_data_frame(igraph::graph_from_adjacency_matrix(cor,mode="undirected",weighted = TRUE))
  if(ncol(Edges)==3){
    colnames(Edges)[3]<- "cor"
  }
  igraph::graph_from_data_frame(Edges, directed=FALSE, vertices=vertex)
}

#  ----------  data prep ----------# 
raw_cor <- read_csv("./data/for-spp-relationship/mara-cooccurence-compiled.csv") %>% drop_na() %>% 
  filter(Yr_Mo != "2018-05",
         !is.na(Protein_lag1),
         !is.na(Height_lag1)) %>%   # the first month does not have previous month measurement
  dplyr::select(Cattle, Wildebeest, Zebra, Thompsons_Gazelle, Impala, Topi,
                Eland, Buffalo, Grants_Gazelle, Waterbuck, Dikdik, Elephant) %>%
  as.matrix(.) %>% # 1,455 x 35. Some months/sites missing values. Dropped. 
  cor(.) %>% graph_from_cor(.)

copula_igraph_partcor <- readRDS("./results/ecoCoupla_partcor_network.RDS") %>% graph_from_cor(.)

hmsc_igraph_hi <- readRDS("./results/Hmsc_network_95.RDS") %>% graph_from_cor(.)  # posterior probability of the correlation estimate to be true is greater than 95%
#hmsc_igraph_hlo <- readRDS("./results/Hmsc_network_50.RDS") # posterior probability of the correlation estimate to be true is greater than 50%

# get node cooordinates so the two graphs will have the same node loactions
Coords <- layout_with_kk(copula_igraph_partcor) %>% 
  as_tibble %>%
  bind_cols(tibble(names = names(V(copula_igraph_partcor))))

a <- raw_cor %>%  
  ggraph(layout = as.matrix(Coords[,1:2])) + # see ?layout_tbl_graph_igraph
  geom_edge_fan0(aes( colour = cor, width= cor, alpha = 0.8)) +
  scale_edge_width(range = c(1, 4))+
  scale_edge_color_gradient2(low="#b2182b",mid="white",high="#2166ac")+
  geom_node_label(aes(label=name), repel = TRUE, nudge_y = 0)+
  geom_node_point(aes( size= 2))+
  theme_void() +
  labs(subtitle = "Raw correlation") +
  theme(
    legend.position = 'none',
    plot.title = element_text(color="black", size=14, face="bold.italic", vjust = 5, hjust = 0.5 ),
    plot.margin = margin(1, 1.5, 1, 1, "cm")
  )

b <- hmsc_igraph_hi %>%  
  ggraph(layout = as.matrix(Coords[,1:2])) + # see ?layout_tbl_graph_igraph
  geom_edge_fan0(aes( colour = cor, width= cor, alpha = 0.8)) +
  scale_edge_width(range = c(1, 4))+
  scale_edge_color_gradient2(low="#b2182b",mid="white",high="#2166ac")+
  geom_node_label(aes(label=name), repel = TRUE, nudge_y = 0)+
  geom_node_point(aes( size= 2))+
  theme_void() +
  labs(subtitle = "Hierarchical Modeling of Species Communities") +
  theme(
    legend.position = 'none',
    plot.title = element_text(color="black", size=14, face="bold.italic", vjust = 5, hjust = 0.5 ),
    plot.margin = margin(1, 1, 1, 1.5, "cm")
  )

c <- copula_igraph_partcor %>%  
  ggraph(layout = as.matrix(Coords[,1:2])) + # see ?layout_tbl_graph_igraph
  geom_edge_fan0(aes( colour = cor, width= cor, alpha = 0.8)) +
  scale_edge_width(range = c(1, 4))+
  scale_edge_color_gradient2(low="#b2182b",mid="white",high="#2166ac")+
  geom_node_label(aes(label=name), repel = TRUE, nudge_y = 0)+
  geom_node_point(aes( size= 2))+
  theme_void() +
  labs(subtitle = "Gaussian copula graphical models") +
  theme(
    legend.position = 'none',
    plot.title = element_text(color="black", size=14, face="bold.italic", vjust = 5, hjust = 0.5 ),
    plot.margin = margin(1, 1.5, 1, 1, "cm")
  )


mara.graph <- grid.arrange(a, b, c, nrow = 1)

ggsave("./figures/materials/mara_graph.png", mara.graph ,
       width = 18, height = 6, device = ragg::agg_png)

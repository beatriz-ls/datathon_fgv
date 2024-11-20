# libs

library(tidymodels)
library(ggplot2)
library(broom)
library(dplyr)
library(janitor)
library(cluster)
library(factoextra)

# load data

dt <- readRDS("data.rds")

# data pre processing

dt <- dt %>%
  clean_names() %>%
  mutate(across(c(open, high, low, close, volume), scale)) %>%
  select(open, high, low, close, volume, name)

######## clusterização ---------------------------------------------------------

dt_cluster <- dt %>% select(-name)

# elbow graphic analisys

set.seed(123)
wss <- map_dbl(1:10, function(k) {
  kmeans(dt_cluster, centers = k, nstart = 10)$tot.withinss
})

df_wss <- data.frame(k = 1:10, wss = wss)

elbow_graph <- ggplot(df_wss, aes(x = k, y = wss)) +
                  geom_point(shape = 19, size = 3, color = "blue") +
                  geom_line(color = "blue", linewidth = 1) +
                  labs(
                    x = "Número de Clusters (k)",
                    y = "Soma dos Quadrados Intra-Cluster (WSS)",
                    title = "Método do Cotovelo"
                  ) +
                  theme_minimal() +
                  theme(
                    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
                    axis.title = element_text(size = 12)
                  ) # obs: 4 clusters?

#ggsave("grafico_cotovelo.png", plot = elbow_graph,
#       width = 7, height = 5, dpi = 300)










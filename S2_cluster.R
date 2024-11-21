# libs

library(tidymodels)
library(ggplot2)
library(broom)
library(dplyr)
library(janitor)
library(cluster)
library(factoextra)
library(dtwclust)

# load data

dt <- readRDS("data.rds")

# data pre processing

dt <- dt %>% select(-symbol) %>%
  mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S")) %>%
  clean_names()

dt_scale <- dt %>%
  clean_names() %>%
  mutate(across(c(open, high, low, close, volume), scale)) %>%
  select(open, high, low, close, volume, name)

######## clusterização ---------------------------------------------------------

dt_cluster <- dt_scale %>% select(-name)

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


######## clusterização para series temporais -----------------------------------

dt_cluster <- dt %>%
  select(date, close, name) %>%
  arrange(name, date)

# Criar séries temporais por moeda
cluster_list <- dt_cluster %>%
  group_by(name) %>%
  summarise(
    series = list(close),
    .groups = "drop"
  )

# por praticidade computacional amostrar 48 moedas
cluster_list <- cluster_list %>%
  slice_sample(n = 48)

# todas as séries tem que ter o mesmo comprimento
max_length <- max(sapply(cluster_list$series, length))
cluster_list$series <- lapply(cluster_list$series, function(series) {
  if (length(series) < max_length) {
    c(series, rep(NA, max_length - length(series)))
  } else {
    series
  }
})

# trocando valores ausentes (NA) por zero ou interpolação
cluster_list$series <- lapply(cluster_list$series, function(series) {
  zoo::na.approx(series, na.rm = FALSE, rule = 2)
})

# normalizando os dados
cluster_list$series <- lapply(cluster_list$series, function(series) {
  scale(series, center = TRUE, scale = TRUE)
})

set.seed(123)

# Número de clusters
num_clusters <- 4 

# Aplicar o k-means com DTW
kmeans_result <- tsclust(
  cluster_list$series,
  type = "partitional",
  k = num_clusters,
  preproc = NULL,
  distance = "dtw_basic",
  centroid = "dba"
)






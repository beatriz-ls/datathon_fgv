# libs

library(dplyr)
library(dtwclust)
library(tidyr)
library(zoo)
library(tidymodels)
library(dtw)
library(plotly)

# load data

dt <- readRDS("short_data.rds")

# data preprocessing

dt_wide <- dt %>%
  select(date, ticker, close) %>%
  pivot_wider(
    names_from = ticker,
    values_from = close 
  )

# analise de dados faltantes

any(is.na(dt_wide))

sum(is.na(dt_wide))

colSums(is.na(dt_wide))

# Criar todas as combinações possíveis de `date` e `ticker`
comb_full <- expand.grid(
  date = unique(dt$date),
  ticker = unique(dt$ticker)
)

comb_missing <- anti_join(comb_full, dt, by = c("date", "ticker"))
print(comb_missing)

# substituindo dados faltantes com aproximandos

dt_wide <- dt_wide %>%
  arrange(date) %>%
  fill(everything(), .direction = "down") 

# normalizando os dados

dt_norm <- scale(dt_wide[,-1])

#### definindo números de k para clusterização

set.seed(123)

# elbow graphic analisys

wss <- map_dbl(1:10, function(k) {
  kmeans(dt_norm, centers = k, nstart = 10)$tot.withinss
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


# índice de silhueta

silhouette_scores <- sapply(2:10, function(k) {
  kmeans_result <- kmeans(dt_norm, centers = k, nstart = 25)
  silhouette_score <- silhouette(kmeans_result$cluster, dist(dt_norm))
  mean(silhouette_score[, 3])  # Média do índice de silhueta
})

plot(2:10, silhouette_scores, type = "b", pch = 19, frame = FALSE,
     xlab = "Número de Clusters", ylab = "Índice de Silhueta Médio",
     main = "Escolha do Número de Clusters - Índice de Silhueta")

# método hierarquico

dt_wide_aux <- dt_wide %>%
  select(-date)

# Calcular a matriz de distância Euclidiana entre as moedas
dist_matrix <- dist(t(dt_wide_aux), method = "DTW")  

# Realizar a clusterização hierárquica diretamente com o método "dtw"
hc_dtw <- hclust(dist_matrix)

# Visualizar o dendrograma
plot(hc_dtw, main = "Dendrograma - Clusterização Hierárquica com DTW", 
     xlab = "Moedas", ylab = "Distância DTW")

# Converter o dendrograma para um formato de plotly

num_clusters <- 5 

# aplicar o k-means com DTW
kmeans_result <- tsclust(
  t(dt_norm),
  type = "partitional",
  k = num_clusters,
  preproc = NULL,
  distance = "dtw_basic",
  centroid = "dba"
)


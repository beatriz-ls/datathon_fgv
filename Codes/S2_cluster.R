# libs

library(dplyr)
library(dtwclust)
library(tidyr)
library(zoo)
library(tidymodels)
library(dtw)
library(plotly)

# load data

dt <- readRDS("Codes/short_data.rds")

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


##### aplicação da clusterização

num_clusters <- 5


# aplicar o k-means com DTW

set.seed(123)

partional_clust <- tsclust(
                  t(dt_norm),
                  type = "partitional",
                  k = num_clusters,
                  preproc = NULL,
                  distance = "dtw_basic",
                  centroid = "dba"
                )


# Testando multiplos modelos 

## Parâmetros
num_clusters <- 5
n_runs <- 10
min_cluster_size <- 8

# Armazenar resultados
best_model <- NULL
best_distance <- Inf  # Inicialmente, a distância é infinita

# Loop para testar múltiplas inicializações
for (i in 1:n_runs) {
  cat("Rodando modelo", i, "...\n")
  
  # Executa o modelo sem set.seed() para inicializações variadas
  current_model <- tsclust(
    t(dt_norm),
    type = "partitional",
    k = num_clusters,
    preproc = NULL,
    distance = "dtw_basic",
    centroid = "dba"
  )
  
  # Verifica se todos os clusters têm o tamanho mínimo
  cluster_sizes <- current_model@clusinfo$size
  if (all(cluster_sizes >= min_cluster_size)) {
    # Calcula a média das distâncias intra-cluster
    current_distance <- mean(current_model@clusinfo$av_dist)
    
    # Verifica se é o melhor modelo até agora
    if (current_distance < best_distance) {
      best_model <- current_model
      best_distance <- current_distance
    }
  } else {
    cat("Modelo descartado devido a clusters com tamanho menor que", min_cluster_size, "\n")
  }
}

# Resultado final
if (!is.null(best_model)) {
  cat("Melhor modelo encontrado com média de distância intra-cluster:", best_distance, "\n")
  print(best_model)
} else {
  cat("Nenhum modelo atendeu ao critério de tamanho mínimo por cluster.\n")
}

saveRDS(best_model, "Codes/cluster_selecionado.rds")

# associando cluster para cada moeda

moeda_cluster <- data.frame(
  ticker = colnames(dt_norm),
  cluster = best_model@cluster
)

g_moeda_cluster <- ggplot(moeda_cluster, aes(x = factor(cluster), y = ticker)) +
  geom_text(
    aes(label = ticker, color = factor(cluster)), 
    position = position_dodge(width = 0.9),
    size = 1
  ) +
  labs(
    title = "Moedas por Cluster",
    x = "Cluster",
    y = "Moedas",
    color = "CLUSTER"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    legend.title = element_text(size = 4), # Ajusta o tamanho do título da legenda
    legend.text = element_text(size = 4)   # Ajusta o tamanho do texto na legenda
        )

ggsave("graficos/g_moeda_cluster.png", plot = g_moeda_cluster)

# gráficos de clusters

dt_aux <- dt %>% 
  select(-c("high", "low", "open")) %>%
  left_join(moeda_cluster, by = "ticker") %>%
  mutate(close_norm = scale(close),
         close_minimax = scale((close - min(close)) / (max(close) - min(close)) * 2 - 1))

# Normalização min-max
dt_aux$normalized_close <- (dt_aux$close - min(dt_aux$close)) / (max(dt_aux$close) - min(dt_aux$close)) * 2 - 1

#saveRDS(dt_aux, "clustered_data.rds")

ggplot(dt_aux, aes(x = date, y = close_norm, group = ticker,
                   color = as.factor(cluster))) +
  geom_line() +
  facet_wrap(~ cluster, scales = "free_y", ncol = 1) +
  theme_minimal() +
  labs(title = "Séries Temporais por Cluster",
       x = "Tempo",
       y = "Valor",
       color = "Cluster")


for (cl in unique(dt_aux$cluster)) {
  p <- ggplot(subset(dt_aux, cluster == cl), aes(x = date, y = close_norm)) +
    geom_line() +
    labs(title = paste("Cluster", cl),
         x = "Tempo",
         y = "Valor Normalizado") +
    theme_minimal()
 print(p) 
}


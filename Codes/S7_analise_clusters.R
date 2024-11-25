#libs

library(ggplot2)
library(dplyr)
library(corrplot)

#Puxar dados
clustered_data <- readRDS("Codes/clustered_data.rds")
best_model <- readRDS("Codes/cluster_selecionado.rds")



#--------- quantas moedas tem em cada clsuter?

cluster_sizes <- best_model@clusinfo$size
print(cluster_sizes)

#--------- qual o tamanho e distancia media intra-cluster?
cluster_distances <- best_model@clusinfo$av_dist
print(cluster_distances)

# Visualização
ggplot(data.frame(cluster = 1:length(cluster_distances), avg_distance = cluster_distances), 
       aes(x = factor(cluster), y = avg_distance, fill = cluster)) +
  geom_bar(stat = "identity") +
  labs(title = "Distância Média Intra-Cluster",
       x = "Cluster",
       y = "Distância Média") +
  theme_minimal()



# ----------- Visualização das Séries Temporais por Cluster

# Associar clusters às séries temporais
dt_aux <- dt %>% 
  select(-c("high", "low", "open")) %>%
  left_join(moeda_cluster, by = "ticker") %>%
  mutate(close_norm = scale(close))

# Plotar séries temporais por cluster
ggplot(dt_aux, aes(x = date, y = close_norm, group = ticker, color = as.factor(cluster))) +
  geom_line(alpha = 0.5) +
  facet_wrap(~ cluster, scales = "free_y") +
  labs(title = "Séries Temporais por Cluster",
       x = "Data",
       y = "Fechamento Normalizado",
       color = "Cluster") +
  theme_minimal()


# ----VOLTAR----- informações estatísticas de cada cluster

estat_cluster <- dt_aux %>%
  group_by(cluster) %>%
  summarise(
    mean_close = mean(close, na.rm = TRUE),
    sd_close = sd(close, na.rm = TRUE),
    min_close = min(close, na.rm = TRUE),
    max_close = max(close, na.rm = TRUE)
  )

print(estat_cluster)

# Visualização com Boxplot
#com outlier
ggplot(dt_aux, aes(x = factor(cluster), y = close, fill = factor(cluster))) +
  geom_boxplot() +
  labs(title = "Distribuição dos Valores de Fechamento por Cluster",
       x = "Cluster",
       y = "Fechamento",
       fill = "Cluster") +
  theme_minimal()

#sem outlier
ggplot(dt_aux, aes(x = factor(cluster), y = close, fill = factor(cluster))) +
  geom_boxplot(outlier.shape = NA) +
  coord_cartesian(ylim = c(-50, 50)) +  # Ajuste os limites do eixo y
  labs(title = "Distribuição dos Valores de Fechamento por Cluster",
       x = "Cluster",
       y = "Fechamento",
       fill = "Cluster") +
  theme_minimal()






# --------- Identificar Outliers por Cluster

iqr_outliers <- clustered_data %>%
  group_by(cluster) %>%
  summarise(
    Q1 = quantile(close, 0.25, na.rm = TRUE),
    Q3 = quantile(close, 0.75, na.rm = TRUE),
    IQR = Q3 - Q1
  ) %>%
  left_join(clustered_data, by = "cluster") %>%
  filter(close < (Q1 - 1.5 * IQR) | close > (Q3 + 1.5 * IQR))

print(iqr_outliers)

ggplot(iqr_outliers, aes(x = factor(cluster), y = close, color = factor(cluster))) +
  geom_point() +
  labs(title = "Outliers por Cluster",
       x = "Cluster",
       y = "Fechamento",
       color = "Cluster") +
  theme_minimal()


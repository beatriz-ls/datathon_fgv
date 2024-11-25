# library(party)
# 
# dt3 <- readRDS("Codes/clustered_data.rds")
# 
# dt3 <- dt3 %>% 
#   mutate(cluster = as.factor(cluster))
# 
# set.seed(1234)
# conglomerados_selecionados <- sample(unique(dt3$ticker), 109*0.7)
# conglomerados_selecionados
# 
# # Filtrar os dados dos conglomerados escolhidos
# train.data <- subset(dt3, ticker %in% conglomerados_selecionados)
# test.data <- subset(dt3, !(ticker %in% conglomerados_selecionados))
# 
# myf <- cluster ~ close
# 
# crypto_ctree <- ctree(myf, data = train.data)
# 
# table(predict(crypto_ctree), train.data$cluster)
# 
# # plot(crypto_ctree)
# 
# testpred <- predict(crypto_ctree, new_data=teste.data)
# table(testpred)
# 
# testpred
# 


# outro método ------------------------------------------------------------
library(party)

dt3 <- readRDS("Codes/clustered_data.rds")

dt3 <- dt3 %>% 
  mutate(cluster = as.factor(cluster))


# transform ---------------------------------------------------------------

df <- dt3 %>% 
  as.data.frame() %>% 
  select(date, close, ticker) %>%
  mutate(date = format(date, "%Y%-%m%-%d"))

tabela <- tidyr::pivot_wider(
  data = df,
  names_from = ticker,        # Colunas serão os nomes
  values_from = close, # Valores serão os fechamentos
  values_fill = NA          # Preencher valores ausentes com NA
)

tabela_preenchida <- tabela %>%
  tidyr::fill(everything(), .direction = "downup") # P


# sample ------------------------------------------------------------------

set.seed(1234)
conglomerados_selecionados <- sample(colnames(tabela_preenchida)[-1], replace = FALSE, 109*0.7)
conglomerados_selecionados

train.data <- tabela_preenchida %>% 
  as.data.frame() %>% 
  select(date, conglomerados_selecionados)

test.data <- tabela_preenchida %>% 
  as.data.frame() %>% 
  select( !(conglomerados_selecionados))


# modelo ------------------------------------------------------------------

# normalização ------------------------------------------------------------

df_moedas <- train.data 
moedas_norm <- scale(df_moedas[,-1])
moedas_norm %>% head() 

moedas_norm

moedas_norm %>% 
  as.data.frame() %>% 
  select(colnames(.)[1:3]) %>% 
  # transformar data frame em formato tidy (long)
  mutate(ind = row_number()) %>% 
  gather(moeda, cotacao_norm, -ind) %>%  
  ggplot(aes(x = ind, y = cotacao_norm)) + 
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ moeda, scale ="free_y")


modelo <- tsclust(t(moedas_norm), 
                  k = 5, 
                  distance = "dtw_lb",
                  centroid = "pam", 
                  seed = 123,
                  trace = TRUE,
                  control = partitional_control(pam.precompute = FALSE),
                  args = tsclust_args(dist = list(window.size = 20L)))

modelo

plot(modelo)


# predição ----------------------------------------------------------------

new_data <- test.data


str(modelo@centroids)


# individual --------------------------------------------------------------

# # Nova série a ser atribuída a um cluster
# new_series <- rnorm(50)
# 
# # Distâncias DTW da nova série para cada centróide
# distances <- sapply(1:length(modelo@centroids), function(i) {
#   dtw_res <- dtw(new_series, modelo@centroids[[i]])  # Usar a função dtw
#   return(dtw_res$distance)
# })
# 
# # Atribuir a nova série ao cluster mais próximo
# predicted_cluster <- which.min(distances)
# print(predicted_cluster)


# varias series -----------------------------------------------------------

new_data <- test.data %>% as.data.frame() %>% 
  select(-date, ADA,AMP,AR)

# Função para prever o cluster para uma nova série
predict_cluster <- function(new_series, centroids) {
  # Calcular as distâncias DTW para cada centróide
  distances <- sapply(1:length(centroids), function(i) {
    dtw_res <- dtw(new_series, centroids[[i]])  # Usar a função dtw
    return(dtw_res$distance)
  })
  
  # Retornar o índice do cluster mais próximo
  return(which.min(distances))
}

# Aplicando a função para cada linha (ou série) do data frame de novas séries
predicted_clusters <- apply(new_data, 2, function(row) {
  predict_cluster(as.numeric(row), modelo@centroids)
})


# Exibir os clusters previstos para cada nova série
print(predicted_clusters)

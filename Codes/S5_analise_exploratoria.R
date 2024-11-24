#libs

library(dplyr)
library(readr)
library(lubridate)  
library(ggplot2)
library(writexl)

# ler base de dados
dt <- readRDS("short_data.rds")

#-------- obter o número de observações por moeda no decorrer dos anos

#mudar formato da data
dt <- dt %>%
  mutate(date = as.Date(date, format = "%Y-%m-%d"))

#extrair ano da data
dt <- dt %>%
  mutate(ano = year(date))

#Agrupar por 'ticker' e 'ano' e Contar as Observações
num_observacoes <- dt %>%
  group_by(ticker, ano) %>%
  summarise(contagem = n()) %>%
  arrange(ticker, ano)

# Visualizar a tabela resultante
print(num_observacoes)

# Gráfico de series mostrando a contagem de observações por ano para cada ticker
series_moedas_ano <- ggplot(num_observacoes, aes(x = ano, y = contagem, color = ticker, group = ticker)) +
  geom_line(size = 1) +  # Adiciona as linhas
  geom_point(size = 2) + # Adiciona pontos para marcar os valores
  labs(
    title = "Contagem de Observações por Moeda ao Longo dos Anos",
    x = "Ano",
    y = "Número de Observações",
    color = "Ticker"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 9),
    axis.title = element_text(size = 6),
    axis.text.x = element_text(size = 5), 
    axis.text.y = element_text(size = 5),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 4)
  ) +
  theme(legend.position = "none")

ggsave("graficos/series_moedas_ano.png", plot = series_moedas_ano)


#-------- obter tabela com a media das variaveis high, low, open e close + variancia

# Criar a tabela de médias para cada moeda
tabela_medias <- dt %>%
  group_by(ticker) %>% # Agrupar por moeda
  summarise(
    media_high = round(mean(high, na.rm = TRUE), 4), # Média de 'high', ignorando valores NA
    mediana_high = round(median(high, na.rm = TRUE), 4),
    variancia_high = round(var(high, na.rm = TRUE), 2),
    media_low = round(mean(low, na.rm = TRUE), 4), # Média de 'low'
    mediana_low = round(median(low, na.rm = TRUE), 4),
    media_open = round(mean(open, na.rm = TRUE), 4), # Média de 'open'
    mediana_open = round(median(open, na.rm = TRUE), 4),
    media_close = round(mean(close, na.rm = TRUE), 4), # Média de 'close'
    mediana_close = round(median(close, na.rm = TRUE), 4),
    variancia_close = round(var(close, na.rm = TRUE), 2)         # Variância de 'close'
  )

# Visualizar a tabela resultante
print(tabela_medias)


write_xlsx(tabela_medias, "tabela_medias.xlsx")

#-------------- grafico de series com as 3 criptos com maiores medias de preço e as 3 com menor media de preço

# 1. Calcular a média de preço (close) para cada criptomoeda
medias_precos <- dt %>%
  group_by(ticker) %>%
  summarise(media_preco = mean(close, na.rm = TRUE)) %>%
  arrange(desc(media_preco))

# 2. Selecionar as 3 criptomoedas com maior e menor média de preço usando slice_max e slice_min
top3_mais_caro <- medias_precos %>% slice_max(order_by = media_preco, n = 3, with_ties = FALSE)
top3_mais_barato <- medias_precos %>% slice_min(order_by = media_preco, n = 3, with_ties = FALSE)

# Combinar as 3 mais caras e as 3 mais baratas
selecao_tickers <- bind_rows(top3_mais_caro, top3_mais_barato)

# Verificar se há 6 tickers únicos
selecao_tickers <- selecao_tickers %>% distinct(ticker, .keep_all = TRUE)

# 3. Filtrar os dados para incluir apenas as criptos selecionadas
dt_caro_barato <- dt %>%
  filter(ticker %in% selecao_tickers$ticker)

# 4. Plotar o gráfico de séries temporais
series_caro_barato <- ggplot(dt_caro_barato, aes(x = date, y = close, color = ticker, group = ticker)) +
  geom_line(size = 1) +
  labs(
    title = "Preços das 3 Criptomoedas Mais Caras e Mais Baratas ao Longo dos Anos",
    x = "Ano",
    y = "Preço de Fechamento",
    color = "Ticker"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 9),
    axis.title = element_text(size = 6),
    axis.text.x = element_text(size = 5), 
    axis.text.y = element_text(size = 5),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 4)
  )


ggsave("graficos/series_caro_barato.png", plot = series_caro_barato)


#--------- calcular preço medio por ano de fechamento


# Calcular o preço médio por ano
preco_anual <- dt %>%
  group_by(ticker, ano) %>%
  summarise(media_close = mean(close, na.rm = TRUE))

# Gráfico de tendências
series_preço_ano_moeda <- ggplot(preco_anual, aes(x = ano, y = media_close, color = ticker, group = ticker)) +
  geom_line(size = 1) +
  labs(
    title = "Tendências do Preço Médio de Fechamento por Moeda",
    x = "Ano",
    y = "Preço Médio de Fechamento",
    color = "Ticker"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 9),
    axis.title = element_text(size = 6),
    axis.text.x = element_text(size = 5), 
    axis.text.y = element_text(size = 5),
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 4),
    legend.spacing.y = unit(0.005, "cm")
  ) +
  theme(legend.position = "none") 

ggsave("graficos/series_preço_ano_moeda.png", plot = series_preço_ano_moeda)




#libs

library(dplyr)
library(readr)
library(lubridate)  
library(ggplot2)

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




#-------------- grafico de series com as 3 criptos com maiores medias de preço e as 3 com menor media de preço

# 1. Calcular a média de preço (close) para cada criptomoeda
medias_precos <- dt %>%
  group_by(ticker) %>%
  summarise(media_preco = mean(close, na.rm = TRUE)) %>%
  arrange(desc(media_preco))

# 2. Selecionar as 3 criptomoedas com maior e menor média de preço
top3_mais_caro <- medias_precos %>% top_n(3, media_preco)
top3_mais_barato <- medias_precos %>% top_n(-3, media_preco)

# Combinar as 3 mais caras e as 3 mais baratas
selecao_tickers <- bind_rows(top3_mais_caro, top3_mais_barato)

# 3. Filtrar os dados para incluir apenas as criptos selecionadas
dt_caro_barato <- dt %>%
  filter(ticker %in% selecao_tickers$ticker)

# 4. Plotar o gráfico de séries temporais
ggplot(dt_filtrado, aes(x = date, y = close, color = ticker, group = ticker)) +
  geom_line(size = 1) +
  labs(
    title = "Preços das 3 Criptomoedas Mais Caras e Mais Baratas ao Longo dos Anos",
    x = "Ano",
    y = "Preço de Fechamento",
    color = "Ticker"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )



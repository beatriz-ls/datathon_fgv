# libs

library(data.table)

# import datasets

archives <- list.files("data", full.names = TRUE)

dt <- rbindlist(lapply(archives, fread))

saveRDS(dt, "data.rds")


# vitória sesana ----------------------------------------------------------

# compactação por dia -----------------------------------------------------

library(dplyr)

dt <- readRDS("data.rds")

dt_dia <- dt %>% 
  mutate(data = as.Date(date)) %>%
  group_by(name, data) %>%
  slice_max(date) %>% 
  mutate(ano_mes = format(date, "%Y-%m"))

dt_dia$ano_mes %>% table()

# exploratória 

dt$ticker %>% unique() %>% length() # 109 cripto moedas
 
dt$ticker %>% table %>% min

# geral

dt$date %>% max
dt$date %>% min()

# plot de informações -----------------------------------------------------

dt_bicoin <- dt %>% filter(ticker == "BTC") # bitcoin

dt %>% as.data.frame() %>% mutate(date = as.Date(date))

library(ggplot2)
library(tseries)
library(lubridate)

plot(dt_bicoin$close, main = "Série Temporal", ylab = "Valores", xlab = "Tempo", col = "blue", lwd = 2)

ts(dt_bicoin$High)

# # x -----------------------------------------------------------------------
# 
# dados <- data.frame(
#   datetime = as.POSIXct(c(
#     "2024-11-22 15:30:00", "2024-11-10 12:00:00",
#     "2024-10-05 14:00:00", "2023-11-22 09:00:00",
#     "2023-11-20 18:00:00", "2023-10-15 08:00:00"
#   ))
# )
# 

# Contar elementos por ano/mês
 quantidade <- dt %>%
   mutate(ano = format(date, "%Y"))
   group_by(ano) %>%
   summarise(quantidade = n())
 
 # Plotar
 ggplot(quantidade, aes(x = ano, y = quantidade)) +
   geom_col(fill = "skyblue") +
   labs(x = "Ano/Mês", y = "Quantidade", title = "Quantidade por Ano/Mês") +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1))

 
 # analisando séries temporais mais recentes
 
 dt$date <- as.Date(dt$date, format = "%Y-%m-%d")
 
 # Encontrar a data mais antiga (primeiro registro) para cada moeda
 datas_mais_antigas <- dt %>%
   group_by(ticker) %>%
   summarise(data_mais_antiga = min(date, na.rm = TRUE)) %>%
   arrange(desc(data_mais_antiga))
 
 head(datas_mais_antigas)
 
 # criando dataset a partir da data mais antiga da moeda mais recente
 
 data_corte <- dt %>%
   group_by(ticker) %>%
   summarise(data_mais_antiga = min(date, na.rm = TRUE)) %>%
   summarise(data_corte = max(data_mais_antiga)) %>% 
   pull(data_corte)
 
 data_corte <- as.Date(data_corte, format = "%Y-%m-%d")
 
 dt_filter <- dt %>%
   filter(date >= data_corte)
 
 saveRDS(dt_filter, "short_data.rds")
 
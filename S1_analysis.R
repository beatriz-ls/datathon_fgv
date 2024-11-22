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

saveRDS(dt_dia, "sample_data.rds")

# exploratória 

# dt$name %>% unique() %>% length() # 109 cripto moedas
# 
# dt$name %>% table %>% min# nem todas possuem coleta para cada hora do dia
# 
# # geral
# dt$date %>% max
# dt$date %>% min()
# 
# 
# # plot de informações -----------------------------------------------------
# 
# dt_bicoin <- dt %>% filter(name == "Bitcoin") # bitcoin
# 
# dt %>% as.data.frame() %>% mutate(date = as.Date(date))
# 
# library(ggplot2)
# library(tseries)
# library(lubridate)
# 
# plot(dt_bicoin$High, main = "Série Temporal", ylab = "Valores", xlab = "Tempo", col = "blue", lwd = 2)
# 
# ts(dt_bicoin$High)

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
# # Contar elementos por ano/mês
# quantidade <- dt_dia %>%
#   mutate(ano = format(date, "%Y"))
#   group_by(ano) %>%
#   summarise(quantidade = n())
# 
# # Plotar
# ggplot(quantidade, aes(x = ano, y = quantidade)) +
#   geom_col(fill = "skyblue") +
#   labs(x = "Ano/Mês", y = "Quantidade", title = "Quantidade por Ano/Mês") +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

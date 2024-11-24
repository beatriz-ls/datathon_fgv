# bases 

# dt1 <- readRDS("Codes/short_data.rds")
# dt2 <- readRDS("Codes/sample_data.rds")
# dt3 <- readRDS("Codes/clustered_data.rds")
# dt4 <- readRDS("Codes/cluster_selecionado.rds")


library(dplyr)

dt <- readRDS("data.rds")

dt_dia <- dt %>% 
  mutate(data = as.Date(date)) %>%
  group_by(name, data) %>%
  slice_max(date) %>% 
  mutate(ano_mes = format(date, "%Y-%m"))

dt_dia$ano_mes %>% table()

# análise exploratória 

# divisão por k-means
data_col <- dt_dia %>% 
  as.data.frame() %>% 
  select(Open,High,Low,Close,Volume) 


data_corr <- dt_dia %>% 
  as.data.frame() %>% 
  select(Open,High,Low,Close,Volume) %>% 
  cor()

# tirando o volume, todas as variáveis são correlacionadas

corrplot::corrplot(data_corr, 
                   method="circle")

require(psych)
psych::cortest.bartlett(data_col)

KMO(data_col)

# análise fatorial é apropriada


# pca ---------------------------------------------------------------------

fit <- princomp(data_col,cor=TRUE)
fit
summary(fit)

screeplot(fit)

PCA<-principal(data_col, nfactors=2,
                           n.obs=30,rotate="varimax",scores=TRUE)
PCA

PCA$loadings

biplot(PCA)

# fator um: valor da moeda, fator 2: volume da moeda


factor.scores(data_col,PCA, 
              Phi = NULL, 
              method = c("Thurstone", "tenBerge", "Anderson",
                         "Bartlett", "Harman","components"),
              rho=NULL)


# utilizar apenas volume e valor de fechamento ----------------------------

library(tidyverse)
library(dtwclust) 



moedas_norm <- scale(dt_dia[,-1])
moedas_norm %>% head() %>% knitr::kable()

library(party)

dt3 <- readRDS("Codes/clustered_data.rds")

dt3 <- dt3 %>% 
  mutate(cluster = as.factor(cluster))

set.seed(1234)
conglomerados_selecionados <- sample(unique(dt3$ticker), 109*0.7)
conglomerados_selecionados

# Filtrar os dados dos conglomerados escolhidos
train.data <- subset(dt3, ticker %in% conglomerados_selecionados)
test.data <- subset(dt3, !(ticker %in% conglomerados_selecionados))

myf <- cluster ~ close

crypto_ctree <- ctree(myf, data = train.data)

table(predict(crypto_ctree), train.data$cluster)

# plot(crypto_ctree)

testpred <- predict(crypto_ctree, new_data=teste.data)
table(testpred)

testpred

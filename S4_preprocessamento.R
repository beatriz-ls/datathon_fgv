# libs

library(tidymodels)
library(janitor)
library(zoo)

# Carregar os dados
dt <- readRDS("data.rds")

# Pré-processamento inicial
df <- dt %>%
  select(-symbol) %>%
  mutate(
    date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S"),
    name = as.factor(name)
  ) %>%
  clean_names() %>%
  select(date, open, high, low, close, volume, name)

# Definir o Recipe
rec <- recipe(~ ., data = df) %>%
  # Selecionar variáveis
  update_role(name, new_role = "identifier") %>%
  
  # Tratar datas
  step_mutate(date = as.Date(date)) %>%
  
  # Lidar com valores ausentes (interpolação)
  step_impute_linear(close, open, high, low, volume) %>%
  
  # Escalar as variáveis numéricas
  step_normalize(open, high, low, close, volume) %>%
  
  # Criar variáveis dummy para variáveis categóricas
  step_dummy(all_nominal_predictors(), -all_outcomes())

# Treinar o Recipe
rec_trained <- rec %>%
  prep(training = df, retain = TRUE)

# Salvar o Recipe treinado
saveRDS(rec_trained, "recipe_trained.rds")

# Função para aplicar o Recipe a novos dados
process_new_data <- function(new_data_path) {
  # Carregar o recipe treinado
  rec_trained <- readRDS("recipe_trained.rds")
  
  # Importar novos dados
  new_data <- fread(new_data_path) %>%
    select(-symbol) %>%
    mutate(
      date = as.POSIXct(date, format = "%Y-%m-%d %H:%M:%S"),
      name = as.factor(name)
    ) %>%
    clean_names() %>%
    select(date, open, high, low, close, volume, name)
  
  # Aplicar o Recipe
  new_data_processed <- rec_trained %>%
    bake(new_data)
  
  return(new_data_processed)
}

# Exemplo de uso:
# new_data_processed <- process_new_data("path_to_new_data.csv")
# saveRDS(new_data_processed, "new_data_processed.rds")

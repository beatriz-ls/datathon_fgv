# libs

library(data.table)

# import datasets

archives <- list.files("data", full.names = TRUE)

dt <- rbindlist(lapply(archives, fread))

saveRDS(dt, "data.rds")


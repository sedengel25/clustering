library(tidyverse)
library(here)

path.base <- here("data", "raw", "worms")

filename <- "worms_noise32_d2_cl21_1"
file.coord <- here(path.base, paste0(filename, ".txt"))
file.label <- here(path.base, paste0(filename, "-labels.txt"))
tibble.data <- read_delim(file.coord, delim = " ", col_names = FALSE)
labels <- read_delim(file.label, delim = " ", col_names = FALSE)[,1] %>% pull(X1)




path.dir.processed.data <- here("data", "processed", "worms")
if(!dir.exists(path.dir.processed.data)){
  dir.create(path.dir.processed.data)
}

list.data <- list(Data = tibble.data, 
                  Cls = as.integer(labels))
write_rds(x = list.data, file = here(path.dir.processed.data, paste0(filename, ".rds")))

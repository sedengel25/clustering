library(tidyverse)
library(here)

path.base <- here("data", "raw", "worms")

file.coord <- here(path.base, "worms_2d.txt")
file.label <- here(path.base, "worms_2d-gt.pa")
tibble.data <- read_delim(file.coord, delim = " ", col_names = FALSE)

char.labels <- read_lines(file = file.label, skip = 4)



path.dir.processed.data <- here("data", "processed", "worms")
if(!dir.exists(path.dir.processed.data)){
  dir.create(path.dir.processed.data)
}

list.data <- list(Data = tibble.data, 
                  Cls = as.integer(char.labels))
write_rds(x = list.data, file = here(path.dir.processed.data, "2d.rds"))

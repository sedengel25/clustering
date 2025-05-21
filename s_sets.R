library(tidyverse)
library(here)

path.base <- here("data", "raw", "s-originals")

file.coord <- here(path.base, "s1-groundtruth-plot.xls")
file.label <- here(path.base, "s1-label.pa")
tibble.data <- readxl::read_xls(file.coord, col_names = FALSE)

index.start <- tibble.data %>%
  mutate(id = row_number()) %>%
  filter(...1 == "Data") %>%
  pull(id)


tibble.data <- tibble.data %>%
  mutate(id = row_number()) %>%
  filter(id >= index.start) %>%
  rename(X = ...2, Y = ...3) %>%
  select(X, Y)
char.labels <- read_lines(file = file.label, skip = 5)



path.dir.processed.data <- here("data", "processed", "s-originals")
dir.exists(path.dir.processed.data)
list.data <- list(Data = tibble.data, 
     Cls = as.integer(char.labels))
write_rds(x = list.data, file = here(path.dir.processed.data, "s1.rds"))

library(tidyverse)
library(here)

url.data <- "https://cs.uef.fi/sipu/datasets/skewed.txt"
url.labels <- "https://cs.uef.fi/sipu/datasets/skewed.pa"
data <- read_table(
  file    = url.data,
  col_names = c("x", "y")    
)


path.dir.processed.data <- here("data", "processed", "variations")
if(!dir.exists(path.dir.processed.data)){
  dir.create(path.dir.processed.data)
}

labels <- read_lines(file = url.labels, skip = 4)
list.data <- list(Data = data, 
                  Cls = as.integer(labels))
write_rds(x = list.data, file = here(path.dir.processed.data, "skewed.rds"))
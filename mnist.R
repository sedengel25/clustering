library(tidyverse)
library(here)
library(reticulate)
reticulate::py_config()

np <- import("numpy")
pacmap <-  import("pacmap")

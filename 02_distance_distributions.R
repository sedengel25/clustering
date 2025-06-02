library(FCPS)
library(tidyverse)
library(dbscan)
library(reticulate)
library(plotly)
library(uwot)
library(devtools)
library(here)
library(aricode)
library(scales)
reticulate::py_config()


use_virtualenv("r-reticulate", required = TRUE)

hdb   <- import("hdbscan")
np    <- import("numpy")
pacmap <-  import("pacmap")
sklearn <- import("sklearn.manifold")

py_hdbscan <- function(df, hdb, int.cluster.size, int.sample) {
  mat <- as.matrix(df)
  X_py <- np$array(mat)
  clusterer <- hdb$HDBSCAN(
    min_cluster_size = int.cluster.size,
    min_samples      = int.sample,    
    metric           = "euclidean"
  )
  clusterer$fit(X_py)
  labels_py   <- clusterer$labels_
  labels_py <- labels_py + 1
  return(labels_py)
}


load_data_obj <- function(source, is_rds = FALSE) {
  if (is_rds) {
    data_obj <- read_rds(source)
  } else {
    data(source, package = "FCPS", envir = environment())
    data_obj <- get(source, envir = environment())
  }
  stopifnot(is.list(data_obj),
            all(c("Data","Cls") %in% names(data_obj)))
  return(data_obj)
}
### ----------------------------------------------------------------------------
char.dataset <- "Chainlink"
data.obj  <- load_data_obj(char.dataset, is_rds = FALSE)

# rds.path  <-  here("data", "processed", "s-originals", "s4.rds")
# data.obj  <- load_data_obj(rds.path, is_rds = TRUE)
mat.org <- data.obj$Data
cls     <- data.obj$Cls


n.dim <- ncol(mat.org)
df.org <- as.tibble(mat.org)
mins   <- map_dbl(df.org, min)
maxs   <- map_dbl(df.org, max)
n.noise <- 0
df.noise <- map2_dfc(mins, maxs, ~ runif(n.noise, .x, .y))
df.noise$label_true <- 0L %>% as.factor()
df.org$label_true <-   cls %>% as.factor()
df.final <- rbind(df.org, df.noise)
mat.final <- df.final[,1:n.dim] %>% as.matrix()


pacmap.emb <- pacmap$PaCMAP(n_components = n.dim %>% as.integer)
pacmap.emb <- pacmap.emb$fit_transform(X = mat.final, init = "pca")


df.pacmap <- as.tibble(pacmap.emb)
# df.pacmap$label_true <- df.final$label_true %>% as.factor()
# colnames(df.pacmap) <- c("X", "Y", "label_true"
#                   #, "label_pred"
# )
# 
# colnames(df.final) <- c("X", "Y", "label_true"
#                          #, "label_pred"
# )
# 
# 
# ggplot(df.final, aes(x = X, y = Y, color = label_true)) +
#   geom_point(alpha = 0.1) +
#   ggtitle("Original") +
#   theme(legend.position = "none")
# 
# ggplot(df.pacmap, aes(x = X, y = Y, color = label_true)) +
#   geom_point(alpha = 0.1) +
#   ggtitle("PaCMAP") +
#   theme(legend.position = "none")

dim(mat.final)
dist.org <- dist(mat.final) %>% as.matrix()
dim(dist.org)
dist.emb <- dist(df.pacmap[, c("V1", "V2", "V3")] %>% as.matrix()) %>% as.matrix()

d.org.vec <- dist.org[lower.tri(dist.org)]
d.emb.vec <- dist.emb[lower.tri(dist.emb)]
d.org.vec <- rescale(d.org.vec, to = c(0, 1))
d.emb.vec <- rescale(d.emb.vec, to = c(0, 1))
rm(dist.org)
rm(dist.emb)
gc()


df.distances <- bind_rows(
  data.frame(dist = d.org.vec, type = "Original distances"),
  data.frame(dist = d.emb.vec, type = "PaCMAP distances")
)

rm(d.org.vec)
rm(d.emb.vec)
gc()


ggplot(df.distances, aes(x = dist, fill = type, color = type)) +
  geom_density(alpha = 0.3) +
  labs(x = "Distance", y = "Density") +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 18L),
    axis.text = element_text(size = 16L),
    axis.title = element_text(size = 18L),
    strip.text = element_text(size = 18L)
  )







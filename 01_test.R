library(FCPS)
library(tidyverse)
library(dbscan)
library(reticulate)
library(plotly)
library(uwot)
library(devtools)
library(here)
library(aricode)
reticulate::py_config()


use_virtualenv("r-reticulate", required = TRUE)

hdb   <- import("hdbscan")
np    <- import("numpy")
pacmap <-  import("pacmap")
trimap <- import("trimap")
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

char.dataset <- "EngyTime"
data.obj  <- load_data_obj(char.dataset, is_rds = FALSE)
rds.path  <-  here("data", "processed", "worms", "2d.rds")
data.obj  <- load_data_obj(rds.path, is_rds = TRUE)
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
trimap.mapper <- trimap$TRIMAP(n_dims = n.dim, apply_pca = TRUE)
trimap.emb <-trimap.mapper$fit_transform(mat.final) 
umap.res <- umap(mat.final, ret_model = TRUE, n_components = n.dim, init = "pca")
umap.emb <- umap.res$embedding
tsne.mapper <- sklearn$TSNE(n_components = n.dim %>% as.integer,
                             perplexity   = 30.0,
                             learning_rate= 200.0,
                             init = "pca",
                             n_iter       = as.integer(1000),
                             random_state = as.integer(42L))

tsne.emb <- tsne.mapper$fit_transform(mat.final)



df.pacmap <- as.tibble(pacmap.emb)
df.umap <- as.tibble(umap.emb)
df.trimap <- as.tibble(trimap.emb)
df.tsne <- as.tibble(tsne.emb)

df.pacmap$label_true <- df.final$label_true %>% as.factor()
df.umap$label_true <- df.final$label_true %>% as.factor()
df.trimap$label_true <- df.final$label_true %>% as.factor()
df.tsne$label_true <- df.final$label_true %>% as.factor()
list.dfs <- list(
  Original = df.final,
  PaCMAP   = df.pacmap,
  UMAP     = df.umap,
  TriMap = df.trimap,
  tSNE = df.tsne
)


# int.cluster.size <- 100L
# int.sample <- int.cluster.size
# 
# for (i in c(5, 10, 20, 50, 100, 200, 500, 1000, 2000, 5000, 10000)){
#   int.cluster.size <- i %>% as.integer
#   int.sample <- int.cluster.size
# 
#   df.pacmap$label_pred <- py_hdbscan(df = df.pacmap[,1:2],
#              hdb = hdb,
#              int.cluster.size = int.cluster.size,
#              int.sample = int.sample) %>% as.factor()
#   
#   df.umap$label_pred <- py_hdbscan(df = df.umap[,1:2],
#                                      hdb = hdb,
#                                      int.cluster.size = int.cluster.size,
#                                      int.sample = int.sample) %>% as.factor()
#   
#   df.trimap$label_pred <- py_hdbscan(df = df.trimap[,1:2],
#                                    hdb = hdb,
#                                    int.cluster.size = int.cluster.size,
#                                    int.sample = int.sample) %>% as.factor()
#   
#   df.final$label_pred <- py_hdbscan(df = df.final[,1:2],
#                                      hdb = hdb,
#                                      int.cluster.size = int.cluster.size,
#                                      int.sample = int.sample) %>% as.factor()
#   cat("min_cluster_size: ", i, "\n")
#   cat("ARI pacmap: ", ARI(df.pacmap$label_true, df.pacmap$label_pred), "\n")
#   cat("ARI umap: ", ARI(df.umap$label_true, df.umap$label_pred), "\n")
#   cat("ARI trimap: ", ARI(df.trimap$label_true, df.trimap$label_pred), "\n")
#   cat("ARI org: ", ARI(df.final$label_true, df.final$label_pred), "\n")
# }

list.dfs <- list(
  Original = df.final,
  PaCMAP   = df.pacmap,
  UMAP     = df.umap,
  TriMap = df.trimap,
  tSNE = df.tsne
)

plot_fun <- function(df, name) {
  if (ncol(df) == 3) {
    colnames(df) <- c("X", "Y", "label_true"
                      #, "label_pred"
                      )
    
    ggplot(df, aes(x = X, y = Y, color = label_true)) +
      geom_point(alpha = 0.1) +
      ggtitle(name) +
      theme(legend.position = "none")
    

    
  } else if(ncol(df) == 4) {
    colnames(df) <- c("X", "Y", "Z", "label_true"
                      #, "label_pred"
                      )
    
    plot_ly(
      data   = df,
      x      = ~X, y = ~Y, z = ~Z,
      color  = ~label_true,
      type   = "scatter3d",
      mode   = "markers",
      marker = list(size = 4) 
    ) %>%
      layout(title = name)
  }
}

plots <- imap(list.dfs, plot_fun)

plots$Original
#length(unique((df.final$label_pred)))
plots$PaCMAP
#length(unique((df.pacmap$label_pred)))
plots$UMAP
#length(unique((df.umap$label_pred)))
plots$TriMap
#length(unique((df.trimap$label_pred)))
plots$tSNE

# plotly_fun <- function(df, name) {
#   colnames(df)[1:2] <- c("X", "Y")
#   plot_ly(
#     data = df,
#     x = ~X, y = ~Y,
#     split = ~label_true,
#     type = "scatter",
#     mode = "markers",
#     marker = list(size = 6)
#   ) %>%
#     layout(
#       title = name,
#       legend = list(title = list(text = "Klasse"))
#     )
# }
# 
# interactive_plots <- imap(list.dfs, plotly_fun)
# interactive_plots$Original

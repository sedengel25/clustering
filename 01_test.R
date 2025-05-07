library(FCPS)
library(tidyverse)
library(dbscan)
library(reticulate)
library(plotly)

reticulate::py_config()

use_virtualenv("r-reticulate", required = TRUE)

hdb   <- import("hdbscan")
np    <- import("numpy")
pacmap <-  import("pacmap")

data("Tetra")


mat.org <- Tetra$Data


n.dim <- ncol(mat.org) - 1
embedding <- pacmap$PaCMAP(n_components = n.dim %>% as.integer, 
                           n_neighbors = 10L, 
                           MN_ratio = 0.5, 
                           FP_ratio = 0.2)
mat.emb <- embedding$fit_transform(X = mat.org, init = "pca")


df.org <- as.data.frame(mat.org)
df.emb <- as.data.frame(mat.emb)
df.org$label_true <- Tetra$Cls %>% as.factor()
df.emb$label_true <- Tetra$Cls %>% as.factor()


if(ncol(mat.org) == 2){
  colnames(df.org) <- c("X", "Y", "label")
  colnames(df.emb) <- c("X", "Y", "label")
  
  ggplot(data = df.org) +
    geom_point(aes(x = X, y = Y, color = label))
  
  ggplot(data = df.emb) +
    geom_point(aes(x = X, y = Y, color = label))
  
} else if(ncol(mat.org) == 3) {
  colnames(df.org) <- c("X", "Y", "Z", "label")
  colnames(df.emb) <- c("X", "Y", "Z", "label")
  
  p.org <- plot_ly(
    data    = df.org,
    x       = ~X,
    y       = ~Y,
    z       = ~Z,
    color   = ~label,              
    # colors  = RColorBrewer::brewer.pal(n = length(levels(df.org$label)), name = "Set1"),
    type    = "scatter3d",
    mode    = "markers",
    marker  = list(size = 4)
  )
  print(p.org)
  p.emb <- plot_ly(
    data    = df.emb,
    x       = ~X,
    y       = ~Y,
    z       = ~Z,
    color   = ~label,              
    # colors  = RColorBrewer::brewer.pal(n = length(levels(df.org$label)), name = "Set1"),
    type    = "scatter3d",
    mode    = "markers",
    marker  = list(size = 4)
  )
  
  print(p.emb)
}





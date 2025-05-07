library(FCPS)
library(tidyverse)
library(dbscan)
library(reticulate)
library(plotly)
#install.packages("uwot")
library(uwot)
reticulate::py_config()

use_virtualenv("r-reticulate", required = TRUE)

hdb   <- import("hdbscan")
np    <- import("numpy")
pacmap <-  import("pacmap")
char.dataset <- "Tetra"
data(char.dataset)


mat.org <- Tetra$Data


n.dim <- ncol(mat.org)
pacmap.emb <- pacmap$PaCMAP(n_components = n.dim %>% as.integer, 
                           n_neighbors = 10L, 
                           MN_ratio = 0.5, 
                           FP_ratio = 0.2)
pacmap.emb <- pacmap.emb$fit_transform(X = mat.org, init = "pca")


umap.res <- umap(mat.org, ret_model = TRUE, n_components = n.dim, init = "random")
umap.emb <- umap.res$embedding
df.org <- as.data.frame(mat.org)
df.pacmap <- as.data.frame(pacmap.emb)
df.umap <- as.data.frame(umap.emb)



df.org$label_true <- Tetra$Cls %>% as.factor()
df.pacmap$label_true <- Tetra$Cls %>% as.factor()
df.umap$label_true <- Tetra$Cls %>% as.factor()

if(ncol(mat.org) == 2){
  colnames(df.org) <- c("X", "Y", "label")
  colnames(df.pacmap) <- c("X", "Y", "label")
  colnames(df.umap) <- c("X", "Y", "label")
  
  ggplot(data = df.org) +
    geom_point(aes(x = X, y = Y, color = label))
  
  ggplot(data = df.pacmap) +
    geom_point(aes(x = X, y = Y, color = label))
  
  ggplot(data = df.umap) +
    geom_point(aes(x = X, y = Y, color = label))
  
} else if(ncol(mat.org) == 3) {
  colnames(df.org) <- c("X", "Y", "Z", "label")
  colnames(df.pacmap) <- c("X", "Y", "Z", "label")
  colnames(df.umap) <- c("X", "Y", "Z", "label")
  
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
  p.pacmap <- plot_ly(
    data    = df.pacmap,
    x       = ~X,
    y       = ~Y,
    z       = ~Z,
    color   = ~label,              
    # colors  = RColorBrewer::brewer.pal(n = length(levels(df.org$label)), name = "Set1"),
    type    = "scatter3d",
    mode    = "markers",
    marker  = list(size = 4)
  )
  
  print(p.pacmap)
  
  p.umap <- plot_ly(
    data    = df.umap,
    x       = ~X,
    y       = ~Y,
    z       = ~Z,
    color   = ~label,              
    # colors  = RColorBrewer::brewer.pal(n = length(levels(df.org$label)), name = "Set1"),
    type    = "scatter3d",
    mode    = "markers",
    marker  = list(size = 4)
  )
  
  print(p.umap)
}





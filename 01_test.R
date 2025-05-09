library(FCPS)
library(tidyverse)
library(dbscan)
library(reticulate)
library(plotly)
library(uwot)
library(devtools)

reticulate::py_config()


use_virtualenv("r-reticulate", required = TRUE)

hdb   <- import("hdbscan")
np    <- import("numpy")
pacmap <-  import("pacmap")
trimap <- import("trimap")
char.dataset <- "Lsun3D"
data(char.dataset)


mat.org <- Lsun3D$Data


n.dim <- ncol(mat.org)
pacmap.emb <- pacmap$PaCMAP(n_components = n.dim %>% as.integer, 
                           n_neighbors = 10L, 
                           MN_ratio = 0.5, 
                           FP_ratio = 0.2)
pacmap.emb <- pacmap.emb$fit_transform(X = mat.org, init = "pca")
trimap.mapper <- trimap$TRIMAP(n_dims = n.dim)
trimap.emb <-trimap.mapper$fit_transform(mat.org) 
umap.res <- umap(mat.org, ret_model = TRUE, n_components = n.dim, init = "random")
umap.emb <- umap.res$embedding

df.org <- as.data.frame(mat.org)
df.pacmap <- as.data.frame(pacmap.emb)
df.umap <- as.data.frame(umap.emb)
df.trimap <- as.data.frame(trimap.emb)

df.org$label_true <- Lsun3D$Cls %>% as.factor()
df.pacmap$label_true <- Lsun3D$Cls %>% as.factor()
df.umap$label_true <- Lsun3D$Cls %>% as.factor()
df.trimap$label_true <- Lsun3D$Cls %>% as.factor()

list.dfs <- list(
  Original = df.org,
  PaCMAP   = df.pacmap,
  UMAP     = df.umap,
  TriMap = df.trimap
)



plot_fun <- function(df, name) {
  if (ncol(df) == 3) {
    colnames(df) <- c("X", "Y", "label_true")
    
    ggplot(df, aes(x = X, y = Y, color = label_true)) +
      geom_point() +
      ggtitle(name)
    
  } else if(ncol(df) == 4) {
    colnames(df) <- c("X", "Y", "Z", "label_true")
    
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
plots$PaCMAP
plots$UMAP
plots$TriMap

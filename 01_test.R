library(FCPS)
library(tidyverse)
library(dbscan)
library(reticulate)
library(plotly)
library(uwot)
library(devtools)
library(here)
reticulate::py_config()


use_virtualenv("r-reticulate", required = TRUE)

hdb   <- import("hdbscan")
np    <- import("numpy")
pacmap <-  import("pacmap")
trimap <- import("trimap")



load_data_obj <- function(source, is_rds = FALSE) {
  if (is_rds) {
    data_obj <- readRDS(source)
  } else {
    data(source, package = "FCPS", envir = environment())
    data_obj <- get(source, envir = environment())
  }
  stopifnot(is.list(data_obj),
            all(c("Data","Cls") %in% names(data_obj)))
  return(data_obj)
}

# char.dataset <- "EngyTime"
# data.obj  <- load_data_obj(char.dataset, is_rds = FALSE)
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
pacmap.emb <- pacmap.emb$fit_transform(X = mat.final, init = "random")
trimap.mapper <- trimap$TRIMAP(n_dims = n.dim)
trimap.emb <-trimap.mapper$fit_transform(mat.final) 
umap.res <- umap(mat.final, ret_model = TRUE, n_components = n.dim, init = "random")
umap.emb <- umap.res$embedding

df.pacmap <- as.tibble(pacmap.emb)
df.umap <- as.tibble(umap.emb)
df.trimap <- as.tibble(trimap.emb)


df.pacmap$label_true <- df.final$label_true 
df.umap$label_true <- df.final$label_true 
df.trimap$label_true <- df.final$label_true 

list.dfs <- list(
  Original = df.final,
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



plotly_fun <- function(df, name) {
  colnames(df)[1:2] <- c("X", "Y")
  plot_ly(
    data = df,
    x = ~X, y = ~Y,
    split = ~label_true,
    type = "scatter",
    mode = "markers",
    marker = list(size = 6)
  ) %>%
    layout(
      title = name,
      legend = list(title = list(text = "Klasse"))
    )
}

interactive_plots <- imap(list.dfs, plotly_fun)
interactive_plots$Original

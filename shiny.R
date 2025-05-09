library(shiny)
library(FCPS)
library(tidyverse)
library(uwot)
library(reticulate)
library(plotly)

# Use the specified virtual environment for Python dependencies
reticulate::use_virtualenv("r-reticulate", required = TRUE)
pacmap <- import("pacmap")
trimap <- import("trimap")

# List all FCPS datasets
dataset_names <- data(package = "FCPS")$results[, "Item"]

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Choose dataset:",
                  choices = dataset_names,
                  selected = dataset_names[1]),
      numericInput("n_noise", "Number of noise points:",
                   value = 50, min = 0, step = 1)
    ),
    mainPanel(
      fluidRow(
        column(6, plotlyOutput("originalPlot")),
        column(6, plotlyOutput("umapPlot"))
      ),
      fluidRow(
        column(6, plotlyOutput("pacmapPlot")),
        column(6, plotlyOutput("trimapPlot"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive load of original dataset
  datasetObj <- reactive({
    data(list = input$dataset, package = "FCPS", envir = environment())
    get(input$dataset, envir = environment())
  })
  
  # Original matrix and labels
  mat_orig <- reactive({ datasetObj()$Data })
  labels_orig <- reactive({ factor(datasetObj()$Cls) })
  n_dim <- reactive({ ncol(mat_orig()) })
  
  # Combine original + noise in bounding box
  df_final <- reactive({
    m <- mat_orig()
    k <- ncol(m)
    # Original tibble
    df_org <- as_tibble(m) %>%
      set_names(paste0("Dim", seq_len(k))) %>%
      mutate(label = labels_orig())
    # Bounds
    mins <- map_dbl(df_org %>% select(starts_with("Dim")), min)
    maxs <- map_dbl(df_org %>% select(starts_with("Dim")), max)
    # Generate noise
    df_noise <- map2_dfc(mins, maxs, ~ runif(input$n_noise, .x, .y)) %>%
      set_names(paste0("Dim", seq_len(k))) %>%
      mutate(label = factor(0))
    # Combine
    bind_rows(df_org, df_noise)
  })
  
  # Final matrix and labels
  mat_final <- reactive({ as.matrix(df_final() %>% select(starts_with("Dim"))) })
  labels_final <- reactive({ df_final()$label })
  
  # Compute embeddings for different methods
  compute_embed <- function(method) {
    m <- mat_final()
    k <- n_dim()
    emb <- switch(method,
                  original = m,
                  umap     = umap(m, n_components = k),
                  pacmap   = {
                    mapper <- pacmap$PaCMAP(n_components = as.integer(k),
                                            n_neighbors = 10L,
                                            MN_ratio = 0.5,
                                            FP_ratio = 0.2)
                    mapper$fit_transform(m, init = "pca")
                  },
                  trimap   = {
                    mapper <- trimap$TRIMAP(n_dims = as.integer(k))
                    mapper$fit_transform(m)
                  })
    df <- as_tibble(emb)
    colnames(df) <- paste0("Dim", seq_len(k))
    df$label <- labels_final()
    df
  }
  
  # Dynamic plot (2D or 3D)
  render_dynamic <- function(df, title) {
    k <- n_dim()
    if (k == 3) {
      plot_ly(df, x = ~Dim1, y = ~Dim2, z = ~Dim3,
              color = ~label, type = 'scatter3d', mode = 'markers',
              size = 2) %>%
        layout(title = title)
    } else {
      plot_ly(df, x = ~Dim1, y = ~Dim2,
              color = ~label, type = 'scatter', mode = 'markers',
              size = 2) %>%
        layout(title = title)
    }
  }
  
  output$originalPlot <- renderPlotly({
    render_dynamic(compute_embed('original'), "Original + Noise")
  })
  output$umapPlot <- renderPlotly({
    render_dynamic(compute_embed('umap'), "UMAP + Noise")
  })
  output$pacmapPlot <- renderPlotly({
    render_dynamic(compute_embed('pacmap'), "PaCMAP + Noise")
  })
  output$trimapPlot <- renderPlotly({
    render_dynamic(compute_embed('trimap'), "TriMap + Noise")
  })
}

shinyApp(ui, server)
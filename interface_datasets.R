library(rvest)
library(stringr)

base.url <- "https://cs.uef.fi/sipu/datasets/"
page <- read_html(base_url)
hrefs <- page %>% html_nodes("a") %>% html_attr("href")
file.links <- hrefs[str_detect(hrefs, "\\.(ts|txt|zip|pa|cb|labels|integer)$")]
full.links <- url_absolute(file_links, base_url)
datasets <- str_split(string = basename(full.links), pattern = "\\.", simplify = TRUE)[,1]

library(tidyverse)
library(rvest)
library(glue)
library(httr)

base_page_url <- "https://www.pracuj.pl/praca?pn="

N_pages <- 759

safe_read_html <- function(f_url) {
  t_page <- GET(f_url,
              user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/71.0.3578.80 Safari/537.36"))
  if(t_page$status_code == 200) {
    t_page$content %>% rawToChar() %>% read_html()
  } else {
    return(NULL)
  }
}


all_links <- vector("character")

for(i in seq_len(N_pages)) {

  cat(glue("Wczytuje {i} stronę indeksu"))

  page_url <- glue("{base_page_url}{i}")

  page <- safe_read_html(page_url)

  if(!is_null(page)) {

    ids <- page %>%
      html_nodes("head > script") %>%
      .[[2]] %>%
      as.character() %>%
      str_match_all("([0-9]){7,}") %>%
      .[[1]] %>%
      .[, 1]

    all_links <- c(all_links, ids)

    cat(" - gotowe\r")
  } else {

    cat(" - błąd\n")
  }
}

print(length(all_links))
all_links <- unique(all_links)
print(length(all_links))

all_links <- paste0("https://www.pracuj.pl/praca/x,oferta,", all_links)


saveRDS(all_links, file = "offer_urls.rds")

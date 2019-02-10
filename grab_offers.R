# pobranie ogloszenia

library(tidyverse)
library(rvest)
library(glue)

safe_read_html <- safely(read_html)


get_offet_details <- function(offer_url) {
  offer_page <- safe_read_html(offer_url)

  if(is_null(offer_page$error)) {
    offer_page <- offer_page$result

    # tytuł oferty
    offer_title <- offer_page %>%
      html_nodes("h1#offerTitle") %>%
      html_text() %>%
      str_trim()

    # treść oferty - wersja z description
    offer_text <- offer_page %>%
      # treść jest w tabeli
      html_node("table#offTable") %>%
      # div w opisem oferty
      html_node("div#description") %>%
      # traktujemy to jako HTML
      as.character() %>%
      # usuwamy tagi htmla
      str_replace_all("<[^>]*>|\n|\r", " ") %>%
      str_squish()

    # jesli sie nie udało próbujemy innej wersji
    if(is.na(offer_text)) {
      # treść oferty - wersja z desc
      offer_text <- offer_page %>%
        # treść jest w tabeli
        html_node("table#offTable") %>%
        # div w opisem oferty
        html_nodes("div.desc") %>%
        as.character() %>%
        paste(., collapse = "\n") %>%
        str_replace_all("<[^>]*>|\n|\r", " ") %>%
        str_squish()
    }

    # parametry oferty
    offer_params <- offer_page %>%
      html_node("div.content") %>%
      html_nodes("script") %>%
      .[[2]] %>%
      html_text() %>%
      str_split("\r\n") %>%
      unlist() %>%
      .[nchar(.) != 0]

    offer_category <- str_sub(offer_params[1], 37, nchar(offer_params[1])-2)
    offer_localization <- str_sub(offer_params[2], 39, nchar(offer_params[2])-2)
    offer_grade <- str_sub(offer_params[3], 34, nchar(offer_params[3])-2)
    offer_company <- str_sub(offer_params[4], 39, nchar(offer_params[4])-2)

    # czy sa podane zarobki?
    l <- offer_page %>% html_nodes("span.o-main__right_offer_cnt_details_item_text")
    if(length(which(html_attr(l, "itemprop") == "baseSalary")) != 0) {
      offer_salary <- l[which(html_attr(l, "itemprop") == "baseSalary")] %>% html_text() %>% str_squish()
    } else {
      offer_salary <- "empty"
    }

    return(tibble(company = offer_company,
                  title = offer_title,
                  grade = offer_grade,
                  localization = offer_localization,
                  category = offer_category,
                  body = offer_text,
                  salary = offer_salary,
                  url = offer_url))

  } else {

    return(tibble())
  }
}


all_links <- readRDS("offer_urls.rds")


# czy sa zapisane jakies dane?
newest_file <- list.files("data/")

if(length(newest_file) == 0) {
  # nie ma - zaczynamy od poczatku
  offers <- tibble()
  start_i <- 1
} else {
  # sa - zaczynamy od ostatnio zapisanego ogloszenia
  newest_file <- newest_file %>% sort(decreasing = TRUE) %>% .[[1]]
  newest_file <- paste0("data/", newest_file)
  offers <- readRDS(newest_file)
  start_i <- max(offers$i) + 1
}


for(i in start_i:length(all_links)) {

  cat(glue("Grabbing offer no {i}"))

  temp_offer <- get_offet_details(all_links[[i]]) %>% mutate(i = i)
  cat(" - done\r")

  offers <- bind_rows(offers, temp_offer)

  if(i %% 25 == 0) {
    saveRDS(offers, file = format(Sys.time(), "data/grabbed_offers_%Y_%m_%d_%H%M.rds"))
    cat("\nData file saved!\n")
  }

  Sys.sleep(5)
}

saveRDS(offers, file = format(Sys.time(), "data/grabbed_offers_%Y_%m_%d_%H%M.rds"))

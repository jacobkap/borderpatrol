get_links <- function(url, subset_string = NULL) {
  page <- xml2::read_html(url)
  links <-
    page %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  if (!is.null(subset_string)) {
    links <- links[grep(subset_string, links)]
  }
  return(links)
}

download_border_patrol_pdfs <- function() {
  setwd(here::here("data/raw"))
  links <- get_links("https://www.cbp.gov/newsroom/media-resources/stats",
                     subset_string = ".pdf")
  links2 <- get_links("https://www.cbp.gov/newsroom/media-resources/stats?page=1",
                      subset_string = ".pdf")
  links <- c(links, links2)
  links <- links[grep("Vision-Strategy", links, invert = TRUE)]

  for (link in links) {

    file_name <- gsub(".*/", "", link)
    file_name <- gsub("%20|-", "_", file_name)
    file_name <- gsub("_+", "_", file_name)
    file_name <- tolower(file_name)
    download.file(link,
                  destfile = file_name,
                  mode = "wb")

  }
}

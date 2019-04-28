library(haven)
convert_codebook_to_pdf <- function(file_name) {
  temp      <- readLines(file_name)
  old_name  <- file_name
  file_name <- gsub(".txt", ".Rmd", file_name)
  cat(temp, sep = "  \n", file = file_name)
  suppressMessages(rmarkdown::render(file_name,
                                     rmarkdown::pdf_document(), quiet = TRUE))
  file.remove(old_name)
  file.remove(file_name)
}

save_files <- function(data, year, file_name, save_name, rda_only = FALSE) {


  if (any(nchar(names(data)) > 30)) {
    print(names(data)[nchar(names(data)) > 30])
  }

  assign(paste0(file_name, year), data) # Change name
  save(list = paste0(file_name, year),
       file = paste0(save_name, year, ".rda"))

  if (rda_only == FALSE) {
    codebook_name <- paste0(file_name, "_codebook_", year, ".txt")
    codebook_name <- gsub("__", "_", codebook_name)
    codebook_name <- gsub("_\\.", "\\.", codebook_name)
    memisc::Write(memisc::codebook(data),
                  file = codebook_name)
    convert_codebook_to_pdf(codebook_name)

    do.call("write_dta", list(as.name(paste0(file_name, year)),
                              path = paste0(save_name,
                                            year, ".dta")))

    do.call("write_csv", list(as.name(paste0(file_name, year)),
                              path = paste0(save_name,
                                            year, ".csv")))

    do.call("rm", list(as.name(paste0(file_name, year))))
  }
}

save_as_zip <- function(file_name, pattern = NULL) {

  all_files <- list.files(pattern = pattern)
  all_files <- sort(all_files)

  zip::zipr(zipfile = paste0(file_name, ".zip"),
            files = all_files)
}


save_raw_as_zip <- function(file_name) {
  all_files <- list.files()
  zip::zip(zipfile = paste0(file_name, ".zip"),
           files = all_files)
}

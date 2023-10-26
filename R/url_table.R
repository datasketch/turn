
#' @export
url_into_table <- function(url){

  if(!is_url){
    stop("Must be a url")
  }

  if(is_google_sheet_url(url)){
    d <- read_public_googlesheet_table(url)
    return(d)
  }

  if(is_github_or_gist_url(url)){
    url <- github_to_raw_url(url)
  }

  ext <- dstools::file_ext(url)
  path <- tempfile(fileext = paste0(".",ext))
  download.file(url, destfile = path)
  d <- table_read(path)
  unlink(path)
  d
}



read_public_googlesheet_table <- function(url) {

  googlesheets4::gs4_deauth()
  result <- tryCatch({
    # Attempt to read the sheet
    data <- read_sheet(url)
    return(data)
  },
  error = function(e) {
    # If an error occurs (e.g., the sheet is private), return a custom message
    if(grepl("PERMISSION_DENIED", e$message)) {
      stop("The Google Sheet appears to be private or inaccessible. Please make sure the sheet is public.")
    } else {
      # Return the original error message if it's not related to HTTP access
      return(paste("An error occurred:", e$message))
    }
  })

  result

}

read_public_googlesheet_tables <- function(url) {
  googlesheets4::gs4_deauth()

  tables <- googlesheets4::sheet_names(url)
  # Read the sheet
  purrr::map(tables, function(sheet){
    googlesheets4::read_sheet(url, sheet = sheet)
  }) |> purrr::set_names(tables)
}




is_url <- function(x) {
  # Regular expression for URL validation
  pattern <- "^(http|https)://[a-z0-9]+([-.]{1}[a-z0-9]+)*\\.[a-z]{2,5}(:[0-9]{1,5})?(/.*)?$"
  grepl(pattern, x, ignore.case = TRUE)
}


is_google_sheet_url <- function(url) {
  # Check if the URL contains the necessary components to be a Google Sheet URL
  grepl("^https://docs\\.google\\.com/spreadsheets/d/.+", url)
}

is_github_or_gist_url <- function(url) {
  # Check if the URL is either a GitHub file URL or a GitHub Gist URL
  is_github_file <- grepl("^https://github\\.com/[^/]+/[^/]+/blob/.+", url)
  is_gist <- grepl("^https://gist\\.github\\.com/[^/]+/[^/]+", url)

  return(is_github_file || is_gist)
}

github_to_raw_url <- function(url) {
  raw_url <- gsub("^https://github\\.com/", "https://raw.githubusercontent.com/", url)
  raw_url <- gsub("/blob/", "/", raw_url)
  raw_url
}




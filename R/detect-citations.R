#' Detect pandoc-style citations
#'
#' By default, this omits any references within code chunks, inline R code,
#' or URLs. To force the inclusion of a reference in the output, include it
#' in an HTML comment or the
#' [nocite front matter field](https://rmarkdown.rstudio.com/authoring_bibliographies_and_citations.html#Unused_References_(nocite)).
#'
#' @param path A character vector, file or URL whose contents may contain citation keys.
#' Multiple files can be passed in as a vector (e.g., from \link{list.files}).
#' @param locale See \link[readr]{default_locale}. Use if encoding might
#' be a problem.
#' @param prefix2rm \code{character} vector with patterns that will be use to
#' search and remove undesireable citations. See Details.
#'
#' @details
#' Quarto has implemented some features that
#'
#'
#' @return A character vector of unique citation keys.
#' @export
#'
#' @examples
#' bbt_detect_citations("\n@citation1 and [@citation2] but not \\@citation3")
#'
bbt_detect_citations <- function(path = bbt_guess_citation_context(),
                                 locale = default_locale(),
                                 prefix2rm = c("fig-", "tbl-", "eq-", "sec-", "lst-", "thm-")){

  text <- vapply(path, read_file, locale = locale, FUN.VALUE = character(1))

  bbt_detect_citations_chr(text = text, prefix2rm = prefix2rm)
}

#' @rdname bbt_detect_citations
#' @export
bbt_guess_citation_context <- function(){

  knitr_doc <- current_input()

  if(is.null(knitr_doc)) {
    stop("Can't detect context (tried current knitr doc)", call. = FALSE)
  }

  knitr_doc
}

bbt_detect_citations_chr <- function(text, prefix2rm) {

  # Checking text in chunks and chunk headers
  # Get indices of positions for chunk begginings
  index <- gregexpr(text = text, pattern = "\n```\\{.+?\\}.+?\r?\n```")

  # Extract chunk codes
  text_chunks <- mapply(start = index[[1]],
                        stop = index[[1]] + attr(x = index[[1]], which = "match.length"),
                        FUN = substr, x = text, SIMPLIFY = FALSE) |>

    # Splitting chunks' text by page breaks
    lapply(strsplit, split = "\n") |>

    # Convert inner list into vectors (getting then a list of vectors)
    lapply(unlist) |>

    # Extracting lines of chunk headers and that starts with #|
    lapply(FUN = str_extract_all,
           pattern = "(^```\\{.+?\\})|(^#\\|[[:print:]]{1,})") |>

    # Cleaning findings and getting a list of vectors
    lapply(unlist) |>

    # Extracting lines that initialize with patterns indicated in 'prefix2rm'
    lapply(FUN = str_extract_all,
           pattern = paste0(paste(sprintf("(^#\\| %scap: *'.+?')", prefix2rm), collapse = "|"),
                            "|(fig\\.cap *= *(\"|').+?(\"|'))")) |>

    # Cleaning findings and getting a list of vectors
    lapply(unlist)

  # Index of non-empty values
  index <- sapply(text_chunks, length) > 0

  # If any, keeping lines with informative lines
  text_chunks <- ifelse(test = sum(index) > 0,
                        yes = paste(text_chunks[index], collapse = "\n"),
                        no = "")

  # regexes inspired from here:
  # https://github.com/benmarwick/wordcountaddin/blob/master/R/hello.R#L163-L199
  text <- paste0(text, collapse = "\n") |>

    # don't include text in code chunks
    gsub(pattern = "\n```\\{.+?\\}.+?\r?\n```", replacement = "") |>

    # don't include text in in-line R code
    gsub(pattern = "`r.+?`", replacement = "") |>

    # don't include inline markdown URLs
    gsub(pattern = "\\(http.+?\\)", replacement = "") |>

    gsub(pattern = "<http.+?>", replacement = "")

  refs <- str_match_all(string = paste(text, text_chunks, sep = "\n\n"),
                        pattern = regex(pattern = "[^a-zA-Z0-9\\\\]@([a-zA-Z0-9_\\.\\-:]+[a-zA-Z0-9])",
                                        multiline = TRUE, dotall = TRUE))[[1]][, 2, drop = TRUE]

  # Identify refs that match with prefix2rm patterns and removing them
  index <- grepl(pattern = paste(sprintf("(^%s)", prefix2rm), collapse = "|"),
                 x = refs, ignore.case = TRUE)

  if(sum(index) > 0){
    message(sprintf("The next refs were not considered based on 'prefix2rm' argument:\n%s",
                    paste(unique(refs[index]), collapse = ", ")))

    refs <- refs[!index]
  }

  unique(refs)
}

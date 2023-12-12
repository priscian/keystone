#' @export
ask_multiple_choice <- function(
  choices = c("yes", "no"), # N.B. These will be trimmed for whitespace & lowercased
  prompt = "Please choose one (%s): ",
  require_full_choice = FALSE,
  choices_sep = "/",
  min_char_marker = "_" # Or NULL or e.g. ")"
)
{
  choices_abo <- choices
  choices_trimmed <- choices %>% stringr::str_trim()

  choices %<>% stringr::str_trim() %>% tolower

  if (any(nchar(choices) == 0))
    stop("Argument 'choices' contains a zero-length option")

  if (length(unique(choices)) != length(choices))
    stop("Argument 'choices' is not a set of unique alternatives")

  choices_cleaned <- choices

  ## Find the minimum no. of starting characters needed to distinguish between
  ##   all the given choices
  if (!require_full_choice) {
    areChoicesUnique <- FALSE
    for (i in seq(min(nchar(choices))))
    {
      if (length(unique(substr(choices, 0, i))) == length(choices)) {
        areChoicesUnique <- TRUE
        choices <- substr(choices, 0, i)

        break
      }
    }

    if (!areChoicesUnique)
      stop("Argument 'choices' does not allow unique partial matching for all alternatives")

    ## Regular expressions template
    re_template <- "^(%s)"
  } else {
    ## Regular expressions template
    re_template <- "^(%s)$"
  }

  choices_cleaned_marked <- choices_trimmed
  if (!is_invalid(min_char_marker) && !require_full_choice) {
    choices_cleaned_marked <-
      sapply(choices_trimmed, stringr::`str_sub<-`, start = i + 1, end = i,
        value = min_char_marker, USE.NAMES = FALSE)
  }

  suppressWarnings(
    promptText <- sprintf(prompt, paste(choices_cleaned_marked, collapse = choices_sep))
  )

  repeat {
    answer_abo <- readline(prompt = promptText) # This already trims leading/trailing whitespace
    answer <- answer_abo %>% stringr::str_trim() %>% tolower

    if (stringr::str_detect(answer, sprintf(re_template, paste(choices, collapse = "|")))) {
      if (!require_full_choice)
        matched_choice_index <- stringr::str_which(choices, substr(answer, 0, i))
      else
        matched_choice_index <- stringr::str_which(choices, answer)

      break
    }
  }

  r <- structure(list(
    original = choices_trimmed[matched_choice_index],
    lowercase = choices_cleaned[matched_choice_index],
    minimal = choices[matched_choice_index]
  ), answer = answer_abo)

  r
}


#' @export
press_enter_to_continue <- function(x = "Press [enter] to continue", before = "", after = "")
{
  invisible(readline(prompt = sprintf("%s%s", before, x)))
  cat(after)
}

#' Get paths needed for script
#'
#' @return List with elements
#'   \code{base} (path of the current project),
#'   \code{r} (path containing r script files),
#'   \code{dat} (path containing the data files),
#' @examples
#'   path <- get_paths()
#' @export
#'
get_paths <- function() {
  path <- list()
  ## get dropbox path relative to current user:
  path$base <- file.path(
    Sys.getenv("HOME"),   ## home dir of current user
    "Dropbox/lists"       ## Dropbox directory
  )
  ## define script and data paths relative to raw path:
  ## (currently not used)
  path$r <- file.path(path$base, "")
  path$dat <- file.path(path$base, "")
  return(path)
}

#' Read health files.
#'
#' All files that correspond to the regular expression will be read and
#' post-processed (removing blank lines, trimmed).
#'
#' @param path Path where the files reside.
#' @param filename_regex Regular expression that is used to match the
#'   files that should be parsed
#'
#' @return A character vector containting the text in the files (which
#'   usually contains a date, a time, and one or more key-value pairs).
#' @examples
#' dat_txt <- read_healthfiles("/Users/name/dat_dir")
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
#'
read_healthfiles <- function(
	path,
	filename_regex = "^health-[0-9]{4}\\.txt")
{
  ## get list of full filenames:
  filename <- list.files(path, filename_regex, full.names = TRUE)

  ## get content of files and unlist:
  dat_txt <- purrr::map(filename, ~ readr::read_lines(.x)) %>%
    unlist()

  ## trim and remove empty lines:
  dat_txt %<>% stringr::str_trim() %>%
    { .[-which(. == "")]}

  ## preprocess:
  dat_txt %<>% stringr::str_replace_all("; +", "; ")

  return(dat_txt)
}


#' Remove comments. And empty lines.
#'
#' Anything after a "##" is removed, including the "##".
#'
#' @param dat_txt Character vector. Usually, but not necessarily, contains
#'   date, time, and one or more key-value pairs. Might also contain
#'   comments after "##" characters.
#'
#' @return The same character vector, but with all comments removed.
#'
#' @examples
#' remove_comments("## this is a comment")
#' remove_comments("2018-08-10; 16:00; note = some text ## with some comment")
#' remove_comments(c(
#'   "## this is a comment",
#'   "2018-08-10; 16:00; note = some text ## with some comment")
#' )
#' @export
remove_comments <- function(dat_txt) {
  ## remove everything after double hashtag:
  ret <- stringr::str_replace(dat_txt, pattern = "##.*$", "")
  ## remove white spaces at beginning and end of line:
  ret <- trimws(ret)
  ## remove empty lines:
  ret <- ret[ret != ""]
  return(ret)
}
#remove_comments(dat_txt)


#' Get timestamps from each line
#'
#' Timestamps have to have the format of "2018-08-24; 17:40;"
#'
#' @param dat_txt Character vector that contains date, time, and one or more
#'   key-value pairs.
#'
#' @return Timestamp of type \code{lubridate::Date}
#'
#' @examples
#' get_timestamp("2018-08-24; 17:40;")
#' \dontrun{
#' get_timestamp(dat_txt[1:2])
#' get_timestamp(dat_txt)
#' get_timestamp(remove_comments(dat_txt))
#' get_timestamp(dat_txt[409:414])
#' }
#' @export
get_timestamp <- function(dat_txt) {
  ## split on semicolon and take first two elements (date and time):
  date_time <- stringr::str_split_fixed(dat_txt, ";", 3)[, 1:2]
  ## make sure that result is a matrix, in case input vector has length 1:
  date_time <- matrix(date_time, ncol = 2)
  ## convert to date:
  ret <- as.Date(lubridate::ymd_hm(paste(date_time[,1], date_time[, 2])))
  return(ret)
}


#' Transform text data (date, key-value pairs) into a long-format data frame.
#'
#' @param dat_txt Character vector that contains date, time, and one or more
#'   key-value pairs.
#' @param sep Character vector of length 1 that specifies which separator
#'   is used to separate the key-value pairs from each other. Default is ";".
#'
#' @return A tibble with three columns: timestamp, key, value. Both key
#'   and value columns are of type character, hence for the value to be used
#'   as numerical data, filtering all non-numeric data and conversion are
#'   still necessary.
#' @examples
#' dat <- c(paste0("2001-11-10; 11:00; arzt=OA Dr. Vorname Nachname; ",
#'                 "Anaesthesist=Dr. Vorname Nachname; ",
#'                 "Instrumentarin=DGKS Vorname Nachname, ",
#'                 "DGKP Vorname Nachname; what=some (long) text with ",
#'                 "semicolons; and other stuff / like slashes, commas, ",
#'                  "question ? marks, etc.;"),
#'          "2001-10-11; 19:40; weight=92.8kg;",
#'          "2001-11-12; 19:30; weight=93.1kg;",
#'          paste0("2001-11-13; 10:00; what=zustand; dauer=5d, ",
#'                 "leicht kraenklich, husten, schnupfen (!), ",
#'                  "leichte temperatur (37.3)"))
#' get_data_long(dat)
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @export
#'
get_data_long <- function(dat_txt, sep = ";") {
  ## tokenize data on delimiter, and trim:
  dat_token <- stringr::str_split(dat_txt, pattern = sep) %>%
    purrr::map(stringr::str_trim)

  ## remove non-key tokens (timestamp, empty):
  dat_token %<>%
    purrr::map(~ stringr::str_trim(.x[-c(1, 2)])) %>%
    purrr::map(~ subset(.x, .x!= ""))

  ## get timestamp for each token:
  tst <- get_timestamp(dat_txt)

  token_length <- purrr::map_int(dat_token, length)

  ## split strings at "=" sign (if there is one):
  dat_token_split <- purrr::map(
    dat_token, ~ stringr::str_split_fixed(.x, "=", n = 2) ## split
  )

  ## get keys:
  key_list <- dat_token_split %>%
    purrr::map(  ## get key: first entry if second is not "". NA if there is no key.
      ~ ifelse(.x[, 2] == "", NA, .x[, 1])
    ) %>%
    purrr::map(~ stringr::str_trim(.x))

  ## get values:
  value_list <- dat_token_split %>%
    purrr::map(  ## get value: second entry if second is not "". NA if there is no key.
      ~ ifelse(.x[, 2] == "", .x[, 1], .x[, 2])
    ) %>%
    purrr::map(~ stringr::str_trim(.x))

  dat_ret <- tibble::tibble(
    "datetime" = rep(tst, times = token_length),
    "key" = unlist(key_list),
    "value" = unlist(value_list)
  )
  return(dat_ret)
}



#' Get character value for a specific key.
#'
#' The function will only return the first value with
#' that key for a specific date (i.e., multiple identical keys for the same
#' timestamp will be ignored.). If key is not found, an empty string will be
#' returned for that line of the data.
#'
#' @param dat_txt Character vector that contains key-value pairs.
#' @param key Character vector of length 1 that specifies the key to look for.
#'   It is assumed that the key is found at the beginning of a key-value
#'   pair (i.e., the key-text has to start with the text specified in the
#'   \code{key} parameter), as well as that it ends with and equals-sign "="
#'   (i.e., format of key-value pairs needs to be \code{"key = value"}).
#'   Can be a regular expression.
#' @param sep Character vector of length 1 that specifies which separator
#'   is used to separate the key-value pairs from each other. Needs to be a
#'   valid regular expression, i.e., if "|" is used, the separator needs to be
#'   "\\|". Default is ";".
#'
#' @return A character vector with as many elements as input elements in
#'   \code{dat_txt}, returning the values of the specified key or an
#'   empty string if that key is not found.
#'
#' @examples
#' dat <- c(paste0("2001-11-10; 11:00; arzt=OA Dr. Vorname Nachname; ",
#'                 "Anaesthesist=Dr. Vorname Nachname; ",
#'                 "Instrumentarin=DGKS Vorname Nachname, ",
#'                 "DGKP Vorname Nachname; what=some (long) text with ",
#'                 "semicolons; and other stuff / like slashes, commas, ",
#'                 "question ? marks, etc.;"),
#'          paste0("2001-10-11; 19:40; caliper = (brust-li: 15/13/16, ",
#'                 "brust-re: 18/14/18, bauch-li: 28/23/25, ",
#'                 "bauch-re: 29/24/24, ",
#'                 "bein-li: 14/12/12, bein-re: 19/20/19);"),
#'          "2001-11-12; 19:30; weight=93.1kg; note = some note here;",
#'          "2001-11-13; 08:00; event = Ende Urlaub",
#'          paste0("2001-11-13; 10:00; what=zustand; dauer=5d, ",
#'                 "leicht kraenklich, husten, schnupfen (!), ",
#'                 "leichte temperatur (37.3)"))
#' get_value_text(dat, key = "caliper")
#' get_value_text(dat, key = "weight")
#' get_value_text(dat, key = "event")
#' get_value_text(dat, key = "note")
#' get_value_text(dat, key = "nonexisiting")
#' @importFrom magrittr %>%
#' @export
#'
get_value_text <- function(dat_txt, key, sep = ";") {
  ## tokenize data on delimiter, and trim:
  dat_token <- stringr::str_split(dat_txt, pattern = sep) %>%
    purrr::map(stringr::str_trim)

  ## adjust key pattern (token needs to start with key,
  ## as well as only have spaces and and an equals sign after the key):
  key_pattern <- paste0("^", key, "[[:space:]]*=")

  ## get token corresponding to key and get all first entries into a
  ## character vector (ignoring multiple identical keys for the same
  ## timestamp):
  dat_keys <- purrr::map(
    dat_token, ~ stringr::str_subset(.x, pattern = key_pattern)
    ) %>%
    purrr::map_chr(~ .x[1])

  ## remove keys (i.e., use only values) and trim
  value_txt <- stringr::str_split_fixed(
    dat_keys, pattern = "=", n = 2) %>%
    subset(select = 2) %>%
    stringr::str_trim()

  return(value_txt)
}



#' Get numeric value for a specific key
#'
#' The function will only return the first value with
#' that key for a specific date (i.e., multiple identical keys for the same
#' timestamp will be ignored.). If key is not found, NA will be returned
#' for that line of the data.
#'
#' @inheritParams get_value_text
#'
#' @return A numeric vector with as many elements as input elements in
#'   \code{dat_txt}, returning the values of the specified key or \code{NA}
#'   if that key is not found or the character value of that key cannot be
#'   converted to a numeric data type.
#' @examples
#' dat <- c(paste0("2001-11-10; 11:00; arzt=OA Dr. Vorname Nachname; ",
#'                 "Anaesthesist=Dr. Vorname Nachname; ",
#'                 "Instrumentarin=DGKS Vorname Nachname, ",
#'                 "DGKP Vorname Nachname; what=some (long) text with ",
#'                 "semicolons; and other stuff / like slashes, commas, ",
#'                 "question ? marks, etc.;"),
#'          paste0("2001-10-11; 19:40; caliper = (brust-li: 15/13/16, ",
#'                 "brust-re: 18/14/18, bauch-li: 28/23/25, ",
#'                 "bauch-re: 29/24/24, ",
#'                 "bein-li: 14/12/12, bein-re: 19/20/19);"),
#'          "2001-11-12; 19:30; weight=93.1kg; note = some note here;",
#'          "2001-11-13; 08:00; event = Ende Urlaub",
#'          paste0("2001-11-13; 10:00; what=zustand; dauer=5d, ",
#'                 "leicht kraenklich, husten, schnupfen (!), ",
#'                 "leichte temperatur (37.3)"))
#' get_value_num(dat, key = "weight")
#' @export
get_value_num <- function(dat_txt, key, sep = ";") {
  value_txt <- get_value_text(dat_txt, key, sep)
  ## remove all non-numeric characters at the end (probably units):
  value_txt <- stringr::str_replace(value_txt, "[^0-9]+$", "")
  value_num <- suppressWarnings(as.numeric(value_txt))
  return(value_num)
}



#' Extract (mean) values for sub-keys.
#'
#' Takes the (vector of) values that contain subkey-value-pairs
#' of the (default) form:
#' \code{(subkey1: val1/val2/val3, subkey2: val1/val2/val3)}
#' and extracts the mean value for a specified subkey.
#'
#' @param value The value extracted from the original key-value pair.
#' @param subkey The subkey for which the data should be extracted.
#' @param key_sep The separator that separates the subkey-value-pairs
#'   from each other. Needs to be a valid regular expression (e.g.,
#'   '|' needs to be escaped as '\\|')
#' @param keyvalue_sep The separator that separates the subkey from the value(s)
#'   of that subkey. Needs to be a valid regular expression (e.g.,
#'   '|' needs to be escaped as '\\|')
#' @param vec_sep The separator that separates the individual values for each
#'   subkey value. Needs to be a valid regular expression (e.g.,
#'   '|' needs to be escaped as '\\|')
#' @param ... parameters passed to \code{mean} function.
#'
#' @return A mean value for all values of the subkey, or \code{NA}.
#' @export
#'
#' @examples
#' #value <- get_value_text(dat_txt, key = "caliper")
#' #value <- value[357:360]
#' #subkey <- "brust-li"
#' dat <- c(paste0("2018-03-23; 20:30; caliper = (brust-li: 14/12/11, ",
#'                 "brust-re: 12/13/13, bauch-li: 25/25/25, ",
#'                 "bauch-re: 26/26/25, bein-li: 15/15/15, ",
#'                 "bein-re: 24/23/26);"),
#'          paste0("2018-03-29; 19:00; weight = 90.1kg;", "2018-03-30; 21:00; ",
#'                 "weight = 89.3kg; note = nach Laufen;"),
#'          paste0("2018-03-30; 21:00; caliper = (brust-li: 12/13/12, ",
#'                 "brust-re: 12/13/13, bauch-li: 28/29/29, bauch-re: 24/21/28, ",
#'                 "bein-li: 14/16/14, bein-re: 22/22/21);"))
#' get_subkey_value_mean(
#'   get_value_text(dat, key = "caliper"),
#'   subkey = "brust-re")
get_subkey_value_mean <- function(value, subkey, key_sep = ",",
                                  keyvalue_sep = ":", vec_sep = "/", ...) {
  ## remove parenthesis:
  keyvaluepairs <- stringr::str_replace_all(value, "^\\(|\\)$", "")
  ## split into key-value pairs and trim key-value pair strings:
  value_split <- stringr::str_split(keyvaluepairs, key_sep)
  value_split <- purrr::map(value_split, stringr::str_trim)

  ## split into a list of matrices ["key, "value"], then trim again:
  key_split <- purrr::map(value_split, ~stringr::str_split_fixed(.x, keyvalue_sep, n = 2))
  key_split <- purrr::map(key_split, ~ apply(.x, 1:2, stringr::str_trim))

  ## extract the value for specified subkey, replacing "character(0)" with NA:
  value_vec <- purrr::map(key_split, ~ .x[which(.x[,1] == subkey), 2])
  value_vec <- purrr::map(value_vec, ~ ifelse(length(.x) == 0, NA, .x))

  ## split into individual values, convert to numeric:
  value_vec_split <- stringr::str_split(value_vec, vec_sep)
  value_vec_split <- purrr::map(value_vec_split, ~ ifelse(.x == "NA", NA, .x))
  value_vec_split_num <- purrr::map(value_vec_split, as.numeric)

  ## calulate mean:
  ret <- purrr::map_dbl(value_vec_split_num, mean, ...)

  ## convert NaN's to NA's (as mean(c(NA)) returns NaN, which is misleading):
  ret[is.nan(ret)] <- NA
  return(ret)
}


#' Calculate body fat from caliper measurements.
#'
#' Using the 3-Falten-Formel by Jackson & Pollock, for men.
#'
#' @param age Age of the person that the caliper measurements were taken of
#'   (at the time of taking the measurements).
#' @param ... parameters passed to \code{calc_bodyfat_mean} and in turn to
#'   \code{mean} function.
#' @inheritParams get_subkey_value_mean
#'
#' @references
#' \url{https://de.wikipedia.org/wiki/Calipometrie}
#' \url{Jackson, Pollock: Generalized equations for predicting body density of women. In: British Journal of Nutrition. Nr.40, Oktober 1978, S.497–504 (englisch)}
#'
#' @return
#' @export
#'
#' @examples
calc_bodyfat <- function(value, age, key_sep = ",",
                        keyvalue_sep = ":", vec_sep = "/", ...) {
  k0 <- 1.10938
  k1 <- 0.0008267
  k2 <- 0.0000016
  ka <- 0.0002574
  d_brust <- (
    get_subkey_value_mean(value, subkey = "brust-li", key_sep = key_sep, keyvalue_sep = keyvalue_sep, vec_sep = vec_sep, ...) +
      get_subkey_value_mean(value, subkey = "brust-re", key_sep = key_sep, keyvalue_sep = keyvalue_sep, vec_sep = vec_sep, ...)
  ) / 2
  d_bauch <- (
    get_subkey_value_mean(value, subkey = "bauch-li", key_sep = key_sep, keyvalue_sep = keyvalue_sep, vec_sep = vec_sep, ...) +
      get_subkey_value_mean(value, subkey = "bauch-re", key_sep = key_sep, keyvalue_sep = keyvalue_sep, vec_sep = vec_sep, ...)
  ) / 2
  d_oberschenkel <- (
    get_subkey_value_mean(value, subkey = "bein-li", key_sep = key_sep, keyvalue_sep = keyvalue_sep, vec_sep = vec_sep, ...) +
      get_subkey_value_mean(value, subkey = "bein-re", key_sep = key_sep, keyvalue_sep = keyvalue_sep, vec_sep = vec_sep, ...)
  ) / 2
  s <- d_brust + d_bauch + d_oberschenkel
  bodyfat <- (( 4.95 / ( k0 - ( k1 * s ) + ( k2 * s^2 ) - ( ka * age ))) - 4.5 ) * 100
  return(bodyfat)
}


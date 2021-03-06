context("test-all")

## ========================================================================= ##
## global variables for tests ####
## ========================================================================= ##

dat_01 <- c(paste0("2001-11-10; 11:00; arzt=OA Dr. Vorname Nachname; ",
                "Anaesthesist=Dr. Vorname Nachname; ",
                "Instrumentarin=DGKS Vorname Nachname, ",
                "DGKP Vorname Nachname; what=some (long) text with ",
                "semicolons; and other stuff / like slashes, commas, ",
                "question ? marks, etc.;"),
            "2001-10-11; 19:40; weight=92.8kg;",
            "2001-11-12; 19:30; weight=93.1kg;",
            "2001-11-12; 19:31; weight=93.1;",
            "2001-11-12; 19:32; weight=93.1",
            paste0("2001-11-13; 10:00; what=zustand; dauer=5d, ",
                   "leicht kraenklich, husten, schnupfen (!), ",
                   "leichte temperatur (37.3)"))

dat_02 <- c(paste0("2001-11-10; 11:00; arzt=OA Dr. Vorname Nachname; ",
                   "Anaesthesist=Dr. Vorname Nachname; ",
                   "Instrumentarin=DGKS Vorname Nachname, ",
                   "DGKP Vorname Nachname; what=some (long) text with ",
                   "semicolons; and other stuff / like slashes, commas, ",
                   "question ? marks, etc.;"),
            paste0("2001-10-11; 19:40; caliper = (brust-li: 15/13/16, ",
                   "brust-re: 18/14/18, bauch-li: 28/23/25, ",
                   "bauch-re: 29/24/24, ",
                   "bein-li: 14/12/12, bein-re: 19/20/19);"),
            "2001-11-12; 19:30; weight=93.1kg; note = some note here;",
            "2001-11-13; 08:00; event = Ende Urlaub",
            paste0("2001-11-13; 10:00; what=zustand; dauer=5d, ",
                   "leicht kraenklich, husten, schnupfen (!), ",
                   "leichte temperatur (37.3)"))

dat_delim <- c("2001-11-12 | 19:30 | weight=93.1kg | note = some note here",
               "2001-11-12; 19:30;  weight=93.1kg | note = some note here",
               "2001-11-13; 08:00; event = Ende Urlaub")

## ========================================================================= ##
## get_timestamp() ####
## ========================================================================= ##

test_that("get_timestamp() works for input vector of lenght 1", {
  expect_equal(length(get_timestamp("2018-08-24; 17:40;")),
                         1)
  expect_equal(get_timestamp("2018-08-24; 17:40"),
                         as.Date("2018-08-24 17:40:00"))
  expect_equal(get_timestamp("2018-08-24; 17:40; some more stuff"),
                         as.Date("2018-08-24 17:40:00"))
})

test_that("get_timestamp function works for vectorized inputs", {
  expect_equal(
    length(get_timestamp(c("2018-08-24; 17:40;", "2018-08-24; 17:40;"))),
    2)
  expect_equal(
    get_timestamp(c("2018-08-24; 17:40;", "2018-09-25; 18:51;")),
    c(as.Date("2018-08-24 17:40:00"), as.Date("2018-09-25 18:51:00")))
})


## ========================================================================= ##
## get_data_long() ####
## ========================================================================= ##


test_that("get_data_long() returns correct long data structure", {
  expect_equal(dim(get_data_long(dat_01)), c(11, 3))
  expect_equal(as.vector(get_data_long(dat_01)[["key"]]),
                         c("arzt", "Anaesthesist", "Instrumentarin", "what", NA,
                           "weight", "weight", "weight", "weight",
                           "what", "dauer"))
  expect_equal(get_data_long(dat_01)["key"],
                         tibble::tibble("key" = c("arzt", "Anaesthesist",
                                          "Instrumentarin", "what", NA,
                                          "weight", "weight", "weight",
                                          "weight", "what", "dauer")))
  expect_equal(
    get_data_long(dat_01)["value"],
    tibble::tibble("value" = c(
      "OA Dr. Vorname Nachname",
      "Dr. Vorname Nachname",
      "DGKS Vorname Nachname, DGKP Vorname Nachname",
      "some (long) text with semicolons",
      "and other stuff / like slashes, commas, question ? marks, etc.",
      "92.8kg", "93.1kg", "93.1", "93.1", "zustand",
      paste0("5d, leicht kraenklich, husten, schnupfen (!), ",
             "leichte temperatur (37.3)"))))
  expect_equal(get_data_long(dat_01)["datetime"],
                         tibble::tibble(
                           "datetime" = as.Date(
                             rep(c("2001-11-10 11:00:00",
                                   "2001-10-11 19:40:00",
                                   "2001-11-12 19:30:00",
                                   "2001-11-12 19:31:00",
                                   "2001-11-12 19:32:00",
                                   "2001-11-13 10:00:00"),
                                 times = c(5, 1, 1, 1, 1, 2)))))
})

## ========================================================================= ##
## get_value_text()
## ========================================================================= ##

test_that("get_value_text() finds key correctly", {
  expect_equal(get_value_text(dat_02, key = "weight"),
                         c("", "", "93.1kg", "",""))
  expect_equal(get_value_text(dat_02, key = "caliper"),
                         c("", "(brust-li: 15/13/16, brust-re: 18/14/18, bauch-li: 28/23/25, bauch-re: 29/24/24, bein-li: 14/12/12, bein-re: 19/20/19)", "", "", ""))
  expect_equal(get_value_text(dat_02, key = "event"),
                         c("", "", "", "Ende Urlaub", ""))
  expect_equal(get_value_text(dat_02, key = "note"),
                         c("", "", "some note here", "", ""))
})

test_that("get_value_text() returns empty strings for nonexisting key", {
  expect_equal(get_value_text(dat_02, key = "nonexisting"),
                         c("", "", "", "", ""))
})

test_that("function get_value_text delimiter argument works as expected", {
  expect_equal(get_value_text(dat_delim, key = "weight", sep = "\\|"),
                         c("93.1kg", "", ""))
})


## ========================================================================= ##
## get_value_num() ####
## ========================================================================= ##

test_that("get_value_num() returns numeric value correctly", {
  expect_equal(get_value_num(dat_01, key = "weight"),
                         c(NA, 92.8, 93.1, 93.1, 93.1, NA))
})

test_that("get_value_num() returns NA (type double) for nonexisting key", {
  expect_equal(get_value_num(dat_02, key = "nonexisting"),
                         c(rep(as.double(NA), 5)))
})

test_that("get_value_num delimiter argument works as expected", {
  expect_equal(get_value_num(dat_delim, key = "weight", sep = "\\|"),
               c(93.1, NA, NA))
})

## ========================================================================= ##
## get_subkey_value_mean()
## ========================================================================= ##

## dat02:
# paste0("2001-10-11; 19:40; caliper = (brust-li: 15/13/16, ",
#        "brust-re: 18/14/18, bauch-li: 28/23/25, ",
#        "bauch-re: 29/24/24, ",
#        "bein-li: 14/12/12, bein-re: 19/20/19);"),

test_that("get_subkey_value_mean() returns mean correctly", {
  expect_equal(get_subkey_value_mean(get_value_text(dat_02, key = "caliper"),
                                     subkey = "brust-li"),
               c(NA, mean(c(15,13,16)), NA, NA, NA))
  expect_equal(get_subkey_value_mean(get_value_text(dat_02, key = "caliper"),
                                     subkey = "brust-re"),
               c(NA, mean(c(18,14,18)), NA, NA, NA))
  expect_equal(get_subkey_value_mean(get_value_text(dat_02, key = "caliper"),
                                     subkey = "bauch-li"),
               c(NA, mean(c(28, 23, 25)), NA, NA, NA))
  expect_equal(get_subkey_value_mean(get_value_text(dat_02, key = "caliper"),
                                     subkey = "bauch-re"),
               c(NA, mean(c(29, 24, 24)), NA, NA, NA))
  expect_equal(get_subkey_value_mean(get_value_text(dat_02, key = "caliper"),
                                     subkey = "bein-li"),
               c(NA, mean(c(14, 12, 12)), NA, NA, NA))
  expect_equal(get_subkey_value_mean(get_value_text(dat_02, key = "caliper"),
                                     subkey = "bein-re"),
               c(NA, mean(c(19, 20, 19)), NA, NA, NA))

})

dat_delim_calip_01 <- c(paste0("2018-03-23; 20:30; caliper = (brust-li: 14/12/11, ",
                            "brust-re: 12/13/13, bauch-li: 25/25/25, ",
                            "bauch-re: 26/26/25, bein-li: 15/15/15, ",
                            "bein-re: 24/23/26);"),
                     paste0("2018-03-30 / 21:00 / ",
                            "weight = 89.3kg / note = nach Laufen;"),
                     paste0("2018-03-30; 21:00; caliper = (brust-li @ 12/13/12, ",
                            "brust-re @ 12/13/13, bauch-li @ 28/29/29, bauch-re @ 24/21/28, ",
                            "bein-li @ 14/16/14, bein-re @ 22/22/21);"),
                     paste0("2018-03-30; 21:00; caliper = (brust-li @ 12/13/12 ! ",
                            "brust-re @ 12/13/13 ! bauch-li @ 28/29/29 ! bauch-re @ 24/21/28 ! ",
                            "bein-li @ 14/16/14 ! bein-re @ 22/22/21);"),
                     paste0("2018-03-30; 21:00; caliper = (brust-li @ 12|13|12, ",
                            "brust-re @ 12|13|13, bauch-li @ 28|29|29, bauch-re @ 24|21|28, ",
                            "bein-li @ 14|16|14, bein-re @ 22|22|21);"))

test_that("get_subkey_value_mean() works with different delimiters (single char)", {
  expect_equal(
    get_subkey_value_mean(
      get_value_text(dat_delim_calip_01, key = "caliper"), subkey = "brust-li",
      key_sep = ",", keyvalue_sep = ":", vec_sep = "/"),
    c(mean(c(14,12,11)), NA, NA, NA, NA)
  )

  expect_warning(
    val <- get_subkey_value_mean(
      get_value_text(dat_delim_calip_01, key = "caliper"), subkey = "brust-li",
      key_sep = ",", keyvalue_sep = "@", vec_sep = "/"),
    "NAs introduced by coercion"
  )
  expect_equal(val, c(NA, NA, mean(c(12,13,12)), NA, NA))

  expect_warning(
    val <- get_subkey_value_mean(
      get_value_text(dat_delim_calip_01, key = "caliper"), subkey = "brust-li",
      key_sep = "!", keyvalue_sep = "@", vec_sep = "/"),
    "NAs introduced by coercion"
  )
  expect_equal(val, c(NA, NA, NA, mean(c(12,13,12)), NA))


  expect_warning(
    val <- get_subkey_value_mean(
      get_value_text(dat_delim_calip_01, key = "caliper"), subkey = "brust-li",
      key_sep = ",", keyvalue_sep = "@", vec_sep = "\\|"),
    "NAs introduced by coercion"
  )
  expect_equal(val, c(NA, NA, NA, NA, mean(c(12,13,12)))
  )
})

## ----------------------------------------------------------------------- ##
## get_subkey_value_mean():: multi-char tests (in smaller chunks)
## ----------------------------------------------------------------------- ##

# dat_delim_calip_tmp <- c(paste0("2018-03-23; 20:30; note = line just to test vectorization"),
#                         paste0("2018-03-23; 20:30; caliper = ",
#                                "(brust-li: 14/12/11, brust-re: 12/13/13, ",
#                                " bauch-li: 25/25/25, bauch-re: 26/26/25, ",
#                                " bein-li:  15/15/15, bein-re:  24/23/26);"))

test_that("get_subkey_value_mean(): key_sep delimiter works (multi char)", {
  dat_delim_calip_02 <- c(paste0("2018-03-23; 20:30; note = line just to test vectorization"),
                          paste0("2018-03-23; 20:30; caliper = ",
                                 "(brust-li: 14/12/11 --- brust-re: 12/13/13 --- ",
                                 " bauch-li: 25/25/25 --- bauch-re: 26/26/25 --- ",
                                 " bein-li:  15/15/15 --- bein-re:  24/23/26);"))
  expect_equal(
    get_subkey_value_mean(
      get_value_text(dat_delim_calip_02, key = "caliper"), subkey = "brust-li",
      key_sep = "---", keyvalue_sep = ":", vec_sep = "/"),
    c(NA, mean(c(12,13,12)))
  )

  dat_delim_calip_03 <- c(paste0("2018-03-23; 20:30; note = line just to test vectorization"),
                           paste0("2018-03-23; 20:30; caliper = ",
                                  "(brust-li: 14/12/11,, brust-re: 12/13/13,, ",
                                  " bauch-li: 25/25/25,, bauch-re: 26/26/25,, ",
                                  " bein-li:  15/15/15,, bein-re:  24/23/26);"))
  expect_equal(
    get_subkey_value_mean(
      get_value_text(dat_delim_calip_03, key = "caliper"), subkey = "brust-li",
      key_sep = ",,", keyvalue_sep = ":", vec_sep = "/"),
    c(NA, mean(c(12,13,12)))
  )
})


test_that("get_subkey_value_mean(): keyvalue_sep delimiter works (multi char)", {
  dat_delim_calip_04 <- c(paste0("2018-03-23; 20:30; note = line just to test vectorization"),
                          paste0("2018-03-23; 20:30; caliper = ",
                                 "(brust-li ~~ 14/12/11, brust-re ~~ 12/13/13, ",
                                 " bauch-li ~~ 25/25/25, bauch-re ~~ 26/26/25, ",
                                 " bein-li ~~  15/15/15, bein-re ~~  24/23/26);"))
  expect_equal(
    get_subkey_value_mean(
      get_value_text(dat_delim_calip_04, key = "caliper"), subkey = "brust-li",
      key_sep = ",", keyvalue_sep = "~~", vec_sep = "/"),
    c(NA, mean(c(12,13,12)))
  )

  dat_delim_calip_05 <- c(paste0("2018-03-23; 20:30; note = line just to test vectorization"),
                          paste0("2018-03-23; 20:30; caliper = ",
                                 "(brust-li ||| 14/12/11, brust-re ||| 12/13/13, ",
                                 " bauch-li ||| 25/25/25, bauch-re ||| 26/26/25, ",
                                 " bein-li |||  15/15/15, bein-re |||  24/23/26);"))
  expect_equal(
    get_subkey_value_mean(
      get_value_text(dat_delim_calip_05, key = "caliper"), subkey = "brust-li",
      key_sep = ",", keyvalue_sep = "\\|\\|\\|", vec_sep = "/"),
    c(NA, mean(c(12,13,12)))
  )
})

test_that("get_subkey_value_mean(): vec_sep delimiter works (multi char)", {
  dat_delim_calip_06 <- c(paste0("2018-03-23; 20:30; note = line just to test vectorization"),
                           paste0("2018-03-23; 20:30; caliper = ",
                                  "(brust-li: 14//12//11, brust-re: 12//13//13, ",
                                  " bauch-li: 25//25//25, bauch-re: 26//26//25, ",
                                  " bein-li:  15//15//15, bein-re:  24//23//26);"))
  expect_equal(
    get_subkey_value_mean(
      get_value_text(dat_delim_calip_06, key = "caliper"), subkey = "brust-li",
      key_sep = ",", keyvalue_sep = ":", vec_sep = "//"),
    c(NA, mean(c(12,13,12)))
  )

  dat_delim_calip_07 <- c(paste0("2018-03-23; 20:30; note = line just to test vectorization"),
                           paste0("2018-03-23; 20:30; caliper = ",
                                  "(brust-li: 14||12||11, brust-re: 12||13||13, ",
                                  " bauch-li: 25||25||25, bauch-re: 26||26||25, ",
                                  " bein-li:  15||15||15, bein-re:  24||23||26);"))
  expect_equal(
    get_subkey_value_mean(
      get_value_text(dat_delim_calip_07, key = "caliper"), subkey = "brust-li",
      key_sep = ",", keyvalue_sep = ":", vec_sep = "\\|\\|"),
    c(NA, mean(c(12,13,12)))
  )
})

dat_bodyfat_tmp <- c(paste0("2018-03-23; 20:30; note = line just to test vectorization"),
                     paste0("2018-03-23; 20:30; caliper = ",
                            "(brust-li: 14/12/11, brust-re: 12/13/one-missing, ",
                            " bauch-li: 25/25/25, bauch-re: 26/26/25, ",
                            " bein-li:  15/15/15, bein-re:  24/23/26);"),
                     paste0("2018-03-23; 20:30; caliper = ",
                            "(brust-li: 20/20/20, brust-re: 20/two/missing, ",
                            " bauch-li: 30/30/30, bauch-re: 30/30/30, ",
                            " bein-li:  20/20/20, bein-re:  20/20/20);"),
                     paste0("2018-03-23; 20:30; caliper = ",
                            "(brust-li: 20/two/and, brust-re: 20/two/missing, ",
                            " bauch-li: 30/30/30, bauch-re: 30/30/30, ",
                            " bein-li:  20/20/20, bein-re:  20/20/20);"),
                     paste0("2018-03-23; 20:30; caliper = ",
                            "(brust-li: all-missing, brust-re: all-missing, ",
                            " bauch-li: 20/20/20, bauch-re: 20/20/20, ",
                            " bein-li:  10/10/10, bein-re:  10/10/10);"))

test_that("get_subkey_value_mean() handles missing values correctly", {
  expect_warning(
    val <- get_subkey_value_mean(
      get_value_text(dat_bodyfat_tmp, key = "caliper"), subkey = "brust-re",
      key_sep = ",", keyvalue_sep = ":", vec_sep = "/", na.rm = TRUE),
    "NAs introduced by coercion")
  expect_equal(val, c(NA, mean(c(12,13)), 20, 20, NA))

  expect_warning(
    val <- get_subkey_value_mean(
      get_value_text(dat_bodyfat_tmp, key = "caliper"), subkey = "brust-re",
      key_sep = ",", keyvalue_sep = ":", vec_sep = "/", na.rm = FALSE),
    "NAs introduced by coercion")
  expect_equal(val, as.double(c(NA, NA, NA, NA, NA)))
})

## ========================================================================= ##
## calc_bodyfat()
## ========================================================================= ##

# dat_bodyfat_01 <- c(paste0("2018-03-23; 20:30; note = line just to test vectorization"),
#                     paste0("2018-03-23; 20:30; caliper = ",
#                            "(brust-li: 14/12/11, brust-re: 12/13/13, ",
#                            " bauch-li: 25/25/25, bauch-re: 26/26/25, ",
#                            " bein-li:  15/15/15, bein-re:  24/23/26);"))

dat_bodyfat_tmp <- c(paste0("2018-03-23; 20:30; note = line just to test vectorization"),
                     paste0("2018-03-23; 20:30; caliper = ",
                            "(brust-li: 14/12/11, brust-re: 12/13/13, ",
                            " bauch-li: 25/25/25, bauch-re: 26/26/25, ",
                            " bein-li:  15/15/15, bein-re:  24/23/26);"),
                     paste0("2018-03-23; 20:30; caliper = ",
                            "(brust-li: 20/20/20, brust-re: 20/20/20, ",
                            " bauch-li: 30/30/30, bauch-re: 30/30/30, ",
                            " bein-li:  20/20/20, bein-re:  20/20/20);"),
                     paste0("2018-03-23; 20:30; caliper = ",
                            "(brust-li: 10/10/10, brust-re: 10/10/10/10/10, ",
                            " bauch-li: 20/20/20, bauch-re: 20/20, ",
                            " bein-li:  10/10/10, bein-re:  10);"))
value_bodyfat_01 <- get_value_text(dat_bodyfat_tmp, key = "caliper", sep = ";")

test_that("calc_bodyfat() returns correct bodyfat values", {
  expect_equal(calc_bodyfat(value_bodyfat_01, age = 39, key_sep = ",",
                            keyvalue_sep = ":", vec_sep = "/"),
               c(NA, 18.26393228, 21.73749209, 13.12175499))
})
## checked with: http://www.got-big.de/Blog/koerperfettrechner/



dat_bodyfat_tmp <- c(paste0("2018-03-23; 20:30; note = line just to test vectorization"),
                     paste0("2018-03-23; 20:30; caliper = ",
                            "(brust-li: 14/12/11, brust-re: 12/13/one-missing, ",
                            " bauch-li: 25/25/25, bauch-re: 26/26/25, ",
                            " bein-li:  15/15/15, bein-re:  24/23/26);"),
                     paste0("2018-03-23; 20:30; caliper = ",
                            "(brust-li: 20/20/20, brust-re: 20/two/missing, ",
                            " bauch-li: 30/30/30, bauch-re: 30/30/30, ",
                            " bein-li:  20/20/20, bein-re:  20/20/20);"),
                     paste0("2018-03-23; 20:30; caliper = ",
                            "(brust-li: 20/two/and, brust-re: 20/two/missing, ",
                            " bauch-li: 30/30/30, bauch-re: 30/30/30, ",
                            " bein-li:  20/20/20, bein-re:  20/20/20);"),
                     paste0("2018-03-23; 20:30; caliper = ",
                            "(brust-li: all-missing, brust-re: all-missing, ",
                            " bauch-li: 20/20/20, bauch-re: 20/20/20, ",
                            " bein-li:  10/10/10, bein-re:  10/10/10);"))
value_bodyfat_02 <- get_value_text(dat_bodyfat_tmp, key = "caliper", sep = ";")

test_that("calc_bodyfat() handles missing values correctly", {
  expect_warning(
    val <- calc_bodyfat(value_bodyfat_02, age = 39, key_sep = ",",
                 keyvalue_sep = ":", vec_sep = "/", na.rm = TRUE),
    "NAs introduced by coercion")
  expect_equal(val, c(NA, 18.24020372, 21.73749209, 21.73749209, NA))

  expect_warning(
    val <- calc_bodyfat(value_bodyfat_02, age = 39, key_sep = ",",
                 keyvalue_sep = ":", vec_sep = "/", na.rm = FALSE),
    "NAs introduced by coercion")
  expect_equal(val, as.double(c(NA, NA, NA, NA, NA)))
})




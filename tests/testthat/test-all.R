context("test-all")



testthat::test_that("get_timestamp function works for input vector of lenght 1", {
  testthat::expect_equal(length(get_timestamp("2018-08-24; 17:40;")), 1)
  testthat::expect_equal(get_timestamp("2018-08-24; 17:40"), as.Date("2018-08-24 17:40:00"))
  testthat::expect_equal(get_timestamp("2018-08-24; 17:40; some more stuff"), as.Date("2018-08-24 17:40:00"))
})
testthat::test_that("get_timestamp function works for vectorized inputs", {
  testthat::expect_equal(length(get_timestamp(c("2018-08-24; 17:40;", "2018-08-24; 17:40;"))), 2)
  testthat::expect_equal(get_timestamp(c("2018-08-24; 17:40;", "2018-09-25; 18:51;")),
                         c(as.Date("2018-08-24 17:40:00"), as.Date("2018-09-25 18:51:00")))
})


testthat::test_that("get_data_long function returns correct long data structure", {
  dat <- c("2001-11-10; 11:00; arzt=OA Dr. Vorname Nachname; Anaesthesist=Dr. Vorname Nachname; Instrumentarin=DGKS Vorname Nachname, DGKP Vorname Nachname; what=some (long) text with semicolons; and other stuff / like slashes, commas, question ? marks, etc.;",
           "2001-10-11; 19:40; weight=92.8kg;",
           "2001-11-12; 19:30; weight=93.1kg;",
           "2001-11-13; 10:00; what=zustand; dauer=5d, leicht kraenklich, husten, schnupfen (!), leichte temperatur (37.3)")
  testthat::expect_equal(dim(get_data_long(dat)), c(9, 3))
  testthat::expect_equal(as.vector(get_data_long(dat)[["key"]]),
                         c("arzt", "Anaesthesist", "Instrumentarin", "what", NA,
                           "weight", "weight", "what", "dauer"))
  testthat::expect_equal(get_data_long(dat)["key"],
                         tibble::tibble("key" = c("arzt", "Anaesthesist",
                                          "Instrumentarin", "what", NA,
                                          "weight", "weight", "what", "dauer")))
  testthat::expect_equal(get_data_long(dat)["value"],
                         tibble::tibble("value" = c("OA Dr. Vorname Nachname",
                                            "Dr. Vorname Nachname",
                                            "DGKS Vorname Nachname, DGKP Vorname Nachname",
                                            "some (long) text with semicolons",
                                            "and other stuff / like slashes, commas, question ? marks, etc.",
                                            "92.8kg", "93.1kg", "zustand",
                                            "5d, leicht kraenklich, husten, schnupfen (!), leichte temperatur (37.3)")))
  testthat::expect_equal(get_data_long(dat)["datetime"],
                         tibble::tibble("datetime" = as.Date(rep(c("2001-11-10 11:00:00",
                                                           "2001-10-11 19:40:00",
                                                           "2001-11-12 19:30:00",
                                                           "2001-11-13 10:00:00"),
                                                         times = c(5, 1, 1, 2)))))
})

testthat::test_that("function get_value_text finds key correctly", {
  dat <- c("2001-11-10; 11:00; arzt=OA Dr. Vorname Nachname; Anaesthesist=Dr. Vorname Nachname; Instrumentarin=DGKS Vorname Nachname, DGKP Vorname Nachname; what=some (long) text with semicolons; and other stuff / like slashes, commas, question ? marks, etc.;",
           "2001-10-11; 19:40; caliper = (brust-li: 15/13/16, brust-re: 18/14/18, bauch-li: 28/23/25, bauch-re: 29/24/24, bein-li: 14/12/12, bein-re: 19/20/19);",
           "2001-11-12; 19:30; weight=93.1kg; note = some note here;",
           "2001-11-13; 08:00; event = Ende Urlaub",
           "2001-11-15; 10:00; what=zustand; dauer=5d, leicht kraenklich, husten, schnupfen (!), leichte temperatur (37.3)")
  testthat::expect_equal(get_value_text(dat, key = "weight"), c("", "", "93.1kg", "",""))
  testthat::expect_equal(get_value_text(dat, key = "caliper"), c("", "(brust-li: 15/13/16, brust-re: 18/14/18, bauch-li: 28/23/25, bauch-re: 29/24/24, bein-li: 14/12/12, bein-re: 19/20/19)", "", "", ""))
  testthat::expect_equal(get_value_text(dat, key = "event"), c("", "", "", "Ende Urlaub", ""))
  testthat::expect_equal(get_value_text(dat, key = "note"), c("", "", "some note here", "", ""))
})
testthat::test_that("function get_value_text returns empty strings for nonexisting key", {
  dat <- c("2001-11-10; 11:00; arzt=OA Dr. Vorname Nachname; Anaesthesist=Dr. Vorname Nachname; Instrumentarin=DGKS Vorname Nachname, DGKP Vorname Nachname; what=some (long) text with semicolons; and other stuff / like slashes, commas, question ? marks, etc.;",
           "2001-10-11; 19:40; caliper = (brust-li: 15/13/16, brust-re: 18/14/18, bauch-li: 28/23/25, bauch-re: 29/24/24, bein-li: 14/12/12, bein-re: 19/20/19);",
           "2001-11-12; 19:30; weight=93.1kg; note = some note here;",
           "2001-11-13; 08:00; event = Ende Urlaub",
           "2001-11-15; 10:00; what=zustand; dauer=5d, leicht kraenklich, husten, schnupfen (!), leichte temperatur (37.3)")
  testthat::expect_equal(get_value_text(dat, key = "nonexisting"), c("", "", "", "", ""))
})
testthat::test_that("function get_value_text delimiter argument works as expected", {
  dat_delim <- c("2001-11-12 | 19:30 | weight=93.1kg | note = some note here",
                 "2001-11-12; 19:30;  weight=93.1kg | note = some note here",
                 "2001-11-13; 08:00; event = Ende Urlaub")
  testthat::expect_equal(get_value_text(dat_delim, key = "weight", sep = "\\|"),
                         c("93.1kg", "", ""))
})

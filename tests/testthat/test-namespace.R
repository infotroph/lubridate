context("Namespace")

eval_detached <- function(expr) {
  l_pos <- which(search() == "package:lubridate")
  m_pos <- which(search() == "package:methods")

  if (length(l_pos) > 0) {
    l_ns <- getNamespace("lubridate")
    on.exit(attachNamespace(l_ns, pos = l_pos))
    detach("package:lubridate", character.only = TRUE)
  }
  if (length(m_pos) > 0) {
    m_ns <- getNamespace("methods")
    on.exit(attachNamespace(m_ns, pos = m_pos), add = TRUE)
    detach("package:methods", character.only = TRUE)
  }

  eval(expr)
}

test_that("methods is not attached", { # confirming test assumptions
  expect_false(eval_detached('package:methods' %in% search()))
})

test_that("lubridate:: calls work when methods is not attached", {
  ts <- as.POSIXct('2017-10-03 03:01:13Z')
  expect_equal( # https://github.com/tidyverse/lubridate/issues/314
    eval_detached(lubridate::round_date(ts, 'minute')),
    round_date(ts, 'minute'))
  expect_equal( # https://github.com/tidyverse/lubridate/issues/407
    eval_detached(lubridate::days(1)),
    days(1))
  expect_equal( # https://github.com/tidyverse/lubridate/issues/499
    eval_detached(lubridate::dhours(22)),
    dhours(22))
})

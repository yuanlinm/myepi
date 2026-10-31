make_long_lung <- function() {
  wide <- survival::lung
  wide$id <- seq_len(nrow(wide))
  wide$event <- as.integer(wide$status == 2)
  first <- transform(wide, time1 = 0, time2 = time / 2, event = 0L)
  second <- transform(wide, time1 = time / 2, time2 = time)
  long <- rbind(first, second)
  long[order(long$id, long$time1), ]
}

test_that("cox_run deduplicates long-format people and cases", {
  long <- make_long_lung()
  n_people <- length(unique(long$id))
  n_cases <- length(unique(long$id[long$event == 1]))
  expected_incidence <- n_cases / (sum(long$time2 - long$time1) / 365.25) * 1e5

  result <- quiet(cox_run(
    long,
    time1 = "time1",
    time2 = "time2",
    event = "event",
    mainvar = "age",
    idL = "id"
  ))$result

  expect_equal(result$Case_Total, sprintf("%d/%d", n_cases, n_people))
  expect_equal(as.numeric(result$Incidence), expected_incidence, tolerance = 0.01)
})

test_that("cox_run keeps row-based behavior when idL is NULL", {
  long <- make_long_lung()
  n_cases <- sum(long$event == 1)

  result <- quiet(cox_run(
    long,
    time1 = "time1",
    time2 = "time2",
    event = "event",
    mainvar = "age"
  ))$result

  expect_equal(result$Case_Total, sprintf("%d/%d", n_cases, nrow(long)))
})

test_that("cox_run incidence is consistent between day and year units", {
  long_d <- make_long_lung()
  long_y <- long_d
  long_y$time1 <- long_y$time1 / 365.25
  long_y$time2 <- long_y$time2 / 365.25

  n_people <- length(unique(long_d$id))
  n_cases <- length(unique(long_d$id[long_d$event == 1]))
  expected_case_total <- sprintf("%d/%d", n_cases, n_people)

  result_d <- quiet(cox_run(
    long_d,
    time1 = "time1",
    time2 = "time2",
    event = "event",
    mainvar = "age",
    idL = "id",
    t_unit = "d"
  ))$result
  result_y <- quiet(cox_run(
    long_y,
    time1 = "time1",
    time2 = "time2",
    event = "event",
    mainvar = "age",
    idL = "id",
    t_unit = "y"
  ))$result

  expect_equal(result_d$Case_Total, expected_case_total)
  expect_equal(result_y$Case_Total, expected_case_total)
  expect_equal(as.numeric(result_d$Incidence), as.numeric(result_y$Incidence), tolerance = 0.01)
})

test_that("repeated event rows are counted once with idL", {
  data <- data.frame(
    id = c(1, 1, 2, 2, 3, 3, 4, 4),
    t1 = c(0, 5, 0, 5, 0, 5, 0, 5),
    t2 = c(5, 10, 5, 10, 5, 10, 5, 10),
    event = c(1, 1, 0, 0, 1, 1, 0, 1),
    x = c(1, 1, 2, 2, 3, 3, 4, 4)
  )

  result <- quiet(cox_run(data, time1 = "t1", time2 = "t2", event = "event", mainvar = "x", idL = "id"))$result
  expect_equal(result$Case_Total, "3/4")
})

test_that("cox_run_sub deduplicates within each subgroup", {
  long <- make_long_lung()
  result <- quiet(cox_run_sub(
    long,
    group_var = "sex",
    time1 = "time1",
    time2 = "time2",
    event = "event",
    mainvar = "age",
    idL = "id"
  ))$result

  expected <- lapply(sort(unique(long$sex)), function(level) {
    rows <- long$sex == level
    sprintf(
      "%d/%d",
      length(unique(long$id[rows & long$event == 1])),
      length(unique(long$id[rows]))
    )
  })
  names(expected) <- paste0("sex: ", sort(unique(long$sex)))
  expect_equal(unname(result$Case_Total), unname(unlist(expected[result$Subgroup])))
})

test_that("cox_run_q deduplicates within quantile groups", {
  long <- make_long_lung()
  result <- quiet(cox_run_q(
    long,
    mainvar = "age",
    q = 2,
    time1 = "time1",
    time2 = "time2",
    event = "event",
    idL = "id"
  ))$result
  totals <- as.integer(sub(".*/", "", result$Case_Total))

  expect_equal(sum(totals), length(unique(long$id)))
})

test_that("idL is validated", {
  long <- make_long_lung()
  expect_error(
    cox_run(
      long,
      time1 = "time1",
      time2 = "time2",
      event = "event",
      mainvar = "age",
      idL = "missing_id"
    ),
    "idL"
  )
})

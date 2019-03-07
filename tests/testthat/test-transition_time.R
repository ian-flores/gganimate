context('test-transition_time')

test_that('transition_time time parameters test', {

  p <- ggplot(airquality, aes(Day, Temp)) +
    geom_line(color = 'red', size = 1) +
    transition_time(Month)

  transition_list <- p$transition$params


  ## Testing time parameter
  time_unquoted <- get_expr(transition_list$time_quo)

  expect_equal(as.character(time_unquoted), 'Month')
  expect_true(typeof(time_unquoted) == 'symbol')
  expect_false(typeof(time_unquoted) == 'character')

  ## Testing range parameter
  time_range <- transition_list$range
  expect_null(time_range)
})

test_that('transition_time range parameters test', {

  p <- ggplot(airquality, aes(Day, Temp)) +
    geom_line(color = 'red', size = 1) +
    transition_time(Month, range = c(5, 7))

  transition_list <- p$transition$params

  ## Testing range parameter
  time_range <- transition_list$range
  expect_identical(time_range, c(5, 7))
})

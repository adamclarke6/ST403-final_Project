

test_that("general checks",{
expect_error(load_fx("dndjbdj"))
expect_no_error(exchange_rate("USD","EUR"))
expect_no_error(exchange_plot("USD","EUR"))
})

calculate_median <- function(a) {
  a <- sort(a)
  n <- length(a)
  m <- (n + 1) %/% 2
  if (n %% 2 == 1) {
    med <- a[m]
  } else {
    med <- mean(a[m:(m+1)])
  }
  return(med)
}


v2 <- c(0,1,2,2)

debugonce()

calculate_median(v2)


library("testthat")

# expect_equal --> approximate match 
# expect_identical
# L --> stands for integer values (type of numeric)
# 1L --> is my output length 1? 

testthat::test_that("The length of result is 1", {
  testthat:expect_equal(
    length(calculate_median(c(0, 1, 2, 2))),
    1L
  )
  testthat:expect_equal(
    length(calculate_median(c(1, 2, 3))), 
    1L
  )
  testthat::expect_equal(
    length(calculate_median((c(7.22, 1.54, 3.47, 2.75)))),
    1L
  )
}
                    
)


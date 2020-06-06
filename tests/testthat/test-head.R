test_that("head for iterator works", {
  database <- logician_database(
    connected(berlin, hamburg),
    connected(hamburg, chicago),
    connected(chicago, london),
    connected(aachen, berlin),
    connected(chicago, portland),
    connected(portland, munich),
    path(A, B) := connected(A, B),
    path(A, B) := connected(A, Z) && path(Z, B)
  )
  iter <- logician_query(database, path(berlin, X))
  res <- head(iter, 4)
  expect_equal(length(res), 4)

  iter <- logician_query(database, path(berlin, X))
  res <- head(iter, 1000)
  expect_equal(length(res), 5)

  iter <- logician_query(database, path(berlin, barcelona))
  res <- head(iter, 1000)
  expect_equal(length(res), 0)
})

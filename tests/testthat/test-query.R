test_that("simple query works", {
  database <- list(
    fact(clause("test", int(5), int(5))),
    fact(clause("test", int(5), int(4))),
    rule(clause("test2", variable("X"), int(5)), list(clause("test", variable("X"), int(5))))
  )

  iter <- logician_query_(database, clause("test", int(5), variable("X")))
  res <- iter$next_value()
  expect_true(res)
  expect_equal(substitutions(res)[[1]]$right, int(5))
  res <- iter$next_value()
  expect_true(res)
  expect_equal(substitutions(res)[[1]]$right, int(4))
  expect_false(iter$next_value())

  iter <- logician_query_(database, clause("test2", int(1), int(5)))
  expect_false(iter$next_value())

  iter <- logician_query_(database, clause("test2", variable("Y"), int(5)))
  res <- iter$next_value()
  expect_true(res)
  expect_equal(substitutions(res)[[1]]$right, int(5))
  expect_false(iter$next_value())

  iter <- logician_query_(database, clause("test2", int(4), int(5)))
  expect_false(iter$next_value())
})

test_that("another more complex example works", {
  # classic family example as here https://en.wikipedia.org/wiki/Prolog
  database <- list(
    fact(clause("mother_child", int(1), int(2))),
    fact(clause("father_child", int(3), int(2))),
    fact(clause("father_child", int(3), int(4))),
    fact(clause("father_child", int(5), int(3))),
    rule(
      clause("sibling", variable("A"), variable("B")),
      list(clause("parent_child", variable("VAR"), variable("A")), clause("parent_child", variable("VAR"), variable("B")))
    ),
    rule(clause("parent_child", variable("X"), variable("Y")), list(clause("father_child", variable("X"), variable("Y")))),
    rule(clause("parent_child", variable("X"), variable("Y")), list(clause("mother_child", variable("X"), variable("Y"))))
  )

  iter <- logician_query_(database, clause("sibling", int(2), int(4)))
  expect_true(iter$next_value())
  expect_false(iter$next_value())
  iter <- logician_query_(database, clause("parent_child", variable("Z"), variable("Y")))
  expect_true(iter$next_value())
  expect_true(iter$next_value())
  expect_true(iter$next_value())
  expect_true(iter$next_value())
  expect_false(iter$next_value())
  iter <- logician_query_(database, clause("parent_child", int(3), variable("Y")))
  expect_true(iter$next_value())
  expect_true(iter$next_value())
  expect_false(iter$next_value())
  iter <- logician_query_(database, clause("parent_child", int(3), variable("X")))
  expect_true(iter$next_value())
  expect_true(iter$next_value())
  expect_false(iter$next_value())
  iter <- logician_query_(database, clause("sibling", variable("X"), variable("Y")))
  expect_true(iter$next_value())
  iter <- logician_query_(database, clause("sibling", variable("A"), variable("B")))
  res <- iter$next_value()
  expect_true(res)
  expect_equal(length(substitutions(res)), 2)
})

test_that("another problem", {
  database <- list(
    fact(clause("a", int(5))),
    fact(clause("b", int(6))),
    rule(clause("test", variable("A"), variable("B")), list(clause("a", variable("A")), clause("b", variable("B"))))
  )
  iter <- logician_query_(database, clause("test", variable("X"), variable("Y")))
  res <- iter$next_value()
  subs <- substitutions(res)
  expect_equal(subs[[1]]$left@name, "X")
  expect_equal(subs[[1]]$right@value, 5)
  expect_equal(subs[[2]]$left@name, "Y")
  expect_equal(subs[[2]]$right@value, 6)
  expect_false(iter$next_value())
})

test_that("another example works", {
  database <- logician_database(
    connected(1, 2),
    connected(2, 3),
    connected(3, 4),
    connected(5, 1),
    connected(3, 6),
    connected(6, 7),
    path(A, B) := connected(A, B),
    path(A, B) := connected(A, Z) && path(Z, B)
  )
  iter <- logician_query(database, path(1, Z))
  expect_true(iter$next_value())
  expect_true(iter$next_value())
  expect_true(iter$next_value())
  expect_true(iter$next_value())
  expect_true(iter$next_value())
  expect_false(iter$next_value())
})

test_that("another datatype example works", {
  database <- logician_database(
    connected(1, berlin),
    connected(berlin, 3),
    connected(3, 4),
    connected("madrid", 1),
    connected(3, "barcelona"),
    connected("barcelona", hamburg),
    path(A, B) := connected(A, B),
    path(A, B) := connected(A, Z) && path(Z, B)
  )
  iter <- logician_query(database, path(1, Z))
  expect_true(iter$next_value())
  expect_true(iter$next_value())
  expect_true(iter$next_value())
  expect_true(iter$next_value())
  expect_true(iter$next_value())
  expect_false(iter$next_value())
})

test_that("we can call any r function that returns true/false", {
  database <- logician_database(
    number(1),
    number(2),
    number(3),
    number(4),
    number(5),
    sum(A, B) := number(A) && number(B) && r(A + B > 3),
    sideeffect(A, B) := number(A) && number(B) && r(A + 1 == B) && r(is.character(print(paste0(A, B))))
  )
  iter <- logician_query(database, sum(1, 3))
  expect_true(iter$next_value())
  iter <- logician_query(database, sum(1, A))
  expect_true(iter$next_value())
  iter <- logician_query(database, sideeffect(1, 2))
  expect_output(iter$next_value(), regexp = "12")
})

test_that("variables are renamed also in expressions", {
  database <- logician_database(
    test(1),
    test2(A) := test(A) && test(B) && r(A + B > 1 + pi * 0)
  )
  iter <- logician_query(database, test2(A))
  expect_true(iter$next_value())
})

test_that("long rules work", {
  database <- logician_database(
    test(1L),
    test(5L),
    test2(1L),
    test3(1L),
    test3(2L),
    test4(1L),
    test_rule(A, B, C, D, E) := test(A) && test2(B) && test3(C) && test4(D) && test2(D) && test(E)
  )
  iter <- logician_query(database, test_rule(A, B, C, D, E))
  expect_true(iter$next_value())
})

test_that("print query result", {
  database <- logician_database(
    test(1),
    test2(A) := test(A) && test(B)
  )
  iter <- logician_query(database, test2(A))
  expect_output(print(iter$next_value()), "A = 1")
})

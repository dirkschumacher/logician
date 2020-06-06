test_that("some simple unifcation work", {
  expect_true(is_unified(unify(int(5), int(5))))
  expect_false(is_unified(unify(int(4), int(5))))
  expect_true(is_unified(unify(atom(as.symbol("wat")), atom(as.symbol("wat")))))
  expect_false(is_unified(unify(atom(as.symbol("wat")), atom(as.symbol("wut")))))
  expect_false(is_unified(unify(char("wat"), char("wut"))))
  expect_true(is_unified(unify(char("wat"), char("wat"))))
  res <- unify(int(5), variable("X"))
  expect_true(
    is_unified(res)
  )
  expect_equal(res@substitutions, list(list(left = int(5), right = variable("X"))))
})

test_that("clause unifcation work", {
  expect_true(
    is_unified(unify(clause("test", int(5), int(5)), clause("test", int(5), int(5))))
  )
  expect_false(
    is_unified(unify(clause("test", int(5), int(5)), clause("test2", int(5), int(5))))
  )
  expect_true(
    is_unified(unify(clause("test", variable("X"), int(5)), clause("test", int(5), int(5))))
  )
  expect_true(
    is_unified(unify(clause("test", variable("X"), variable("X")), clause("test", int(5), int(5))))
  )
  expect_false(
    is_unified(unify(clause("test", variable("X"), variable("X")), clause("test", int(5), int(6))))
  )
  expect_true(
    is_unified(unify(clause("test", variable("X"), variable("Y")), clause("test", int(5), int(6))))
  )
  expect_true(
    is_unified(unify(clause("test", variable("X"), int(5)), clause("test", variable("Y"), int(5))))
  )
})

test_that("further clause unification tests", {
  res <- unify(clause("test", int(5), variable("X")), clause("test", variable("X"), variable("Y")))
  subs <- res@substitutions
  expect_true(is_unified(res))
})

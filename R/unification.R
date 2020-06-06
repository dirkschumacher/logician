setClass("unification_result")
setClass("unification_ok", slots = c("substitutions" = "list"), contains = "unification_result")
setClass("unification_fail", contains = "unification_result")

setGeneric("unify", function(x, y) standardGeneric("unify"))
setMethod("unify", c("logician_scalar", "logician_scalar"), function(x, y) {
  if (x@value == y@value) {
    ok()
  } else {
    fail()
  }
})

setMethod("unify", c("ANY", "ANY"), function(x, y) {
  fail()
})

setMethod("unify", c("logician_variable", "logician_variable"), function(x, y) {
  ok(substitutions = list(list(left = x, right = y)))
})

setMethod("unify", c("logician_int", "logician_variable"), function(x, y) {
  ok(substitutions = list(list(left = x, right = y)))
})

setMethod("unify", c("logician_variable", "logician_int"), function(x, y) {
  ok(substitutions = list(list(left = x, right = y)))
})

setMethod("unify", c("logician_atom", "logician_variable"), function(x, y) {
  ok(substitutions = list(list(left = x, right = y)))
})

setMethod("unify", c("logician_variable", "logician_atom"), function(x, y) {
  ok(substitutions = list(list(left = x, right = y)))
})

setMethod("unify", c("logician_char", "logician_variable"), function(x, y) {
  ok(substitutions = list(list(left = x, right = y)))
})

setMethod("unify", c("logician_variable", "logician_char"), function(x, y) {
  ok(substitutions = list(list(left = x, right = y)))
})

setMethod("unify", c("logician_clause", "logician_clause"), function(x, y) {
  if (x@name != y@name) {
    return(fail())
  }
  if (length(x@args) != length(y@args)) {
    return(fail())
  }
  substitutions <- list()
  n <- length(x@args)
  args_x <- x@args
  args_y <- y@args
  for (i in seq_len(n)) {
    res <- unify(args_x[[i]], args_y[[i]])
    if (inherits(res, "unification_fail")) {
      return(fail())
    }
    # forward apply bindings
    substitutions <- c(substitutions, res@substitutions)
    for (j in seq_len(n - i) + i) {
      # TODO: refactor
      # TODO: make all of that binding lookup O(1)
      if (is_variable(args_x[[j]])) {
        args_x[[j]] <- substitute_bindings(args_x[[j]], substitutions, "left")
      }
      if (is_variable(args_y[[j]])) {
        args_y[[j]] <- substitute_bindings(args_y[[j]], substitutions, "right")
      }
    }
  }
  ok(substitutions = substitutions)
})

substitute_clause <- function(clause, substitutions, side = "right") {
  UseMethod("substitute_clause")
}

substitute_clause.logician_r_expr <- function(clause, substitutions, side = "right") {
  other_side <- if (side == "left") "right" else "left"
  for (sub in substitutions) {
    val_side <- sub[[side]]
    val_other <- sub[[other_side]]
    stopifnot(is_variable(val_side))
    from_val <- as.symbol(val_side@name)
    to_val <- if (is_variable(val_other)) as.symbol(val_other@name) else val_other@value
    clause@code <- replace_symbol(clause@code, from_val, to_val)
  }
  clause
}

substitute_clause.logician_clause <- function(clause, substitutions, side = "right") {
  clause@args <- lapply(clause@args, substitute_bindings, substitutions = substitutions, side = side)
  clause
}

substitute_bindings <- function(x, substitutions, side = "left") {
  if (!is_variable(x)) {
    return(x)
  }
  var_binding <- Filter(function(val) {
    is_variable(val[[side]]) && val[[side]]@name == x@name
  }, substitutions)
  if (length(var_binding) != 1) {
    return(x)
  }
  other_side <- if (side == "left") "right" else "left"
  var_binding[[1]][[other_side]]
}

fail <- function() {
  new("unification_fail")
}

ok <- function(substitutions = list()) {
  new("unification_ok", substitutions = substitutions)
}

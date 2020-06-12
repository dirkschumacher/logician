#' Build a database of rules and facts
#' @param ... your facts and rules
#'
#' @examples
#' database <- logician_database(
#'   test(1, 2),
#'   test2(2, 3),
#'   test3(X, A, Y) := test(X, A) && test2(A, Y)
#' )
#'
#' iter <- logician_query(database, test3(X, 2, 3))
#' iter$next_value()
#' @include types.R
#' @export
logician_database <- function(...) {
  dots <- as.list(substitute(...()))
  lapply(dots, function(element) {
    is_fact <- element[[1]] != ":="
    if (is_fact) {
      fact(to_clause(element))
    } else { # is rule
      head_clause <- to_clause(element[[2]])
      args <- binary_tree_to_list(element[[3]])
      body_clauses <- lapply(args, to_clause_or_expr)
      rule(head_clause, body_clauses)
    }
  })
}

to_clause_or_expr <- function(element) {
  if (element[[1]] == "r") {
    stopifnot(length(element) == 2)
    r_expr(element[[2]])
  } else {
    to_clause(element)
  }
}

binary_tree_to_list <- function(root) {
  if (root[[1]] == "&&") {
    c(binary_tree_to_list(root[[2]]), list(root[[3]]))
  } else {
    list(root)
  }
}

reserved_names <- "r"

to_clause <- function(element) {
  clause_name <- as.character(element[[1]])
  if (clause_name %in% reserved_names) {
    stop("The clause name '", clause_name, "' is reserved name. Please rename. Sorry.")
  }
  args <- lapply(element[-1], function(x) {
    convert_from_r(x)
  })
  do.call(clause, c(list(clause_name), args))
}

convert_from_r <- function(val) {
  UseMethod("convert_from_r", val)
}

convert_from_r.numeric <- function(val) {
  stopifnot(length(val) == 1, !is.na(val), val %% 1 == 0)
  int(as.integer(val))
}

convert_from_r.integer <- function(val) {
  stopifnot(length(val) == 1, !is.na(val))
  int(val)
}

starts_with_upper <- function(x) {
  stopifnot(length(x) == 1, nchar(x) >= 1)
  substr(x[1], 1, 1)  == substr(toupper(x[1]), 1, 1)
}

convert_from_r.name <- function(val) {
  val <- as.character(val)
  is_upper_case <- starts_with_upper(val)
  if (is_upper_case) {
    variable(val)
  } else {
    atom(val)
  }
}

convert_from_r.character <- function(val) {
  stopifnot(length(val) == 1, !is.na(val))
  char(val)
}

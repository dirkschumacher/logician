#' Query a database
#'
#' @param database a list of rules and facts
#' @param question a clause of type \code{logician_clause} or an R expression
#'   of type \code{logician_r_expr}
#'
#' @export
#' @rdname logician_query
#' @include query.R
#' @examples
#' database <- logician_database(
#'   connected(berlin, hamburg),
#'   connected(hamburg, chicago),
#'   connected(chicago, london),
#'   connected(aachen, berlin),
#'   connected(chicago, portland),
#'   connected(portland, munich),
#'   path(A, B) := connected(A, B),
#'   path(A, B) := connected(A, Z) && path(Z, B)
#' )
#' iter <- logician_query(database, path(berlin, hamburg))
#' iter$next_value()
#'
#' iter <- logician_query(database, path(berlin, X))
#' iter$next_value()
#' iter$next_value()
#' iter$next_value()
logician_query <- function(database, question) {
  question <- to_clause(substitute(question))
  logician_query_(database, question)
}

#' @rdname logician_query
#' @export
logician_query_ <- function(database, question) {
  UseMethod("logician_query_", question)
}

#' @rdname logician_query
#' @export
logician_query_.logician_clause <- function(database, question) {
  stack <- list()
  push <- function(x) stack <<- list(x, stack)
  pop <- function() {
    el <- stack[[1]]
    stack <<- stack[[2]]
    el
  }
  put_database_on_stack(database, question, push)
  iterator(
    function() {
      while (length(stack) > 0) {
        element <- pop()
        res <- handle_stack_element(push, pop, database, element)
        if (res$should_return) {
          return(res$value)
        }
      }
      return(result(FALSE))
    }
  )
}

#' @rdname logician_query
#' @export
logician_query_.logician_r_expr <- function(database, question) {
  has_returned <- FALSE
  return(
    iterator(
      function() {
        if (has_returned) {
          return(result(FALSE))
        }
        res <- eval(question@code, envir = globalenv())
        stopifnot(is.logical(res), length(res) == 1, !is.na(res))
        has_returned <<- TRUE
        return(result(res))
      }
    )
  )
}

#' @export
print.logician_query_result <- function(x, ...) {
  cat(format(as.logical(x)))
  cat("\n")
  i <- 0
  subs <- substitutions(x)
  n <- length(subs)
  for (sub in subs) {
    i <- i + 1
    cat(sub$left@name, "=", sub$right@value)
    if (i < n) {
      cat(";\n")
    } else {
      cat(".\n")
    }
  }
}

#' Return the first n results from an iterator
#'
#' @param x an object
#' @param n an length 1 integer >= 1.
#'   sometimes that can result in an infinite loop
#' @param ... unused dots
#'
#' @return
#' For each solution a list of substitions.
#'
#' @examples
#' database <- logician_database(
#'   connected(berlin, hamburg),
#'   connected(hamburg, chicago),
#'   connected(chicago, london),
#'   connected(aachen, berlin),
#'   connected(chicago, portland),
#'   connected(portland, munich),
#'   path(A, B) := connected(A, B),
#'   path(A, B) := connected(A, Z) && path(Z, B)
#' )
#' iter <- logician_query(database, path(berlin, X))
#' head(iter, 4) # get the first 4 results if they exists
#' @export
#' @importFrom utils head
head.logician_iterator <- function(x, n = 6, ...) {
  stopifnot(n >= 1, is.numeric(n), length(n) == 1)
  res <- vector("list", length = n)
  for (i in seq_len(n)) {
    val <- x$next_value()
    if (val) {
      res[[i]] <- substitutions(val)
    } else {
      res <- res[seq_len(i - 1)]
      break
    }
  }
  res
}

result <- function(x, substitutions = list()) {
  stopifnot(is.logical(x), length(x) == 1, is.list(substitutions))
  attr(x, "substitutions") <- substitutions
  class(x) <- c("logician_query_result", class(x))
  x
}

substitutions <- function(x) {
  stopifnot(inherits(x, "logician_query_result"))
  attr(x, "substitutions", exact = TRUE)
}

iterator <- function(next_value_fun) {
  structure(list(
    next_value = next_value_fun
  ), class = c("logician_iterator", "list"))
}

handle_stack_element <- function(push, pop, database, element) {
  UseMethod("handle_stack_element", element)
}

handle_stack_element.fact <- function(push, pop, database, element) {
  res <- unify(element$clause, element$target@clause)
  if (is_unified(res)) {
    val <- result(TRUE, res@substitutions)
    return(list(should_return = TRUE, value = val))
  }
  list(
    should_return = FALSE
  )
}

handle_stack_element.rule <- function(push, pop, database, element) {
  target <- element$target
  res <- unify(element$clause, target@head)
  if (is_unified(res)) {
    terms <- lapply(target@body,
      function(x) substitute_clause(x, substitutions = res@substitutions, side = "right")
    )
    head <- substitute_clause(target@head, res@substitutions, side = "right")
    evaluate_body_again(
      terms,
      depth = 0,
      push = push,
      unified_head = head,
      substitutions = list(res@substitutions)
    )
  }
  list(
    should_return = FALSE
  )
}

handle_stack_element.rule_body_iterator <- function(push, pop, database, element) {
  n_terms <- element$n
  depth <- element$depth
  stopifnot(n_terms > 0)
  terms <- vector("list", n_terms)
  for (i in rev(seq_len(n_terms))) {
    terms[[i]] <- pop()
  }
  if (depth == 0) {
    depth <- depth + 1
    current_term <- terms[[depth]]
    iter <- logician_query_(database, current_term$term)
    current_term$iter <- iter
    terms[[depth]] <- current_term
    evaluate_body_again(
      terms, depth, push,
      substitutions = list(),
      unified_head = element$unified_head
    )
  } else if (depth < n_terms) {
    current_term <- terms[[depth]]
    iter <- current_term$iter
    res <- iter$next_value()
    if (res) {
      depth <- depth + 1
      next_term <- terms[[depth]]
      all_subs <- c(
        unlist(element$substitutions, recursive = FALSE),
        substitutions(res)
      )
      iter <- logician_query_(
        database, substitute_clause(next_term$term, all_subs, side = "left"))
      next_term$iter <- iter
      terms[[depth]] <- next_term
      evaluate_body_again(
        terms, depth, push,
        substitutions = c(element$substitutions, list(substitutions(res))),
        unified_head = element$unified_head
      )
    } else {
      depth <- depth - 1
      if (depth > 0) {
        evaluate_body_again(
          terms, depth, push,
          unified_head = element$unified_head,
          substitutions = element$substitutions[-length(element$substitutions)]
        )
      }
    }
  } else if (depth == n_terms) {
    current_term <- terms[[depth]]
    iter <- current_term$iter
    res <- iter$next_value()
    if (res) {
      vars <- Filter(is_variable, element$unified_head@args)
      subs <- Filter(function(x) {
        length(Filter(function(y) x$left@name == y@name, vars)) > 0
      }, c(unlist(element$substitutions, recursive = FALSE), substitutions(res)))

      if (length(subs) > 0) {
        evaluate_body_again(
          terms, depth, push,
          unified_head = element$unified_head,
          substitutions = element$substitutions
        )
      }
      return(
        list(
          should_return = TRUE,
          value = result(TRUE, subs)
        )
      )
    } else {
      depth <- depth - 1
      if (depth > 0) {
        evaluate_body_again(
          terms, depth, push,
          unified_head = element$unified_head,
          substitutions = element$substitutions[-length(element$substitutions)]
        )
      }
    }
  }
  list(
    should_return = FALSE
  )
}

evaluate_body_again <- function(terms, depth, push, unified_head, substitutions) {
  for (term in terms) {
    val <- if (is.list(term)) term else list(term = term)
    push(new_node(val, type = "term_list_entry"))
  }
  push(new_node(list(
    n = length(terms),
    depth = depth,
    unified_head = unified_head,
    substitutions = substitutions
  ),
  type = "rule_body_iterator"
  ))
}

new_node <- function(x, type = "clause") {
  x$type <- type
  class(x) <- c(type, class(x))
  x
}

put_database_on_stack <- function(database, clause, push) {
  for (element in rev(database)) {
    class <- if (is_fact(element)) "fact" else "rule"
    push(new_node(list(clause = clause, target = element), type = class))
  }
}

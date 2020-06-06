
#' Check if object is a fact
#'
#' @param x any object
#'
#' @export
is_fact <- function(x) {
  inherits(x, "logician_fact")
}

#' Check if object is a rule
#'
#' @param x any object
#'
#' @export
is_rule <- function(x) {
  inherits(x, "logician_rule")
}

#' Create a fact
#' @param x a clause
#' @export
fact <- function(x) {
  new("logician_fact", clause = x)
}


#' Create an int
#' @param x an integer or numeric that can be converted to an integer.
#' @export
int <- function(x) {
  new("logician_int", value = x)
}

#' Create a character
#' @param x a length 1 character.
#' @export
char <- function(x) {
  new("logician_char", value = x)
}

#' Create a variable
#' @param name a length 1 character.
#' @export
variable <- function(name) {
  new("logician_variable", name = name)
}

#' Create an atom
#' @param x a length 1 character.
#' @export
atom <- function(x) {
  new("logician_atom", value = x)
}

#' Create a clause
#' @param name a length 1 character, the name of the clause.
#' @param ... any number of valid arguments. I.e. int, atom, variable or char.
#' @export
clause <- function(name, ...) {
  new("logician_clause", name = name, args = list(...))
}

#' Create an r expression
#' @param code valid R code that returns a length 1 logical. Any symbols with
#' uppercase first letter will be treated as internal variables. You can only
#' refer to names from the globalenv here.
#' @export
r_expr <- function(code) {
  # we intentionally do not capture the calling environment
  new("logician_r_expr", code = code)
}

#' Create a rule
#' @param head a head is always a clause
#' @param body a body is a list of clauses or r expressions.
#' @export
rule <- function(head, body) {
  body <- rename_non_head_variables(extract_variables(head), body)
  new("logician_rule", head = head, body = body)
}

extract_variables <- function(val) {
  UseMethod("extract_variables")
}

extract_variables.logician_r_expr <- function(val) {
  # we assume all symbols that start with upper case letter are variables
  # used in our logical program and not R globalenv symbols
  vars <- all.vars(val@code)
  vars[vapply(vars, starts_with_upper, logical(1))]
}

extract_variables.logician_clause <- function(val) {
  vapply(Filter(is_variable, val@args), function(x) x@name, character(1))
}

variable_counter_class <- setRefClass("variable_counter_class", fields = list(counter = "numeric"))
variable_counter_class$methods(
  get = function() {
    counter
  },
  inc = function() {
    counter <<- counter + 1
    counter
  }
)
variable_counter <- variable_counter_class(counter = 1)

new_var_name <- function() {
  var_name <- paste0("LOGICIAN_INTERNAL_VAR_", variable_counter$get())
  variable_counter$inc()
  var_name
}

rename_non_head_variables <- function(head_vars, body) {
  var_mapping <- list()
  lapply(body, function(clause) {
    clause_vars <- extract_variables(clause)
    vars_to_rename <- setdiff(clause_vars, head_vars)
    if (length(vars_to_rename) == 0) {
      return(clause)
    }
    for (var in vars_to_rename) {
      if (is.null(var_mapping[[var]])) {
        var_mapping[[var]] <<- list(left = variable(var), right = variable(new_var_name()))
      }
    }
    substitute_clause(clause, substitutions = var_mapping, side = "left")
  })
}

replace_symbol <- function(code, symbol_from, symbol_to) {
  if (is.symbol(code) && code == symbol_from) {
    return(symbol_to)
  }
  if (is.call(code) && length(code) > 1) {
    for (i in seq_along(code)[-1]) {
      code[[i]] <- replace_symbol(code[[i]], symbol_from, symbol_to)
    }
  }
  code
}

is_unified <- function(x) {
  inherits(x, "unification_ok")
}

setClass("logician_scalar", slots = c("value" = "ANY"))
setClass("logician_int", contains = "logician_scalar")
setClass("logician_char", contains = "logician_scalar")
setClass("logician_atom", contains = "logician_scalar")


setClass("logician_variable", slots = c("name" = "ANY"))
setClass("logician_clause", slots = c("name" = "character", args = "list"))
setClass("logician_r_expr", slots = c("code" = "ANY"))

setClass("logician_fact", slots = c("clause" = "logician_clause"))
setClass("logician_rule", slots = c("head" = "logician_clause", "body" = "list"))

is_variable <- function(x) {
  inherits(x, "logician_variable")
}

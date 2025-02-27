% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{logician_database}
\alias{logician_database}
\title{Build a database of rules and facts}
\usage{
logician_database(...)
}
\arguments{
\item{...}{your facts and rules}
}
\description{
Build a database of rules and facts
}
\examples{
database <- logician_database(
  test(1, 2),
  test2(2, 3),
  test3(X, A, Y) := test(X, A) && test2(A, Y)
)

iter <- logician_query(database, test3(X, 2, 3))
iter$next_value()
}


<!-- README.md is generated from README.Rmd. Please edit that file -->

# Logic programming in R

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/dirkschumacher/logician/workflows/R-CMD-check/badge.svg)](https://github.com/dirkschumacher/logician/actions)
<!-- badges: end -->

The goal of `logician` is to do logic programming inspired by
[datalog](https://en.wikipedia.org/wiki/Datalog)/[prolog](https://en.wikipedia.org/wiki/Prolog)
in R. It is written in R without any third-party package dependencies.
It targets interactive use and smaller instances of logical programs.

Non-goal: be fully prolog compatible and super fast.

Side-goal: experiment and have fun.

## Installation

~~You can install the released version of logician from
[CRAN](https://CRAN.R-project.org) with:~~

``` r
install.packages("logician")
```

Or the current github version from here:

``` r
remotes::install_github("dirkschumacher/logician")
```

## General principle

The general idea is to query a database of *facts* and *rules* and ask
questions. `logician` tries prove that your query is either `true` or
`false`. Since you can use variables, there might be multiple
assignments to variables that make your query `true`. `logician` will
return these one by one as an iterator.

This is all work in progress and still a bit hacky, but usable.

## Example

``` r
library(logician)
```

### A graph example

Here we define a database with two types of elements:

1.  a fact for each tuple of nodes that are directly connected
2.  a rule that determines if there exists a path between two nodes.
      - A path between `A` and `B` exists if `A` and `B` are connected
        OR
      - if `A` is connected to an intermediate node `Z` and there exists
        a path from `Z` to `B`.

<!-- end list -->

``` r
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
```

``` r
iter <- logician_query(database, path(berlin, hamburg))
iter$next_value()
#> TRUE
```

``` r
iter <- logician_query(database, path(berlin, munich))
iter$next_value()
#> TRUE
```

At last let’s find all nodes `berlin` is connected to.

``` r
iter <- logician_query(database, path(berlin, X))
iter$next_value()
#> TRUE
#> X = hamburg.
iter$next_value()
#> TRUE
#> X = chicago.
iter$next_value()
#> TRUE
#> X = london.
iter$next_value()
#> TRUE
#> X = portland.
iter$next_value()
#> TRUE
#> X = munich.
iter$next_value()
#> FALSE
```

## Embracing the host

Unlike prolog, we run on a host language and can integrate R into the
evaluation of rules. The only requirement is that the R expression
returns a length 1 logical and does not depend on anything outside the
`globalenv`. If that is good idea remains to be seen :) One downside
that the list of possible results could be infinite.

Any expression in the `r` clause will be treated as an R expression that
is not unified as the usual clauses. All variables need to be bound
before it can be used.

``` r
database <- logician_database(
    number(1),
    number(2),
    number(3),
    number(4),
    number(5),
    sum(A, B) := number(A) && number(B) && r(A + B > 3)
)
```

``` r
iter <- logician_query(database, sum(1, B))
iter$next_value()
#> TRUE
#> B = 3.
iter$next_value()
#> TRUE
#> B = 4.
iter$next_value()
#> TRUE
#> B = 5.
iter$next_value()
#> FALSE
```

### Database Example

You could also think of the `database` as a real database. Where each
fact is a row of a specific table. `rules` are additional logical
structures of your data. Then you can use logical programming instead of
SQL to query your data.

At the moment this is not as practical though as it could be.

``` r
database <- logician_database(
  # the employee table aka relation
  employee(bart),
  employee(homer),
  employee(marge),
  employee(maggie),
  employee(lisa),
  
  # the payment table
  salary(bart, 100),
  salary(homer, 100),
  salary(marge, 120),
  salary(maggie, 140),
  salary(lisa, 180),
  
  # reporting hierarchy
  manages(lisa, maggie),
  manages(lisa, marge),
  manages(marge, homer),
  manages(marge, bart),
  
  # direct and indirect reports
  reports_to(A, B) := manages(B, A),
  reports_to(A, B) := manages(B, X) && reports_to(A, X),
  
  # a salary query
  makes_more(A, X) := salary(A, Y) && r(Y > X + pi)
  # using pi here to show that certain R symbols from the globalenv
  # can be used.
)
```

``` r
# who reports to lisa?
iter <- logician_query(database, reports_to(A, lisa))
iter$next_value()
#> TRUE
#> A = maggie.
iter$next_value()
#> TRUE
#> A = marge.
iter$next_value()
#> TRUE
#> A = homer.
iter$next_value()
#> TRUE
#> A = bart.
iter$next_value()
#> FALSE

# and who makes more than 140 + pi?
iter <- logician_query(database, makes_more(A, 140))
iter$next_value()
#> TRUE
#> A = lisa.
iter$next_value()
#> FALSE
```

And instead of this manual database, you generate a database with real
world data. Though the query system might slow for large amount of at
this point.

## API

The API has two components: one aimed at interactive use and another one
when you want to program with it (e.g. embed it into another package).

  - `logician_database` a helper function to construct a database with
    the help of non-standard evaluation. A database is just a list of
    `fact`s and `rule`s that you can also construct yourself using
    helper functions such as `fact`, `rule`, `atom`, `char`, `clause`,
    `variable`, `int` and `r_expr`.

  - `logician_query` main query function for interactive use. You can
    use `head` on an iterator to return the first `n` results (if they
    exists).

  - `logician_query_` same as `logician_query` but expects a `clause` or
    `r_expr`.

## Datatypes and Terminology

  - A database contains `facts` and `rules`.
  - Each `fact` is a `clause`.
  - A `clause` has a name and a list of `arguments`.
  - An argument can either be an `atom`, `int`, `char` or `var`.
  - Per convention, variables start with a capital letter, atoms with a
    lower case letter.
  - Each `rule` has a `head` and a `body`.
  - A `head` is a `clause`.
  - A `body` is a list of `clauses` or `R expressions`.

## Current Limitations

  - Still experimental, so there might be bugs or undefined behavior.
    API likely will have breaking changes in the future.
  - `logician_query` supports only one clause at the moment. If you want
    to query for multiple clauses at one, create a rule for that query.
  - No list support … yet.
  - No cuts. Might never be supported.
  - No higher order terms. Though most likely I will not support them.

## Test Coverage

``` r
covr::package_coverage()
#> logician Coverage: 98.08%
#> R/unification.R: 96.30%
#> R/database.R: 97.50%
#> R/types.R: 97.67%
#> R/query.R: 98.86%
```

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/my_abind.R
\name{my_abind}
\alias{my_abind}
\title{Bind arrays together, based on their dimension names.}
\usage{
my_abind(..., .list = FALSE, .list_new.dnn = "..NEW..")
}
\arguments{
\item{...}{A sequence of disjoint arrays (or \code{NULL}s, that
will be ignored) that we want to put together into a larger
array.  The arrays must have the same number of dimensions with
properly named dimension names.  The dimension names of these
arrays must pairwise have empty intersections.  It is also
possible to give a list of arrays, in which case the other two
arguments are needed too.}

\item{.list}{Logical value, default \code{FALSE}, that can be used
to feed a list of arrays into the function.  The list does not
need names if the arrays have non-overlapping dimension-names.
It is allowed to have a list where all the dimension-names are
identical, but in this case the list must have unique names
that can be used to adjust the dimension-names of the arrays in
it.}

\item{.list_new.dnn}{A character string, default \code{"..NEW.."},
that will be used as the name of the new dimension when a list
of arrays must have its dimension-names adjusted,
cf. discussion related to the \code{.list}-argument.}
}
\value{
A larger array containing all of the smaller ones, put
    together by the help of \code{abind} along a dimension selected
    by the code.
}
\description{
This function is a wrapper for \code{abind} that binds a bunch of
arrays together after an inspection of their dimension-names (it
will crash if the the arrays have no named dimensions.  As this is
a wrapper for \code{abind}, it will be slightly slower, but the
effect is tiny for large arrays and it simplifies the writing of
the code in other functions.
}

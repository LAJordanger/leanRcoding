% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/spy.R
\name{spy}
\alias{spy}
\title{Spy on the arguments used}
\usage{
spy()
}
\value{
This function returns a list with two components, a paired
list \code{formals} and an environment \code{arg.env}.
\code{formals} contains all the arguments needed for a rerun of the
targeted function.  Note that \code{formals} will _only_ return the
\code{symbol}, i.e. the name, when an argument is given to the
function from an externally stored object.  The environment
\code{arg.env} contains all the actual values needed in order to
rerun this function later on.
}
\description{
This function is to be used inside of a target function, and it
will then record the full set of formals for that function and in
addition will it store the corresponding list of all the arguments
that were used when the function was called, both defaulted values
and user specified values.  The main reason for this function is to
ensure that the bootstrap-wrapper simply can replace the original
time series with the re-sampled alternatives, and that this should
suffice to rerun the analysis with the original specifications of
the remaining arguments.
}

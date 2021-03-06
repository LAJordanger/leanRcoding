% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/equal_environments.R
\name{equal_environments}
\alias{equal_environments}
\title{Equal environments.}
\usage{
equal_environments(env1, env2)
}
\arguments{
\item{env1}{The first environment to investigate}

\item{env2}{The second environment to investigate.}
}
\value{
The result of this function will be either \code{TRUE} or
    \code{FALSE}, with an attribute \code{identical} that reveals
    the result of the identical test.
}
\description{
This function will check if two environments are equal enough to be
considered interchangeable, i.e. with regard to having them
attached to the \code{search}-path.
}
\details{
This function is mainly intended as a helper-function for
    \code{options_attach}, and its present incarnation does not go
    deep into the internal hierarchy of the environments.  The
    procedure starts out by testing if the environments are
    identical, and if not it looks into them to see if they contain
    the exact same content.  If the main environments contains
    environments themselves, then the test will only check if these
    are identical.  It's possible to use an iterative algorithm,
    but that would require the addition of a safeguard against
    infinite recursions.
}

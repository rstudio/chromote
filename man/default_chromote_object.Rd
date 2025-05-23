% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chromote.R
\name{default_chromote_object}
\alias{default_chromote_object}
\alias{has_default_chromote_object}
\alias{set_default_chromote_object}
\title{Default Chromote object}
\usage{
default_chromote_object()

has_default_chromote_object()

set_default_chromote_object(x)
}
\arguments{
\item{x}{A \link{Chromote} object.}
}
\description{
Returns the Chromote package's default \link{Chromote} object. If
there is not currently a default \code{Chromote} object that is active, then
one will be created and set as the default.
}
\details{
\code{ChromoteSession$new()} calls this function by default, if the
\code{parent} is not specified. That means that when
\code{ChromoteSession$new()} is called and there is not currently an
active default \code{Chromote} object, then a new \code{Chromote} object will
be created and set as the default.
}

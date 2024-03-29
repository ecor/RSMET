% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/as.meteoioini.R
\name{as.meteoioini}
\alias{as.meteoioini}
\alias{as.meteoioini,character-method}
\alias{as.meteoioini,list-method}
\alias{as.meteoioini,meteoioini-method}
\title{Coerces an object to a \code{meteoioini-class}  object}
\usage{
as.meteoioini(object, ...)

as.meteoioini(object, ...)

\S4method{as.meteoioini}{character}(object, ...)

\S4method{as.meteoioini}{list}(object, ...)

\S4method{as.meteoioini}{meteoioini}(object, ...)
}
\arguments{
\item{object}{the object to be coerced}

\item{...}{further arguments}
}
\description{
It coerces an object to a \code{meteoioini-class}  object. In case the object is a character string, it is considered to be the name of an INI file and the \code{meteoioini-class}  object is imported.
}
\examples{

ini <- as.meteoioini("test")
 


}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pushsmetintoini.R
\name{pushSmetIntoIni}
\alias{pushSmetIntoIni}
\title{Pushes SMET object file into a \code{\link{meteoioini-class}} object}
\usage{
pushSmetIntoIni(
  ...,
  smetlist = NULL,
  ini = as.meteoioini("test"),
  smetwdir = system.file("temp", package = "RSMET"),
  force.smetdir = TRUE,
  new.ini.file = NA,
  append = FALSE
)
}
\arguments{
\item{...}{\code{\link{smet-class}} objects}

\item{smetlist}{(alternative) list of \code{\link{smet-class}} objects}

\item{ini}{a \code{\link{meteoioini-class}} object}

\item{smetwdir}{optional working directory used for SMET writing on disks. Default is the package internal directory \code{system.file("temp",package="RSMET")}.}

\item{force.smetdir}{logical value. In case it is \code{TRUE} the SMETs are written on \code{smetwdir} directory, otherwise thay can be written on \code{slot(ini,"Input")$METEOPAH} directory if specified.}

\item{new.ini.file}{character string. DEfault is \code{NA}. If it is specified, the returned \code{\link{meteoioini-class}} object is saved as the \code{new.ini.file} file.}

\item{append}{logical value. Default is code{FALSE}. If it is \code{TRUE} SMETs will be appended to the pre-existing ones. If it is \code{TRUE}, \code{force.smetdir} and \code{smetwdir} arguments will be ingored.}
}
\value{
An updated \code{\link{meteoioini-class}} object.
}
\description{
Pushes SMET object file into a \code{\link{meteoioini-class}} object
}
\examples{

ini <- as.meteoioini("test")

AA <- as.smet("test")
BB <- as.smet("test")


newini <- pushSmetIntoIni(AA,BB,ini=ini)
newini <- pushSmetIntoIni(AA=AA,BB=BB,ini=ini)
newini <- pushSmetIntoIni(AA=AA,BB=BB,ini=ini)




##  NOT YET IMPLEMENTED

}

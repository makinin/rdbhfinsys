% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_data.R
\name{finsys_dbh_import}
\alias{finsys_dbh_import}
\title{Laster ned alle finsystabellene}
\usage{
finsys_dbh_import(arstall, institusjoner = .finsys_dbh_inst())
}
\arguments{
\item{arstall}{velg årstall}

\item{institusjoner}{institusjoner som delta i Blåttfete}
}
\value{
tibble
}
\description{
Bruker rdbhapi pakke for å
laste ned alle tabellene som vi trenger for
videre beregning
}

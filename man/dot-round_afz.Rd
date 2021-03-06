% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tidy_data.R
\name{.round_afz}
\alias{.round_afz}
\title{Beregner budsjettendringstall per indikator}
\usage{
.round_afz(x, digits = 0)
}
\arguments{
\item{x}{value}

\item{digits}{decimal places}
}
\description{
Bruker alternativ avrundringsfunskjon,
med ulik behandling av avrundingsfeil
på 0.5n (afz = away from zero, 1.5 ≈ 2, -2.5 ≈ -3).
Funksjonen base::round
bruker såkalt "banker's rounding" hvor det rundes bort
fra oddetall (1.5 ≈ 2, -2.5 ≈ -2).
}
\keyword{internal}

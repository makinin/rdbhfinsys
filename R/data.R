#' @title Predefined data for "Blåtthefte" report
#'
#' @description A list contains a data that is necessary for calculation of financial report for Higher
#' education insitutions in Norway.
#' @format a \code{tibble} with one column which is
#' @describe {
#' \item{Institutionskode}{character of code that are inclueds in Financial report}
#'
#' "institutions"
#'
#' #' @title A list of institutions with special rate, rate not equal to one in Financial report
#'
#' @description A list contains a data that is have a special rate and are included in Financial report.
#'
#' @format a \code{tibble} with three columns which are
#' @describe {
#' \item{institusjonskode}{character of code that are inclueds in Financial report}
#' \item{indikator}{categori with spacial thretemet}
#' \item{tilskuddsgrad}{numerical value of special rate}
#' }
#'
#' "tilskuddsgrad"
#' @title A list of institutions which is excluded from certain categories
#'
#' @description A list contains a list of institutions and categories they are excluded from calculations.
#'
#' @format a \code{tibble} with two columns which are
#' @describe {
#' \item{institusjonskode}{character of code that are inclueds in Financial report}
#' \item{unntak}{categori institutions are excluded from calculations}
#' }
#'
#' "unntak"

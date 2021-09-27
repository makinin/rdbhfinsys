#' Inneholder institusjoner i Blåtthefte
#' @description Funksjon returnerer en vektor med
#' institusjonskode
#'
#'
#' @return a vector of institutions
#' @keywords internal


.finsys_dbh_institusjoner <- function() {
  inst_list <- c("0212", "0215", "0217", "0221", "0222", "0232",
              "0233", "0235", "0236", "0237", "0238", "0252",
              "0254", "0256", "0257", "0259",
              "0261", "0262", "0263", "0264",
              "1110", "1120", "1130", "1150",
              "1160", "1171",
              "1173", "1174", "1175", "1176",
              "1210", "1220", "1240", "1260", "6220", "8202",
              "8208", "8221", "8223", "8224", "8225",
              "8227", "8228", "8232", "8234", "8241",
              "8243", "8247",
              "8248", "8249", "8252", "8253")
inst_list
}


#' DBH-tabeller med API-valg
#'
#' @description Spøring til DBH-API med en
#' liste over tabller i Blåtthefte
#'
#' @param arstall velg årstall
#' @param institusjoner institusjoner som delta i Blåttfete
#'
#' @return tibble
#' @importFrom tibble tribble
#' @keywords internal

.finsys_dbh_query <- function(arstall,
                           institusjoner = .finsys_dbh_institusjoner()) {
  tibble::tribble(~tabellnavn, ~table_id, ~group_by, ~filters,
  "studiepoeng", 900, c("Institusjonskode", "\u00c5rstall",
              "Finmodkode student", "Finmodekode emne", "Studentkategori"),
  list("\u00c5rstall" =
         c(arstall, arstall - 1), Institusjonskode = institusjoner),
  "kandidater", 907, NULL, list("\u00c5rstall" = c(arstall, arstall - 1),
                                Institusjonskode = institusjoner),
  "utveksling", 142, c("\u00c5rstall",
                       "Institusjonskode", "Utvekslingsavtale", "Type",
               "Niv\u00e5kode"), list("\u00c5rstall" = c(arstall, arstall - 1),
                                              Institusjonskode = institusjoner),
  "ekonomi", 902, NULL, list("\u00c5rstall" = c(arstall, arstall - 1),
                             Institusjonskode = institusjoner),
  "publisering", 373, c("\u00c5rstall", "Institusjonskode"),
  list("\u00c5rstall" = c(arstall, arstall - 1),
       "Kode for type publiseringskanal" = c("1", "2"),
       Institusjonskode = institusjoner),
  "doktorgrader", 101, c("Institusjonskode", "\u00c5rstall"),
  list("\u00c5rstall" = c(arstall, arstall - 1),
       Institusjonskode = institusjoner),
  "doktorgrader_samarbeid", 100,
  c("\u00c5rstall", "Institusjonskode (arbeidsgiver)"),
  list("\u00c5rstall" = c(arstall, arstall - 1),
       "Institusjonskode (arbeidsgiver)" = institusjoner),
  "PKU", 98, NULL, list("\u00c5rstall" = c(arstall, arstall - 1)),
  "institusjoner", 211, NULL, NULL,
  )
}



#' Laster ned alle finsystabellene
#'
#' @description Bruker rdbhapi pakke for å
#' laste ned alle tabellene som vi trenger for
#' videre beregning
#'
#' @param arstall velg årstall
#' @param institusjoner institusjoner som delta i Blåttfete
#'
#' @return tibble
#' @export
#' @importFrom purrr pmap
#' @importFrom rdbhapi dbh_data
#' @importFrom stringr str_to_lower
#' @importFrom dplyr rename_with
#' @importFrom stats setNames
#'
#' @export


finsys_dbh_import <- function(arstall,
                institusjoner = .finsys_dbh_institusjoner()) {
  local({

    res <- purrr::pmap(.finsys_dbh_query(arstall, institusjoner),
                function(tabellnavn, ...) {

                  res <-
                    do.call(rdbhapi::dbh_data, list(...)) %>%
                    dplyr::rename_with(stringr::str_to_lower)
                  res <- stats::setNames(list(res), tabellnavn)
                  res
                })

    unlist(res, recursive = FALSE)

  })
}

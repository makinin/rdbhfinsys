#' Inneholder institusjoner i Blåtthefte
#' @description Funksjon returnerer en vektor med
#' institusjonskode
#'
#'
#' @return a vector of institutions
#' @export
#' @keywords internal


.finsys_dbh_inst <- function() {
  inst_list <- c("0212", "0215", "0217", "0221", "0222", "0232",
                 "0233", "0235", "0236", "0237", "0238", "0252",
                 "0254", "0256", "0257", "0259", "0261", "0262",
                 "0263", "0264", "1110", "1120", "1130", "1150",
                 "1160", "1171", "1173", "1174", "1175", "1176",
                 "1210", "1220", "1240", "1260", "6220", "8202",
                 "8208", "8221", "8223", "8224", "8225", "8227",
                 "8228", "8232", "8234", "8241", "8243", "8247",
                 "8248", "8249", "8252", "8253")
  inst_list
}

#' Inneholder institusjoner med tilskuddsgrad i Blåtthefte
#' @description Funksjon returnerer en vektor med tilskuddsgrad
#' verdi for indikator og
#' institusjonskode
#'
#'
#' @return a vector of institutions
#' @keywords internal
.finsys_dbh_tilskuddsgrad <- function(){
  tibble::tribble(~institusjonskode, ~indikator, ~tilskuddsgrad,
                  "8241",	"studiepoeng", 0.3,
                  "8243",	"studiepoeng", 0.7,
                  "8253",	"studiepoeng", 0.6,
                  "8241", "kandidater", 0.3,
                  "8243", "kandidater", 0.7,
                  "8253", "kandidater",	0.6,
                  "8241", "utveksling",	0.3,
                  "8243", "utveksling",	0.7,
                  "8253", "utveksling",	0.6,
                  "8241", "studiepoeng", 0.3,
                  "8243",	"studiepoeng", 0.7,
  )
}



#' Inneholder unntakk institusjoner for indikator i Blåtthefte
#' @description Funksjon returnerer en vektor med unntak indikator og
#' institusjonskode
#'
#'
#' @return a vector of institutions
#' @keywords internal
.finsys_dbh_unntak <- function(){
  tibble::tribble (~institusjonskode, ~indikator,
                   "1220", "publisering",
                   "6220", "studiepoeng",
                   "1210", "publisering",
                   "8232", "EU",
                   "8224", "EU",
                   "8234", "EU",
                   "8202", "EU",
                   "8223", "EU",
                   "8227", "doktorgrader",
                   "8243", "doktorgrader",
                   "8247", "EU",
                   "8248", "EU",
                   "8228", "doktorgrader",
                   "8225", "EU",
                   "1220", "EU",
                   "6220", "kandidater",
                   "1210", "EU",
                   "8232", "NFR",
                   "8224", "NFR",
                   "8234", "NFR",
                   "8202", "NFR",
                   "8223", "NFR",
                   "8227", "publisering",
                   "8243", "publisering",
                   "8247", "NFR",
                   "8248", "NFR",
                   "8228", "publisering",
                   "8225", "NFR",
                   "1220", "NFR",
                   "6220", "utveksling",
                   "1210", "NFR",
                   "8232", "BOA",
                   "8224", "BOA",
                   "8234", "BOA",
                   "8202",	"BOA",
                   "8223", "BOA",
                   "8227", "EU",
                   "8243", "EU",
                   "8247", "BOA",
                   "8248", "BOA",
                   "8228",	"EU",
                   "8225", "BOA",
                   "1220", "BOA",
                   "6220", "publisering",
                   "1210", "BOA",
                   "8227", "NFR",
                   "8243", "NFR",
                   "8228", "NFR",
                   "6220", "EU",
                   "8227", "BOA",
                   "8243", "BOA",
                   "8228", "BOA",
                   "6220", "NFR",
                   "6220", "BOA", )

}


#' DBH-tabeller med API-valg
#'
#' @description Spøring til DBH-API med en
#' liste over tabller i Blåtthefte
#'
#' @param top velg årstall
#' @param institusjoner institusjoner som delta i Blåttfete
#'
#' @return tibble
#' @importFrom tibble tribble
#' @keywords internal

.finsys_dbh_query <- function(top, institusjoner = .finsys_dbh_inst()) {
  tibble::tribble(~tabellnavn, ~table_id, ~group_by, ~filters,
                  "studiepoeng", 900, c("Institusjonskode", "\u00c5rstall",
                                        "Finmodkode student", "Finmodekode emne", "Studentkategori"),
                  list("\u00c5rstall" =
                         c("top", top), Institusjonskode = institusjoner),
                  "kandidater", 907, NULL, list("\u00c5rstall" = c("top", top),
                                                Institusjonskode = institusjoner),
                  "utveksling", 142, c("\u00c5rstall",
                                       "Institusjonskode", "Utvekslingsavtale", "Type",
                                       "Niv\u00e5kode"), list("\u00c5rstall" = c("top", top),
                                                              Institusjonskode = institusjoner),
                  "ekonomi", 902, NULL, list("\u00c5rstall" = c("top", top),
                                             Institusjonskode = institusjoner),
                  "publisering", 373, c("\u00c5rstall", "Institusjonskode"),
                  list("\u00c5rstall" = c("top", top),
                       "Kode for type publiseringskanal" = c("1", "2"),
                       Institusjonskode = institusjoner),
                  "doktorgrader", 101, c("Institusjonskode", "\u00c5rstall"),
                  list("\u00c5rstall" = c("top", top),
                       Institusjonskode = institusjoner),
                  "doktorgrader_samarbeid", 100,
                  c("\u00c5rstall", "Institusjonskode (arbeidsgiver)"),
                  list("\u00c5rstall" = c("top", top),
                       "Institusjonskode (arbeidsgiver)" = institusjoner),
                  "PKU", 98, NULL, list("\u00c5rstall" = c("top", top)),
                  "institusjoner", 211, NULL, NULL,
  )
}



#' Laster ned alle finsystabellene
#'
#' @description Bruker rdbhapi pakke for å
#' laste ned alle tabellene som vi trenger for
#' videre beregning
#'
#' @param top velg årstall
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


finsys_dbh_import <- function(top,
                              institusjoner = .finsys_dbh_inst()) {
  local({
    
    res <- purrr::pmap(.finsys_dbh_query(top, institusjoner),
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

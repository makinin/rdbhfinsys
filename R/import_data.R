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

#' Inneholder satser for resultatbaser uttelling og rammer data i Blåtthefte
#' @description Funksjon returnerer en vektor med satser og rammer
#' verdi for indikator og
#' budsjettår
#'
#' @export
#' @return a vector of institutions
#' @keywords internal
.finsys_dbh_indikatorsatser <- function(){
  tibble::tribble(~ budsjettar, ~ indikator, ~ kategori, ~ sats_rb, ~ ramme,
                  2022, "studiepoeng", "A", 145100, NA,
                  2022, "studiepoeng", "B", 111250, NA,
                  2022, "studiepoeng", "C", 74200, NA,
                  2022, "studiepoeng", "D", 54000, NA,
                  2022, "studiepoeng", "E", 43900, NA,
                  2022, "studiepoeng", "F", 37100, NA,
                  2022, "kandidater", "A", 110250, NA,
                  2022, "kandidater", "B", 83200, NA,
                  2022, "kandidater", "C", 55050, NA,
                  2022, "kandidater", "D", 41550, NA,
                  2022, "kandidater", "E", 33700, NA,
                  2022, "kandidater", "F", 28050, NA,
                  2022, "doktorgrader", NA, 423900, NA,
                  2022, "utveksling", "ordinar", 11300, NA,
                  2022, "utveksling", "Erasmus+", 16900, NA,
                  2022, "publisering", NA, NA,636255000,
                  2022, "EU", NA, NA, 577383000,
                  2022, "NFR", NA, NA, 346435000,
                  2022, "BOA", NA, NA, 346564000,
                  2021, "studiepoeng", "A", 141400, NA,
                  2021, "studiepoeng", "B", 108450, NA,
                  2021, "studiepoeng", "C", 72300, NA,
                  2021, "studiepoeng", "D", 52650, NA,
                  2021, "studiepoeng", "E", 42800, NA,
                  2021, "studiepoeng", "F", 36150, NA,
                  2021, "kandidater", "A", 107450, NA,
                  2021, "kandidater", "B", 81100, NA,
                  2021, "kandidater", "C", 53650, NA,
                  2021, "kandidater", "D", 40500, NA,
                  2021, "kandidater", "E", 32850, NA,
                  2021, "kandidater", "F", 27350, NA,
                  2021, "doktorgrader", NA, 413150, NA,
                  2021, "utveksling", "ordinar", 11000, NA,
                  2021, "utveksling", "Erasmus+", 16450, NA,
                  2021, "publisering", NA, NA, 620132000,
                  2021, "EU", NA, NA, 562751000,
                  2021, "NFR", NA, NA, 337656000,
                  2021, "BOA", NA, NA, 337782000,
                  2020, "studiepoeng", "A", 137000, NA,
                  2020, "studiepoeng", "B", 105100, NA,
                  2020, "studiepoeng", "C", 70050, NA,
                  2020, "studiepoeng", "D", 51000, NA,
                  2020, "studiepoeng", "E", 41450, NA,
                  2020, "studiepoeng", "F", 35050, NA,
                  2020, "kandidater", "A", 104100, NA,
                  2020, "kadidater", "B", 78600, NA,
                  2020, "kandidater", "C", 52000, NA,
                  2020, "kandidater", "D", 39250, NA,
                  2020, "kandidater", "E", 31850, NA,
                  2020, "kandidater", "F", 26500, NA,
                  2020, "doktorgrader", NA, 400350, NA,
                  2020, "utveksling", "Erasmus+", 15950, NA,
                  2020, "utveksling", "ordinar", 10650, NA,
                  2020, "publisering", NA, NA, 600903000,
                  2020, "EU", NA, NA, 545301000,
                  2020, "NFR", NA, NA, 327186000,
                  2020, "BOA", NA, NA, 327308000,
                  2019, "studiepoeng", "A", 132750, NA,
                  2019, "studiepoeng", "B", 101850, NA,
                  2019, "studiepoeng", "C", 67900, NA,
                  2019, "studiepoeng", "D", 49400, NA,
                  2019, "studiepoeng", "E", 40150, NA,
                  2019, "studiepoeng", "F", 33950, NA,
                  2019, "kandidater", "A", 100850, NA,
                  2019, "kandidater", "B", 76150, NA,
                  2019, "kandidater", "C", 50400, NA,
                  2019, "kandidater", "D", 38050, NA,
                  2019, "kandidater", "E", 30850, NA,
                  2019, "kandidater", "F", 25700, NA,
                  2019, "doktorgrader", NA, 387950, NA,
                  2019, "utveksling", "Erasmus+", 15450, NA,
                  2019, "utveksling", "ordinar", 10300, NA,
                  2019, "publisering", NA, NA, 581231000,
                  2019, "EU", NA, NA, 528392000,
                  2019, "NFR", NA, NA, 317035000,
                  2019, "BOA", NA, NA, 317035000,

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

#' Inneholder årsendring for berigning i Blåtthefte
#' @description Funksjon returnerer en vektor med årsendring
#' per budsjettår
#'
#'
#' @return a tibble budsjettar, arsendring_pst
#' @keywords internal
.finsys_dbh_arsendring <- function(){
  tibble::tribble (~budsjettar, ~arsendring_pst,
                   2019, 2.9,
                   2020, 3.2,
                   2021, 3.2,
                   2022, 3.2,
                   2023, 1,

  )
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

.finsys_dbh_query <- function(arstall, institusjoner = .finsys_dbh_inst()) {
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
                              institusjoner = .finsys_dbh_inst()) {
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

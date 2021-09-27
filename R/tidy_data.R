#' DBH_API institusjoner
#'
#' @description Hjelper funksjon med institusjoner fra DBH API
#' @param .data a tibble
#' @param finsys_dbh a tibble
#' @return a tibble
#'
#' @importFrom rdbhapi dbh_data
#' @importFrom dplyr rename_with
#' @importFrom stringr str_to_lower
#' @importFrom dplyr rename
#' @keywords internal

.finsys_dbh_institutions <- function(.data, finsys_dbh) {

  finsys_dbh$institusjoner %>%

    dplyr::rename(institusjonskode_nyeste =
                    enc2utf8("institusjonskode (sammensl\u00e5tt)"),
                  institusjonsnavn_nyeste =
                    enc2utf8("sammensl\u00e5tt navn"))

}

#' DBH_API studiepoeng
#' @description Hjelp funksjon for studiepoeng
#'
#' @param .data a tibble
#' @param finsys_dbh a tibble
#'
#' @return a tibble
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr across
#' @importFrom dplyr filter
#' @importFrom stringr str_to_upper
#' @keywords internal
#'
.finsys_dbh_studiepoeng <- function(.data, finsys_dbh) {
  finsys_dbh$studiepoeng %>%
    dplyr::rename(indikatorverdi =
                    .data$`ny produksjon egentfin`) %>%
    # Bruker finansieringskategorien basert på studentens
    #tilhørighet for BI og for emnet ellers
    dplyr::mutate(kategori =
                    dplyr::case_when(institusjonskode == "8241" ~
                                       .data$`finmodkode student`,
                                     TRUE ~
                                       .data$`finmodekode emne`)) %>%
    dplyr::mutate(dplyr::across(c("studentkategori", "kategori"),
                                stringr::str_to_upper)) %>%
    dplyr::filter(kategori %in% LETTERS[1:6],
                  .data$studentkategori == "S")

}
#' DBH-kandidater
#' @description  Hjelp funksjon for kandidater
#'
#' @param .data a tibble
#' @param finsys_dbh a tibble
#'
#' @return a tibble
#' @importFrom tidyr replace_na
#' @importFrom tidyr pivot_longer
#' @importFrom readr parse_integer
#' @importFrom dplyr rename
#' @importFrom dplyr mutate
#' @importFrom dplyr across
#' @importFrom dplyr select
#'
#' @keywords internal
#'
.finsys_dbh_kandidater <- function(.data, finsys_dbh) {
  finsys_dbh$kandidater %>%
    dplyr::rename(faktor =
                    .data$`uttelling kode:1=enkel, 2=dobbel`,
                  etterrapportert = .data$etterrapp,
                  innpasset = .data$`dobbel til enkel`,
                  kategori = .data$`finansierings-kategori`) %>%
    dplyr::mutate(dplyr::across("kategori", stringr::str_to_upper)) %>%
    dplyr::mutate(dplyr::across("faktor",
                                readr::parse_integer)) %>%
    dplyr::mutate(dplyr::across(c("etterrapportert", "innpasset"),
                                tidyr::replace_na, 0))  %>%
    dplyr::mutate(ordinar =
                    .data$totalt - .data$etterrapportert - .data$innpasset) %>%
    dplyr::select(-.data$totalt) %>%
    tidyr::pivot_longer(c(.data$ordinar,
                          .data$etterrapportert, .data$innpasset),
                        names_to = "kandidatgruppe",
                        values_to = "indikatorverdi")
}

#' DBH_API utveksling
#' @description  Hjelp funksjon for utveksling studenter
#'
#' @param .data a tibble
#' @param finsys_dbh a tibble
#'
#' @return a tibble
#' @keywords internal
#' @importFrom dplyr filter
#' @importFrom dplyr case_when
#' @importFrom tidyr drop_na



.finsys_dbh_utveksling <- function(.data, finsys_dbh) {
  finsys_dbh$utveksling %>%
    dplyr::rename(indikatorverdi = `antall totalt`) %>%

    dplyr::mutate(dplyr::across(c("utvekslingsavtale", "type", "niv\u00e5kode"),
                                stringr::str_to_upper)) %>%
    dplyr::filter("niv\u00e5kode" != "FU") %>%
    dplyr::mutate(kategori =
    dplyr::case_when(utvekslingsavtale == "ERASMUS+" & type == "NORSK"
                                     ~ "Erasmus+",
                                     utvekslingsavtale != "INDIVID"
                                     ~ "ordinar")) %>%
    tidyr::drop_na(kategori)
}

#' DBH_API doktorgrader
#' Help function for doktorgrader kandidater
#'
#' @param .data a tibble
#' @param finsys_dbh a tibble
#'
#' @return a tibble
#'
#' @importFrom purrr map_dfr
#' @importFrom stringr str_replace_all
#' @keywords internal
#'
.finsys_dbh_doktorgarder <- function(.data, finsys_dbh) {
  list(ordinar = finsys_dbh$doktorgrader,
       `samarbeids-ph.d.` = finsys_dbh$doktorgrader_samarbeid,
       PKU = finsys_dbh$PKU) %>%
    purrr::map_dfr(~stats::setNames(., stringr::str_replace_all(names(.),
                                  c("antall($| totalt)" = "indikatorverdi",
                                 "institusjonskode.*" = "institusjonskode"))),
                   .id = "kandidatgruppe") %>%
    dplyr::mutate(faktor = dplyr::case_when(kandidatgruppe ==
                                     "samarbeids-ph.d." ~ 0.2))

}

#' DBH_API publiseringspoeng
#' @description Hjelp funksjon for publisering
#'
#' @param .data a tibble
#' @param finsys_dbh a tibble
#'
#' @return a tibble
#' @keywords internal
#'
.finsys_dbh_publisering <- function(.data, finsys_dbh) {
  finsys_dbh$publisering %>%
    dplyr::rename(indikatorverdi = .data$publiseringspoeng)
}
#' DBH-API økonomidata
#' @description Hjelp funksjon for økonomi data
#'
#' @param .data a tibble
#' @param finsys_dbh a tibble
#'
#' @return a tibble
#' @importFrom purrr map2_dbl
#' @keywords internal

.finsys_dbh_ekonomi <- function(.data, finsys_dbh) {
  finsys_dbh$ekonomi %>%
    dplyr::rename(EU = .data$eu) %>%
    dplyr::mutate(NFR =
               purrr::map2_dbl(.data$nfr, .data$rff, sum, na.rm = TRUE),
                  BOA = purrr::map2_dbl(.data$bidragsinntekter,
                   .data$oppdragsinntekter, sum, na.rm = TRUE))


}
#' Prisjustering
#' @description Hjep funksjon for beregning av årsdeflator
#'
#' @return a tibble
#' @importFrom dplyr lead
#' @export

finsys_dbh_prisjustering <- function() {
  prisjustering <- prisjustering %>%
    dplyr::arrange(budsjettar) %>%
    dplyr::mutate(arsdeflator = 1 / (dplyr::lead(arsendring_pst) / 100 + 1))
}

#' Lager samletabell og beregner endringer og uttelling
#'
#' @description Bruker import data fra DBH-API og hjelper funksjoner
#' for å lage en samletabell med kategori, kandidatgruppe, faktor,
#' idikatorverdi, indikatorendring, budsjettendring som skal brukes
#' videre for å lage produksjonstableller
#'
#' @param arstall velg årstall
#' @param .data a tibble
#' @return a tibble
#' @export
#'
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom dplyr anti_join
#' @importFrom dplyr semi_join
#' @importFrom dplyr left_join
#' @importFrom tidyr complete
#' @importFrom tidyr nesting
#' @importFrom tidyr full_seq
#' @importFrom dplyr lag

finsys_dbh_tidy <- function(arstall, .data) {



  finsys_dbh <- finsys_dbh_import(arstall = arstall)

  finsys <- finsys_dbh

  finsys$studiepoeng <- .finsys_dbh_studiepoeng(finsys_dbh = finsys_dbh)

  finsys$kandidater <- .finsys_dbh_kandidater(finsys_dbh = finsys_dbh)

  finsys$utveksling <- .finsys_dbh_utveksling(finsys_dbh = finsys_dbh)

  finsys$doktorgrader <- .finsys_dbh_doktorgarder(finsys_dbh = finsys_dbh)

  finsys$publisering <- .finsys_dbh_publisering(finsys_dbh = finsys_dbh)

  finsys$ekonomi <- .finsys_dbh_ekonomi(finsys_dbh = finsys_dbh)

  finsys$institusjoner <- .finsys_dbh_institutions(finsys_dbh = finsys_dbh)
  prisjustering <- finsys_dbh_prisjustering()

  finsys <-
    c("EU", "NFR", "BOA") %>%
    stats::setNames(., .) %>%
    purrr::map(~dplyr::select(finsys$ekonomi,
                              c("institusjonskode", "\u00e5rstall",
                                "indikatorverdi" = .))) %>%

    c(finsys)
  finsys_data <-

    finsys[c("studiepoeng", "kandidater", "utveksling",
             "doktorgrader", "publisering", "EU", "NFR", "BOA")] %>%

    dplyr::bind_rows(.id = "indikator") %>%
    dplyr::rename(arstall = "\u00e5rstall") %>%
    dplyr::mutate(budsjettar = arstall + 2) %>%

    dplyr::group_by(.data$budsjettar,
                    .data$institusjonskode,
                    .data$indikator,
                    .data$kategori,
                    .data$kandidatgruppe,
                    .data$faktor)  %>%

    dplyr::summarise(dplyr::across("indikatorverdi", sum, na.rm = TRUE),
                     .groups = "drop") %>%

    # Legger til endringstall fra året før


    tidyr::complete(budsjettar = tidyr::full_seq(.data$budsjettar, 1),
                    .data$institusjonskode,
                    tidyr::nesting(.data$indikator,
                                   .data$kategori,
                                   .data$kandidatgruppe,
                                   .data$faktor),
                    fill = list(indikatorverdi = 0)) %>%

    # Legger til endringstall fra året før
    dplyr::group_by(
      .data$institusjonskode,
      .data$indikator,
      .data$kategori,
      .data$kandidatgruppe,
      .data$faktor) %>%
    dplyr::arrange(.data$budsjettar) %>%
    dplyr::mutate(indikatorendring = .data$indikatorverdi -
                    dplyr::lag(.data$indikatorverdi)) %>%

    dplyr::anti_join(unntak,
                     by = c("institusjonskode", "budsjettar", "indikator")) %>%
    # Legger til satser og beregner budsjettendring
    #for indikatorer med åpen ramme
    dplyr::left_join(indikatorsatser,
                     by = c("budsjettar", "indikator", "kategori")) %>%
    dplyr::left_join(tilskuddsgrad,
                     by = c("institusjonskode", "budsjettar", "indikator")) %>%

    dplyr::mutate(budsjettendring =
                    .data$indikatorendring *
                    .data$sats_rb *
                    tidyr::replace_na(.data$faktor, 1) *
                    tidyr::replace_na(.data$tilskuddsgrad, 1)) %>%
    # Legger til historisk uttelling for indikatorer med lukket ramme
    dplyr::left_join(dplyr::left_join(dplyr::filter(uttelling_historisk,
                                             indikator %in% c("publisering", "BOA", "EU", "NFR")),
                                      prisjustering,
                                      by = "budsjettar") %>%
                       dplyr::mutate(uttelling_prisjustert = uttelling / arsdeflator) %>%
                       dplyr::select(.data$budsjettar,
                                     .data$institusjonskode, .data$indikator, .data$uttelling_prisjustert),
                     by = c("budsjettar", "institusjonskode", "indikator")) %>%

    # Beregner satser og uttelling for indikatorer med lukket ramme
    dplyr::group_by(.data$budsjettar, .data$indikator) %>%
    dplyr::mutate(sats_rb =
                    dplyr::case_when(is.na(.data$sats_rb) ~
                                       .data$ramme / sum(.data$indikatorverdi),
                                     TRUE ~ .data$sats_rb))%>%
    dplyr::group_by(.data$institusjonskode,
                    .data$indikator,
                    .data$kategori,
                    .data$kandidatgruppe,
                    .data$faktor) %>%
    dplyr::arrange(.data$budsjettar)%>%
    dplyr::mutate(budsjettendring =
                    dplyr::coalesce(.data$budsjettendring,
                                    (.data$indikatorverdi * .data$sats_rb) -
                                      tidyr::replace_na(lag(.data$uttelling_prisjustert), 0)
                                     )
                  )
  finsys$institusjoner <- finsys$institusjoner %>%

    dplyr::semi_join(finsys_data, by = "institusjonskode") %>%
    dplyr::left_join(dplyr::select(finsys$institusjoner,
                                   institusjonskode, kortnavn_nyeste = kortnavn),
                     by = c("institusjonskode_nyeste" = "institusjonskode"))

  finsys_data <- finsys_data %>%
    dplyr::left_join(dplyr::select(finsys$institusjoner,
                                   institusjonskode,
                                   institusjonskode_nyeste, institusjonsnavn_nyeste, kortnavn_nyeste),
                     by = "institusjonskode")


  finsys_data


}


#' Beregner budsjettendringstall per indikator
#' @description Bruker alternativ avrundringsfunskjon,
#' med ulik behandling av avrundingsfeil
#' på 0.5n (afz = away from zero, 1.5 ≈ 2, -2.5 ≈ -3).
#' Funksjonen base::round
#' bruker såkalt "banker's rounding" hvor det rundes bort
#' fra oddetall (1.5 ≈ 2, -2.5 ≈ -2).
#'
#' @param x value
#' @param digits decimal places
#' @keywords internal
#'
.round_afz <- function(x, digits = 0) {
  scale <- 10^digits
  return(sign(x) * trunc((abs(x) * scale) + 0.5) / scale)
}

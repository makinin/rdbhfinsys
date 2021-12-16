globalVariables(c("."))
#' Finsieringsystemet data
#' @description Filtrerer finsystabeller
#' til hva som gir uttelling, og harmoniserer
#' variabelnavn og -verdier til sammenslåing
#' @param arstall a tibble
#'
#' @return a tibble
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr case_when
#' @importFrom dplyr across
#' @importFrom dplyr filter
#' @importFrom stringr str_to_upper
#' @importFrom tidyr replace_na
#' @importFrom tidyr pivot_longer
#' @importFrom readr parse_integer
#' @importFrom dplyr rename
#' @importFrom dplyr select
#' @importFrom tidyr drop_na
#' @importFrom tidyr replace_na
#' @importFrom purrr map_dfr
#' @importFrom stringr str_replace_all
#' @importFrom stats setNames
#' @importFrom purrr map2_dbl
#' @importFrom rlang .data
#' @keywords internal
#' @export
#'
.finsys_dbh_data <- function(arstall) {
  finsys_dbh <- finsys_dbh_import(arstall  = arstall)
  finsys <- finsys_dbh
# studiepoeng data
  finsys$studiepoeng <- finsys_dbh$studiepoeng %>%
    dplyr::rename(indikatorverdi =
    .data$`ny produksjon egentfin`) %>%
    # Bruker finansieringskategorien basert på studentens
    #tilhørighet for BI og for emnet ellers
    dplyr::mutate(kategori =
    dplyr::case_when(institusjonskode == "8241" ~
    `finmodkode student`, TRUE ~
    `finmodekode emne`)) %>%
    dplyr::mutate(dplyr::across(c("studentkategori", "kategori"),
    stringr::str_to_upper)) %>%
    dplyr::filter(.data$kategori %in% LETTERS[1:6],
    .data$studentkategori == "S")
#kandidater data
  finsys$kandidater <- finsys_dbh$kandidater %>%
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
  dplyr::select(- .data$totalt) %>%
  tidyr::pivot_longer(c(.data$ordinar,
   .data$etterrapportert, .data$innpasset),
  names_to = "kandidatgruppe",
  values_to = "indikatorverdi")
#utveksling data
  finsys$utveksling <- finsys_dbh$utveksling %>%
    dplyr::rename(indikatorverdi = .data$`antall totalt`) %>%

    dplyr::mutate(dplyr::across(c("utvekslingsavtale", "type", "niv\u00e5kode"),
    stringr::str_to_upper)) %>%
    dplyr::filter("niv\u00e5kode" != "FU") %>%
    dplyr::mutate(kategori =
    dplyr::case_when(utvekslingsavtale == "ERASMUS+" & type == "NORSK"
     ~ "Erasmus+",
    utvekslingsavtale != "INDIVID"
    ~ "ordinar")) %>%
    tidyr::drop_na(.data$kategori)
#doktorgrader data
  finsys$doktorgrader <- list(ordinar = finsys_dbh$doktorgrader,
    samarbeids_phd = finsys_dbh$doktorgrader_samarbeid,
    PKU = finsys_dbh$PKU) %>%
    purrr::map_dfr(~stats::setNames(., stringr::str_replace_all(names(.),
    c("antall($| totalt)" = "indikatorverdi",
    "institusjonskode.*" = "institusjonskode"))),
                   .id = "kandidatgruppe") %>%
    dplyr::mutate(faktor = dplyr::case_when(kandidatgruppe ==
    "samarbeids-ph.d." ~ 0.2))
  finsys$publisering <- finsys_dbh$publisering %>%
    dplyr::rename(indikatorverdi = .data$publiseringspoeng)

  #økonomi data
  finsys$ekonomi <- finsys_dbh$ekonomi %>%
    dplyr::rename(EU = .data$eu) %>%
    dplyr::mutate(NFR =
    purrr::map2_dbl(.data$nfr, .data$rff, sum, na.rm = TRUE),
    BOA = purrr::map2_dbl(.data$bidragsinntekter,
  .data$oppdragsinntekter, sum, na.rm = TRUE))

  finsys$institusjoner <- finsys_dbh$institusjoner %>%

    dplyr::rename(institusjonskode_nyeste =
    enc2utf8("institusjonskode (sammensl\u00e5tt)"),
    institusjonsnavn_nyeste =
    enc2utf8("sammensl\u00e5tt navn"))

  finsys <-
    c("EU", "NFR", "BOA") %>%
    stats::setNames(., .) %>%
    purrr::map(~dplyr::select(finsys$ekonomi,
       c("institusjonskode", "\u00e5rstall",
         "indikatorverdi" = all_of(.)))) %>%

    c(finsys)

}

#' Finsieringsystemet indikator og indikatorendring verdier
#' @description Legger til endringstall fra året før
#' @param arstall a tibble
#'
#' @return a tibble
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom tidyr complete
#' @importFrom tidyr full_seq
#' @importFrom tidyr nesting
#' @importFrom dplyr arrange
#' @importFrom dplyr lag
#' @importFrom dplyr semi_join
#' @importFrom dplyr left_join
#' @importFrom dplyr ungroup
#' @keywords internal
#' @export
#'

finsys_dbh_tidy <- function(arstall){
  arstall <- arstall
  finsys<- .finsys_dbh_data(arstall = arstall)
  finsys_data <- finsys[c("studiepoeng", "kandidater", "utveksling", "doktorgrader",
  "publisering", "EU", "NFR", "BOA")] %>%

    dplyr::bind_rows(.id = "indikator") %>%
    dplyr::rename(arstall = "\u00e5rstall") %>%
    dplyr::mutate(budsjettar = .data$arstall + 2) %>%

    dplyr::group_by(.data$budsjettar,
                    .data$institusjonskode,
                    .data$indikator,
                    .data$kategori,
                    .data$kandidatgruppe,
                    .data$faktor) %>%

    dplyr::summarise(dplyr::across("indikatorverdi", sum, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::ungroup() %>%

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
                    dplyr::lag(.data$indikatorverdi))

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

#' Prisjustering
#' @description Hjep funksjon for beregning av årsdeflator
#' @return a tibble
#' @importFrom dplyr lead
#' @export
#' @keywords internal

.finsys_dbh_prisjustering <- function() {
  .finsys_dbh_arsendring() %>%
    dplyr::arrange(.data$budsjettar) %>%
    dplyr::mutate(arsdeflator = 1 / (dplyr::lead(.data$arsendring_pst) / 100 + 1))
}

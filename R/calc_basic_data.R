
#' Lager tabeller med produksjonsdata, tilsvarende dem i Blått hefte
#'
#' @param df dataramme
#' @param filter_indikatorer indikatorene som skal inngå i tabellen
#' @param kolonne_vars variablene som skal spres på kolonner (indikator/kategori/osv.)
#' @param funs tilleggsfunksjon som skal anvendes på kolonnene (for å lage andelstall)
#' @importFrom dplyr vars
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom tidyr unite
#' @return a tibble
#' @export
#'

lag_produksjonstabell <- function(df,
filter_indikatorer, kolonne_vars, funs){
   if (is.null(funs)) {
funs <- identity
   }
   df %>%
   dplyr::filter(.data$indikator %in% filter_indikatorer) %>%
   tidyr::unite("kolonne_vars_samlet", kolonne_vars) %>%
   dplyr::group_by(across(c("institusjonskode_nyeste",
   "institusjonsnavn_nyeste",
   "kortnavn_nyeste",
   "kolonne_vars_samlet",
   "budsjettar"))) %>%
   dplyr::summarise(dplyr::across(c("indikatorverdi", "indikatorendring"), sum)) %>%
   dplyr::ungroup() %>%
   tidyr::pivot_wider(names_from =
   "kolonne_vars_samlet", values_from  = c("indikatorverdi", "indikatorendring") ) %>%
   dplyr::mutate(dplyr::across(-dplyr::matches("institusjon|navn|arstall"), funs))
}


#' Lager tabeller med produksjonsdata, tilsvarende dem i Blått hefte
#' @description funksjon for å lage tabeller med grunnlagsdata
#' @param df velg tibble gjerne output fra finsys_dbh_tidy
#' @return a tibble
#' @export
#'

finsys_produksjonstabeller <- function(df){

   tribble(~filter_indikatorer, ~kolonne_vars, ~funs,
   "studiepoeng", "kategori", NULL,
   "kandidater", c("kategori", "faktor"), NULL,
   c("doktorgrader", "utveksling"), c("indikator", "faktor", "kategori"), NULL,
   c("publisering", "EU", "NFR", "BOA"), c("indikator") ,
   list(pst = function(x) 100 * x / sum(x, na.rm = TRUE))) %>%


   pmap(function(...) {
      df %>%
  lag_produksjonstabell(...)
   })
}


#' Lager budsjettendring tabell
#' @description budsjettendring tabeller fra api data
#' @param df data set fra finsys_dbh_tidy
#' @return a tibble
#' @export
#' @importFrom dplyr if_else
#' @importFrom dplyr matches

finsys_budsjettendringer <- function(df){
   unntak <- .finsys_dbh_unntak()
   indikatorsatser <- .finsys_dbh_indikatorsatser()
   tilskuddsgrad <- .finsys_dbh_tilskuddsgrad()
   prisjustering <- .finsys_dbh_prisjustering()
   uttelling_historisk <- uttelling_historisk
   df %>%
   dplyr::anti_join(unntak,
   by = c("institusjonskode",  "indikator")) %>%
      # Legger til satser og beregner budsjettendring
      #for indikatorer med åpen ramme
      dplyr::left_join(indikatorsatser,
      by = c("budsjettar", "indikator", "kategori")) %>%
      dplyr::left_join(tilskuddsgrad,
      by = c("institusjonskode",  "indikator")) %>%
      dplyr::mutate(budsjettendring =
      .data$indikatorendring *
      .data$sats_rb *
      tidyr::replace_na(.data$faktor, 1) *
      tidyr::replace_na(.data$tilskuddsgrad, 1)) %>%

      # Legger til historisk uttelling for indikatorer med lukket ramme
      dplyr::left_join(dplyr::left_join
      (dplyr::filter(uttelling_historisk,
      .data$indikator %in% c("publisering", "BOA", "EU", "NFR")),
      prisjustering, by = "budsjettar") %>%
      dplyr::mutate(uttelling_prisjustert =
      .data$uttelling / .data$arsdeflator) %>%
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
      tidyr::replace_na(lag(.data$uttelling_prisjustert), 0))) %>%
      dplyr::group_by(.data$budsjettar,
                      .data$institusjonskode_nyeste,
                      .data$institusjonsnavn_nyeste,
                      .data$kortnavn_nyeste,
                      .data$indikator) %>%
      dplyr::summarise(budsjettendring_urund =
      sum(.data$budsjettendring)) %>%

      dplyr::mutate(budsjettendring =
      .round_afz(.data$budsjettendring_urund, -3)) %>%
      dplyr::mutate(budsjettendring = .data$budsjettendring / 1000)

}




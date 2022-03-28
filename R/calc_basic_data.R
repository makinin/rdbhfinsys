
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
                             "arstall"))) %>%
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





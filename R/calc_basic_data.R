
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
                                  filter_indikatorer,
                                  kolonne_vars,
                                  funs
) {
   if (is.null(funs)) {
      funs <- identity
   }
   df %>%
      dplyr::filter(indikator %in% filter_indikatorer) %>%
      tidyr::unite("kolonne_vars_samlet", kolonne_vars) %>%
      dplyr::group_by(across(c("institusjonskode_nyeste",
                    "institusjonsnavn_nyeste",
                    "kortnavn_nyeste",
                    "kolonne_vars_samlet",
                    "budsjettar"))) %>%
      dplyr::summarise(dplyr::across("indikatorverdi", sum)) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_wider(names_from = "kolonne_vars_samlet", values_from  = "indikatorverdi") %>%
      dplyr::mutate(dplyr::across(-dplyr::matches("institusjon|navn|arstall"), funs))
}


#' Lager tabeller med produksjonsdata, tilsvarende dem i Blått hefte
#' @description funksjon for å lage tabeller med grunnlagsdata
#' @param arstall velg årstall
#' @return a tibble
#' @export
#'

finsys_produksjonstabeller <- function(arstall){
   df <- finsys_dbh_tidy(arstall = arstall)
   tribble(~filter_indikatorer, ~kolonne_vars, ~funs,
           "studiepoeng", "kategori", NULL,
           "kandidater", c("kategori", "faktor"), NULL,
           c("doktorgrader", "utveksling"), c("indikator", "faktor", "kategori"), NULL,
           c("publisering", "EU", "NFR", "BOA"), c("indikator") ,
           list(pst = function(x) 100 * x / sum(x, na.rm = TRUE))) %>%


   pmap(function(...) {
      df %>%
         dplyr::filter(budsjettar == arstall + 2) %>%

         lag_produksjonstabell(...)
   })
}


#' Lager budsjettendring tabell
#' @description budsjettendring tabeller fra api data
#'
#' @param df data set fra finsys_dbh_tidy
#'
#' @return
#' @export
#'
#' @importFrom dplyr if_else
#' @importFrom dplyr matches
finsys_budsjettendringer <- function(df) {
   df %>%
      dplyr::group_by(budsjettar,
               institusjonskode_nyeste,
               institusjonsnavn_nyeste,
               kortnavn_nyeste,
               indikator) %>%
      dplyr::summarise(budsjettendring_urund = sum(budsjettendring)) %>%

      dplyr::mutate(budsjettendring = .round_afz(budsjettendring_urund, -3))

}




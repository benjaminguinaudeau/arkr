#' read_holding
#' @export
read_holding <- function(url){
  url %>%
    tabulizer::extract_tables(method = "stream") %>%
    purrr::map_dfr(tibble::as_tibble) %>%
    purrr::set_names(c("rank", head(t(as.matrix(.[1,])), -1))) %>%
    tail(-1) %>%
    janitor::clean_names() %>%
    dplyr::mutate_all(numerize) %>%
    dplyr::mutate(day = lubridate::today(tzone = "CET"))
}

#' read_holding
#' @export
get_holding <- function(etf = c("innovation", "genomic", "fintech", "space",
                                "israel", "internet", "robot", "printing")){
  url <- switch(
    etf,
    "innovation" = "https://ark-funds.com/wp-content/fundsiteliterature/holdings/ARK_INNOVATION_ETF_ARKK_HOLDINGS.pdf",
    "genomic" = "https://ark-funds.com/wp-content/fundsiteliterature/holdings/ARK_GENOMIC_REVOLUTION_MULTISECTOR_ETF_ARKG_HOLDINGS.pdf",
    "fintech" = "https://ark-funds.com/wp-content/fundsiteliterature/holdings/ARK_FINTECH_INNOVATION_ETF_ARKF_HOLDINGS.pdf",
    "space" = "https://ark-funds.com/wp-content/fundsiteliterature/holdings/ARK_SPACE_EXPLORATION_&_INNOVATION_ETF_ARKX_HOLDINGS.pdf",
    "israel" = "https://ark-funds.com/wp-content/fundsiteliterature/holdings/ARK_ISRAEL_INNOVATIVE_TECHNOLOGY_ETF_IZRL_HOLDINGS.pdf",
    "internet" = "https://ark-funds.com/wp-content/fundsiteliterature/holdings/ARK_NEXT_GENERATION_INTERNET_ETF_ARKW_HOLDINGS.pdf",
    "robot" = "https://ark-funds.com/wp-content/fundsiteliterature/holdings/ARK_AUTONOMOUS_TECHNOLOGY_&_ROBOTICS_ETF_ARKQ_HOLDINGS.pdf",
    "printing" = "https://ark-funds.com/wp-content/fundsiteliterature/holdings/THE_3D_PRINTING_ETF_PRNT_HOLDINGS.pdf"
    # Crypto/Mobility
  )

  read_holding(url)
}

#' numerize
#' @export
numerize <- function(x){
  if(!any(stringr::str_detect(x, "[^\\,\\.\\d]"))){
    as.numeric(stringr::str_remove_all(x, "\\,"))
  } else {
    x
  }
}

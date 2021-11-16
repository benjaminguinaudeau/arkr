#' read_holding
#' @export
read_holding <- function(url){
  tmp <- tempfile()

  res <- httr::GET(url,
                   httr::add_headers(`user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/90.0.4430.212 Safari/537.36 Edg/90.0.818.62"),
                   httr::timeout(2)
  )
  writeBin(res$content, tmp)

  tmp %>%
    tabulizer::extract_tables(method = "stream") %>% #bashR::simule_map(1)
    purrr::map_dfr(~{
      tmp <- tibble::as_tibble(.x) %>%
        dplyr::filter(V2 != "") %>%
        dplyr::glimpse() %>%
        purrr::set_names(t(as.matrix(.[1,]))) %>%
        tail(-1)

      if(ncol(tmp) == 6){
        names(tmp)[1] <- "Company"
      }
      return(tmp)
    }) %>%
    janitor::clean_names() %>%
    dplyr::mutate_all(numerize) %>%
    dplyr::mutate(stamp = lubridate::now(tzone = "CET"))
}

#' read_holding
#' @export
get_holding <- function(etf = c("innovation", "genomic", "fintech", "space",
                                "israel", "internet", "robot", "printing")){
  url <- switch(
    etf,
    "innovation" = "https://ark-funds.com/wp-content/uploads/funds-etf-pdf/ARK_INNOVATION_ETF_ARKK_HOLDINGS.pdf",
    "genomic" = "https://ark-funds.com/wp-content/uploads/funds-etf-pdf/ARK_GENOMIC_REVOLUTION_ETF_ARKG_HOLDINGS.pdf",
    "fintech" = "https://ark-funds.com/wp-content/uploads/funds-etf-pdf/ARK_FINTECH_INNOVATION_ETF_ARKF_HOLDINGS.pdf",
    "space" = "https://ark-funds.com/wp-content/uploads/funds-etf-pdf/ARK_SPACE_EXPLORATION_&_INNOVATION_ETF_ARKX_HOLDINGS.pdf",
    "israel" = "https://ark-funds.com/wp-content/uploads/funds-etf-pdf/ARK_ISRAEL_INNOVATIVE_TECHNOLOGY_ETF_IZRL_HOLDINGS.pdf",
    "internet" = "https://ark-funds.com/wp-content/uploads/funds-etf-pdf/ARK_NEXT_GENERATION_INTERNET_ETF_ARKW_HOLDINGS.pdf",
    "robot" = "https://ark-funds.com/wp-content/uploads/funds-etf-pdf/ARK_AUTONOMOUS_TECH._&_ROBOTICS_ETF_ARKQ_HOLDINGS.pdf",
    "printing" = "https://ark-funds.com/wp-content/uploads/funds-etf-pdf/THE_3D_PRINTING_ETF_PRNT_HOLDINGS.pdf"
    # Crypto/Mobility
  )
  url %>%
    read_holding() %>%
    dplyr::mutate(etf = paste("ark", etf, sep = "_"))
}

#' numerize
#' @export
numerize <- function(x){
  if(!any(stringr::str_detect(purrr::discard(x, is.na), "[^\\,\\.\\d]"))){
    as.numeric(stringr::str_remove_all(x, "\\,"))
  } else {
    x
  }
}

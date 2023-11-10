f_cs_repression_1 <- function(dat) {
  BAYES_SEED <- 864938  # From random.org
  
  model <- brm(
    bf(
      v2csreprss_delta_lead1 ~ negriovmean + loggdpcap +
        pct_changegdp1 + v2csreprss + revdem + leadermil + has_conflict +
        v2x_elecoff + v2xdd_dd + v2xeg_eqprotec + v2x_corr + v2x_clphy +
        count_chinacomem + count_russiacomem + coldwar +
        year_c + (1 + year_c | ccodecow),
      decomp = "QR"
    ),
    data = dat,
    chains = 4, warmup = 1000, iter = 2000, seed = BAYES_SEED
  )
  
  return(model)
}

f_ccsi_1 <- function(dat) {
  BAYES_SEED <- 112230  # From random.org
  
  model <- brm(
    bf(
      v2xcs_ccsi_delta_lead1 ~ negriovmean + loggdpcap +
        pct_changegdp1 + revdem + v2xcs_ccsi + leadermil + has_conflict +
        v2x_elecoff + v2xdd_dd + v2xeg_eqprotec + v2x_corr + v2x_clphy +
        count_chinacomem + count_russiacomem + coldwar +
        year_c + (1 + year_c | ccodecow),
      decomp = "QR"
    ),
    data = dat,
    chains = 4, warmup = 1000, iter = 2000, seed = BAYES_SEED
  )
  
  return(model)
}

f_freeexp_1 <- function(dat) {
  BAYES_SEED <- 511137  # From random.org
  
  model <- brm(
    bf(
      v2x_freexp_altinf_delta_lead1 ~ negriovmean + loggdpcap +
        pct_changegdp1 + revdem + v2x_freexp_altinf + leadermil + has_conflict +
        v2x_elecoff + v2xdd_dd + v2xeg_eqprotec + v2x_corr + v2x_clphy +
        count_chinacomem + count_russiacomem + coldwar +
        year_c + (1 + year_c | ccodecow),
      decomp = "QR"
    ),
    data = dat,
    chains = 4, warmup = 1000, iter = 2000, seed = BAYES_SEED
  )

  return(model)
}

f_civlib_1 <- function(dat) {
  BAYES_SEED <- 385912  # From random.org
  
  model <- brm(
    bf(
      v2x_civlib_delta_lead1 ~ negriovmean + loggdpcap +
        pct_changegdp1 + revdem + v2x_civlib + leadermil + has_conflict +
        v2x_elecoff + v2xdd_dd + v2xeg_eqprotec + v2x_corr + v2x_clphy +
        count_chinacomem + count_russiacomem + coldwar +
        year_c + (1 + year_c | ccodecow),
      decomp = "QR"
    ),
    data = dat,
    chains = 4, warmup = 1000, iter = 2000, seed = BAYES_SEED
  )

  return(model)
}

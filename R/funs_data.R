library(countrycode)


clean_cottiero_haggard <- function(path) {
  cottiero_haggard <- read_csv(path, show_col_types = FALSE) |> 
    mutate(coldwar = as.logical(coldwar))
  
  return(cottiero_haggard)
}

create_panel_skeleton <- function(cottiero_haggard_clean) {
  skeleton <- cottiero_haggard_clean |> 
    distinct(ccodecow, year) |> 
    mutate(
      country = countrycode(ccodecow, origin = "cown", destination = "country.name"),
      iso3 = countrycode(ccodecow, origin = "cown", destination = "iso3c"),
      region = countrycode(ccodecow, origin = "cown", destination = "region"),
      un_region = countrycode(ccodecow, origin = "cown", destination = "un.region.name"),
      un_subregion = countrycode(ccodecow, origin = "cown", destination = "un.regionsub.name")
    ) |> 
    arrange(ccodecow, year)
  
  return(skeleton)
}

clean_vdem <- function(path) {
  vdem_raw <- read_rds(path) |> as_tibble()

  vdem_clean <- vdem_raw |>
    filter(year >= 1940, year <= 2023) |>
    select(
      ccodecow = COWcode, year,

      # Civil society stuff
      v2csreprss, # CSO repression
      v2xcs_ccsi, # Core civil society index (entry/exit, repression, participatory env)
      e_miinterc, e_miinteco, e_civil_war,
      e_pt_coup, e_pt_coup_attempts, 
      v2x_elecoff, v2xdd_dd, v2xeg_eqprotec,

      # Human rights and politics
      # Political corruption index (less to more, 0-1) (public sector +
      # executive + legislative + judicial corruption)
      v2x_corr,
      v2x_rule, # Rule of law index

      # Rights indexes
      v2x_freexp_altinf, # Media freedom
      v2x_civlib, # Civil liberties index
      v2x_clphy, # Physical violence index
      v2x_clpriv, # Private civil liberties index
      v2x_clpol # Political civil liberties index
    ) |>
    group_by(ccodecow) |>
    mutate(across(starts_with("e_"), ~ifelse(is.na(.x), 0, .x))) |> 
    mutate(has_conflict = (e_miinterc + e_miinteco + e_civil_war) > 1) |> 
    mutate(
      across(
        c(v2csreprss, v2xcs_ccsi, v2x_freexp_altinf, v2x_civlib),
        list(
          lead1 = ~ lead(.x, n = 1),
          lead3 = ~ lead(.x, n = 3),
          lead5 = ~ lead(.x, n = 5),
          delta = ~ .x - lag(.x, n = 1),
          delta_lead1 = ~ lead(.x, n = 1) - .x,
          delta_lead3 = ~ lead(.x, n = 3) - .x,
          delta_lead5 = ~ lead(.x, n = 5) - .x
        )
      )
    ) |> 
    mutate(
      across(dplyr::contains("delta"), list(lag = ~ lag(.x, 2)))
    )

  return(vdem_clean)
}

cumprod_na <- function(x) {
  x[is.na(x)] <- 1
  return(cumprod(x))
}

cumsum_na <- function(x) {
  x[is.na(x)] <- 0
  return(cumsum(x))
}

make_final_data <- function(skeleton, cottiero_haggard_clean, vdem_clean) {
  final <- skeleton |>
    left_join(cottiero_haggard_clean, by = join_by(ccodecow, year)) |>
    left_join(vdem_clean, by = join_by(ccodecow, year)) |>
    
    # Center year at 1980 so that 1970 = -10, 2008 = 28, and so on
    mutate(year_c = year - 1980) |> 
    
    # Treatment variables
    group_by(ccodecow) |>
    mutate(
      across(
        c(negriovmean),
        list(
          lag1 = ~ lag(.x, n = 1),
          lag2 = ~ lag(.x, n = 2)
        )
      )
    ) |> 
    # Treatment history
    mutate(
      across(
        c(negriovmean_lag1, negriovmean_lag2),
        list(
          cumsum = ~ cumsum_na(.x)
        )
      )
    ) |> 
    ungroup()
    
  
  return(final)
}

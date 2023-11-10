library(targets)
library(tarchetypes)
library(crew)
suppressPackageStartupMessages(library(dplyr))

options(
  tidyverse.quiet = TRUE,
  dplyr.summarise.inform = FALSE
)

# Bayesian stuff
suppressPackageStartupMessages(library(brms))
options(
  mc.cores = 4,
  brms.backend = "cmdstanr",
  brms.threads = 2
)

set.seed(202228)  # From random.org

tar_option_set(
  packages = c("tidyverse"),
  # controller = crew_controller_local(workers = 2),  # Magical parallelization
  format = "qs",
  workspace_on_error = TRUE,
  workspaces = c()
)

# here::here() returns an absolute path, which then gets stored in tar_meta and
# becomes computer-specific (i.e. /Users/andrew/Research/blah/thing.Rmd).
# There's no way to get a relative path directly out of here::here(), but
# fs::path_rel() works fine with it (see
# https://github.com/r-lib/here/issues/36#issuecomment-530894167)
here_rel <- function(...) {fs::path_rel(here::here(...))}

# Run the R scripts in the R/ folder
tar_source()

# Pipeline ----------------------------------------------------------------
list(
  ## Raw data files ----
  tar_target(vdem_raw_file,
    here_rel("data", "raw_data", "V-Dem-CY-FullOthers_R_v13",
      "V-Dem-CY-Full+Others-v13.rds"),
    format = "file"),
  tar_target(cottiero_haggard_raw_file,
    here_rel("data", "raw_data", "ISQ_C_H_allreplication",
      "cottiero_haggard.csv"),
    format = "file"),
  
  ## Process and clean data ----
  tar_target(cottiero_haggard_clean, clean_cottiero_haggard(cottiero_haggard_raw_file)),
  tar_target(vdem_clean, clean_vdem(vdem_raw_file)),
  
  tar_target(skeleton, create_panel_skeleton(cottiero_haggard_clean)),
  
  tar_target(final_data, make_final_data(skeleton, cottiero_haggard_clean, vdem_clean)),
  
  ## Models ----
  tar_target(m_cs_repression_1, f_cs_repression_1(final_data)),
  tar_target(m_ccsi_1, f_ccsi_1(final_data)),
  tar_target(m_freeexp_1, f_freeexp_1(final_data)),
  tar_target(m_civlib_1, f_civlib_1(final_data)),

  NULL
)

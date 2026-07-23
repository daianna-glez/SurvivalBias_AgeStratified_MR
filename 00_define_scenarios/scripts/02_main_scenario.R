library(tidyr)
library(sessioninfo)

rm(list = ls())


# ==============================================================================
#   Scenario 2: Exposure effect on the outcome βx ≠ 0, no interaction βxA = 0 
# ==============================================================================
## Define dirs
input_dir <- paste(getwd(), "00_define_scenarios", "inputs", sep = "/")
out_dir <- paste(getwd(), "00_define_scenarios", "outputs", sep = "/")
plot_dir <- paste(getwd(), "00_define_scenarios", "plots", sep = "/")

dir.create(input_dir, recursive = T, showWarnings = F)
dir.create(out_dir, recursive = T, showWarnings = F)
dir.create(plot_dir, recursive = T, showWarnings = F)


## Scenarios from main scenario (βx = 0, βxA = 0)
scenarios01 <- get(load(paste0(out_dir, "/scenario.01.Rdata"), verbose = T))

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
#  Same base model: αɢ = 0.1, αu = 0.3, βu = 0.3, βA = 0.1, βx = 0.2, βxA = 0, 
#                   γo = 2, γx = -0.2, γA = -0.1, γxA = 0, γu = -0.1
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

## Define ranges for varying parameters
beta_Xs = c(-0.4, -0.2, 0.2, 0.4)

scenarios02 <- scenarios01
for(beta_X in beta_Xs){
  
    temp <- scenarios01
    temp$beta_X = beta_X
    scenarios02 <- rbind(scenarios02, temp)
}

scenarios02$main_scenario = "scenario.02"
scenarios02$main_scenario_val = scenarios02$beta_X

scenarios02[, 5:15] <- apply(scenarios02[, 5:15] , 2, as.numeric)
save(scenarios02, file = paste0(out_dir, "/scenario.02.Rdata"))

scenarios02_table <- scenarios02
write.table(scenarios02_table, file = paste0(out_dir, "/scenarios.02.csv"), col.names = F, row.names = F, quote = F, na = "NA", sep = "\t")







## Reproducibility info
session_info()

# ─ Session info ────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# setting  value
# version  R version 4.3.1 (2023-06-16)
# os       Rocky Linux 8.10 (Green Obsidian)
# system   x86_64, linux-gnu
# ui       RStudio
# language (EN)
# collate  en_US.UTF-8
# ctype    en_US.UTF-8
# tz       GB
# date     2026-07-22
# rstudio  2023.09.1+494 Desert Sunflower (server)
# pandoc   NA
# 
# ─ Packages ────────────────────────────────────────────────────────────────────────────────────────────────────────────────────
# !  package     * version date (UTC) lib source
# cli           3.6.5   2025-04-23 [1] CRAN (R 4.3.1)
# colorspace    2.1-1   2024-07-26 [2] CRAN (R 4.3.1)
# cowplot     * 1.1.3   2024-01-22 [2] CRAN (R 4.3.1)
# crayon        1.5.3   2024-06-20 [2] CRAN (R 4.3.1)
# VP dplyr         1.1.4   2026-04-03 [2] CRAN (R 4.3.1) (on disk 1.2.1)
# farver        2.1.2   2024-05-13 [2] CRAN (R 4.3.1)
# generics      0.1.3   2022-07-05 [2] CRAN (R 4.3.1)
# ggplot2     * 3.5.1   2024-04-23 [2] CRAN (R 4.3.1)
# glue          1.8.0   2024-09-30 [2] CRAN (R 4.3.1)
# gtable        0.3.6   2024-10-25 [2] CRAN (R 4.3.1)
# labeling      0.4.3   2023-08-29 [2] CRAN (R 4.3.1)
# VP lifecycle     1.0.4   2026-01-08 [2] CRAN (R 4.3.1) (on disk 1.0.5)
# magrittr      2.0.3   2022-03-30 [2] CRAN (R 4.3.1)
# munsell       0.5.1   2024-04-01 [2] CRAN (R 4.3.1)
# pillar        1.10.1  2025-01-07 [2] CRAN (R 4.3.1)
# pkgconfig     2.0.3   2019-09-22 [2] CRAN (R 4.3.1)
# purrr         1.0.4   2025-02-05 [2] CRAN (R 4.3.1)
# R6            2.6.1   2025-02-15 [2] CRAN (R 4.3.1)
# ragg          1.3.3   2024-09-11 [2] CRAN (R 4.3.1)
# VP rlang         1.1.5   2026-04-06 [2] CRAN (R 4.3.1) (on disk 1.2.0)
# rstudioapi    0.17.1  2024-10-22 [2] CRAN (R 4.3.1)
# scales        1.3.0   2023-11-28 [2] CRAN (R 4.3.1)
# sessioninfo * 1.2.2   2021-12-06 [2] CRAN (R 4.3.1)
# stringi       1.8.4   2024-05-06 [2] CRAN (R 4.3.1)
# stringr       1.5.1   2023-11-14 [2] CRAN (R 4.3.1)
# systemfonts   1.3.2   2026-03-05 [1] CRAN (R 4.3.1)
# textshaping   1.0.0   2025-01-20 [2] CRAN (R 4.3.1)
# tibble        3.2.1   2023-03-20 [2] CRAN (R 4.3.1)
# VP tidyr       * 1.3.1   2025-12-19 [2] CRAN (R 4.3.1) (on disk 1.3.2)
# tidyselect    1.2.1   2024-03-11 [2] CRAN (R 4.3.1)
# utf8          1.2.4   2023-10-22 [2] CRAN (R 4.3.1)
# VP vctrs         0.6.5   2026-03-21 [2] CRAN (R 4.3.1) (on disk 0.7.2)
# withr         3.0.2   2024-10-28 [2] CRAN (R 4.3.1)
# 
# [1] /home/dg745/R/x86_64-pc-linux-gnu-library/4.3
# [2] /usr/local/Cluster-Apps/R/4.3.1-icelake/lib64/R/library
# 
# V ── Loaded and on-disk version mismatch.
# P ── Loaded and on-disk path mismatch.
# 
# ───────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────────

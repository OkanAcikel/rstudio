required <- c(
  "readr","readxl","writexl","dplyr","tidyr","stringr","janitor","labelled",
  "psych","ggplot2","broom","car","lm.beta","pwr","MASS","emmeans"
)
to_install <- setdiff(required, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, quiet = TRUE)
invisible(lapply(required, library, character.only = TRUE))

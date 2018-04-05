############################
# Data formatting examples #
############################

library(PWFSLSmoke)
library(tidyr)
library(dplyr)
library(rlang)

monitorData <- PWFSLSmoke::Northwest_Megafires

## How to convert ws_monitor data to tidy format

monMeta <- as_tibble(monitorData[["meta"]])
monData <- as_tibble(monitorData[["data"]])

# TODO understand NSE: dplyr.tidyverse.org/articles/programming
tidyData_nse <-  monData %>%
  gather("monitorID", "pm25", -.data$datetime) %>%
  inner_join(monMeta, by = "monitorID")



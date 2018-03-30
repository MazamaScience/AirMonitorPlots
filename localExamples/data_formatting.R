############################
# Data formatting examples #
############################

library(PWFSLSmoke)
library(tidyr)

monitorData <- Northwest_Megafires 


## How to convert ws_monitor data to tidy format

monMeta = as_tibble(monitorData[['meta']])
monData = as_tibble(monitorData[['data']])

monitorTidy <-  monData %>% 
    gather(monitorID, pm25, -datetime) %>% 
    inner_join(monMeta, by = 'monitorID')
    

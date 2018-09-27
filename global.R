
#-----------------------------------------------------------------
# Library
#-----------------------------------------------------------------
# Shiny
library(shiny)
library(shinythemes)
library(DT)

# Data manipulation
library(tidyr)
library(dplyr)
library(openxlsx)
library(readr)
library(tidyquant)
library(lubridate)

# Data visualization
library(corrr)
library(corrplot)
library(RColorBrewer)
library(cowplot)
library(ggthemes)
library(gridExtra)
library(ggrepel)
library(ellipse)
library(scales)
library(ggpmisc)

# Models
library(PerformanceAnalytics)

options(shiny.maxRequestSize = 100*1024^2) 

#-----------------------------------------------------------------
# WD & Shiny Option
#-----------------------------------------------------------------

main.path <- getwd()
git.data.path <- paste0(main.path, "/data/")
setwd(git.data.path)

TOT_IDX_DATA <- read_rds("TOT_IDX_DATA.rds")
KB_IDX_RET_DATA <- read_rds("KB_IDX_RET_DATA.rds")

con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

match.idx <- read.csv(file = paste0(git.data.path, "MATCH_IDX.csv"), 
                      header = T) %>% 
  as_tibble() %>% mutate_if(is.factor, as.character) %>% select(-X)

etf.ticker.name <- match.idx %>% 
  select(TICKER_NAME) %>% arrange(TICKER_NAME) %>% pull

RAW_KB_ETF_LIST <- read.xlsx(xlsxFile = paste0(git.data.path, "KB_ETF_HOLD.xlsx"), 
                             sheet = 1, colNames = T) %>% as_tibble()



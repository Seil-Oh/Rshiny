
setwd("/data")

TOT_IDX_DATA <- read_rds("TOT_IDX_DATA.rds")
KB_IDX_RET_DATA <- read_rds("KB_IDX_RET_DATA.rds")

con = gzcon(url('http://www.systematicportfolio.com/sit.gz', 'rb'))
source(con)
close(con)

match.idx <- read.csv(file = "/data/MATCH_IDX.csv", 
                      header = T) %>% 
  as_tibble() %>% mutate_if(is.factor, as.character) %>% select(-X)

etf.ticker.name <- match.idx %>% 
  select(TICKER_NAME) %>% arrange(TICKER_NAME) %>% pull

RAW_KB_ETF_LIST <- read.xlsx(xlsxFile = "/data/KB_ETF_HOLD.xlsx", 
                             sheet = 1, colNames = T) %>% as_tibble()



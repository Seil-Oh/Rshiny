
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

#-----------------------------------------------------------------
# WD & Shiny Option
#-----------------------------------------------------------------

# setwd("D:/GoogleDrive/NPS/Rshiny/")
options(shiny.maxRequestSize = 100*1024^2) 
source("global.R")

#-----------------------------------------------------------------
# Server
#-----------------------------------------------------------------
server <- function(input, output, session) {
  
  output$table.fnguide <- DT::renderDataTable(DT::datatable({
    
    req(input$file.fnguide)
    
    tryCatch(
      {
        
        RAW_SHEETS <- getSheetNames(input$file.fnguide$datapath)
        
        data_modi_fnguide <- function(x) {
          
          # Getting Header Information Session
          INFO_DATA <- x %>%
            as_tibble() %>% slice(1:6) %>% t() %>% as_tibble() %>% slice(-1) %>%
            rename(TICKER = V1, TICKER_NAME = V2, ITEM_NUMBER = V4, ITEM_NAME = V5) %>%
            select(TICKER, TICKER_NAME, ITEM_NUMBER, ITEM_NAME)
          
          # Change wide format to long format
          MAIN_DATA <- x %>%
            as_tibble() %>% slice(-1:-6) %>%
            rename(DATE = X1) %>%
            mutate(DATE = as_date(DATE)) %>%
            mutate_if(is.character, as.numeric)
          
          colnames(MAIN_DATA)[-1] <- INFO_DATA %>% select(TICKER) %>% unlist
          
          # Merge raw data with header information
          MAIN_DATA <- MAIN_DATA %>%
            gather(TICKER, VALUE, -DATE) %>%
            na.omit() %>%
            left_join(INFO_DATA, by = "TICKER")
          
          return(MAIN_DATA)
          
        }
        
        for (i in seq_along(RAW_SHEETS)){
          
          # Load Raw WiseFn Data
          RW_DATA_NAME <- paste0("RAW_",RAW_SHEETS[i])
          cat("LOAD:", i, "/",length(RAW_SHEETS), "-", RW_DATA_NAME, "DATA SET", "\n")
          assign(RW_DATA_NAME,
                 read.xlsx(input$file.fnguide$datapath, sheet = i,
                           rows = c(9:7000), colNames = F, detectDates = T))
          
          # Modify Raw data
          MODI_DATA_NAME <- paste0("RAW_", RW_DATA_NAME)
          TEMP_DATA <- get(RW_DATA_NAME)
          assign(MODI_DATA_NAME, data_modi_fnguide(TEMP_DATA))
          
        }
        
        FN_IDX_DATA <- tibble()
        for (i in seq_along(RAW_SHEETS)) {
          
          DATA_LIST <- paste0("RAW_RAW_", RAW_SHEETS)
          FN_IDX_DATA <- bind_rows(FN_IDX_DATA, get(DATA_LIST[i]))
          
        }
        
        FN_IDX_DATA <- FN_IDX_DATA %>% 
          select(DATE, TICKER, TICKER_NAME, VALUE) %>% 
          mutate(SOURCE = "FNGUIDE")
        
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    
    return(FN_IDX_DATA)
    
  }))
  
  output$table.wisefn <- DT::renderDataTable(DT::datatable({
    
    req(input$file.wisefn)
    
    tryCatch(
      {
        
        RAW_SHEETS <- getSheetNames(input$file.wisefn$datapath)
        
        data_modi_wise <- function(x) {
          
          INFO_DATA <- x %>%
            as_tibble() %>% slice(1:5) %>% t() %>% as_tibble() %>% slice(-1) %>%
            rename(TICKER = V1, TICKER_NAME = V2, ITEM_NUMBER = V3, ITEM_NAME = V5) %>%
            select(TICKER, TICKER_NAME, ITEM_NUMBER, ITEM_NAME)
          
          # Change wide format to long format
          MAIN_DATA <- x %>%
            as_tibble() %>% slice(-1:-5) %>%
            mutate(X1 = as.Date(X1)) %>%
            mutate_if(is.character, as.numeric)
          
          colnames(MAIN_DATA)[-1] <- INFO_DATA %>% select(TICKER) %>% unlist
          
          # Merge raw data with header information
          MAIN_DATA <- MAIN_DATA %>%
            rename(DATE = X1) %>%
            gather(TICKER, VALUE, -DATE) %>%
            na.omit(PRICE) %>%
            left_join(INFO_DATA, by = "TICKER")
          
          return(MAIN_DATA)
          
        }
        
        for (i in seq_along(RAW_SHEETS)){
          
          # Load Raw WiseFn Data
          RW_DATA_NAME <- paste0("RAW_", RAW_SHEETS[i])
          cat("LOAD:", i, "/",length(RAW_SHEETS), "-", RW_DATA_NAME, "DATA SET", "\n")
          assign(RW_DATA_NAME,
                 read.xlsx(input$file.wisefn$datapath, sheet = i,
                           rows = c(8:11,14:5000), colNames = F, detectDates = T))
          
          # Modify Raw data
          MODI_DATA_NAME <- paste0("RAW_", RW_DATA_NAME)
          TEMP_DATA <- get(RW_DATA_NAME)
          assign(MODI_DATA_NAME, data_modi_wise(TEMP_DATA))
          
        }
        
        WISE_IDX_DATA <- tibble()
        for (i in seq_along(RAW_SHEETS)) {
          
          DATA_LIST <- paste0("RAW_RAW_", RAW_SHEETS)
          WISE_IDX_DATA <- bind_rows(WISE_IDX_DATA, get(DATA_LIST[i]))
          
        }
        
        WISE_IDX_DATA <- WISE_IDX_DATA %>% 
          select(DATE, TICKER, TICKER_NAME, VALUE) %>% 
          mutate(SOURCE = "WISEFN") 
        
        
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    
    return(WISE_IDX_DATA)
    
  }))
  
  output$table.etf.idx <- DT::renderDataTable(DT::datatable({
    
    req(input$file.etf.idx)
    
    tryCatch(
      {
        
        RAW_FILT_KB_ETF <- read.csv(file = input$file.etf.idx$datapath, 
                                    header = T) %>% 
          as_tibble() %>% 
          mutate_if(is.factor, as.character) %>% 
          select(-X)
        
      },
      error = function(e) {
        stop(safeError(e))
      }
    )
    
    return(RAW_FILT_KB_ETF)
    
  }))
  
  tot.data <- reactive({
    
    TMP_TOT_DATA <- KB_IDX_RET_DATA %>%
      left_join(match.idx, by = c("TICKER")) %>%
      filter(TICKER_NAME %in% etf.ticker.name,
             DATE >= input$etf.date.range[1] %>% as.character(),
             DATE <= input$etf.date.range[2] %>% as.character()) %>% 
      select(DATE, TICKER, TICKER_NAME_IDX, DAILY_RETURN_IDX) %>% 
      left_join(match.idx %>%
                  select(TICKER, ABBRE), by = "TICKER") %>%
      select(-TICKER, -TICKER_NAME_IDX) %>%
      rename(TICKER = ABBRE,
             DAILY_RETURN = DAILY_RETURN_IDX)
    
    M_TOT_RET_TIME <- TMP_TOT_DATA %>%
      spread(TICKER, DAILY_RETURN) %>%
      select(DATE) %>%
      mutate(DATE = as.character(DATE)) %>%
      unlist
    
    M_TOT_RET_DATA <- TMP_TOT_DATA %>%
      spread(TICKER, DAILY_RETURN) %>%
      select(-DATE) %>%
      as.matrix
    
    DAILY_RET_DATA <- zoo(M_TOT_RET_DATA, as.Date(M_TOT_RET_TIME))
    
    PORT_RET_DATA <- as.xts(DAILY_RET_DATA)
    
  })
  
  selected.data <- reactive({
    
    KB_IDX_RET_DATA %>%
      left_join(match.idx, by = c("TICKER")) %>%
      filter(TICKER_NAME %in% input$etf.ticker.list,
             DATE >= input$date.range[1] %>% as.character(),
             DATE <= input$date.range[2] %>% as.character()) %>% 
      select(DATE, TICKER, TICKER_NAME, TICKER_NAME_IDX, DAILY_RETURN_IDX)
    
  })
  
  selected.asset.data <- reactive({
    
    TMP_TOT_DATA <- selected.data() %>%
      select(DATE, TICKER, TICKER_NAME_IDX, DAILY_RETURN_IDX) %>% 
      left_join(match.idx %>%
                  select(TICKER, ABBRE), by = "TICKER") %>%
      select(-TICKER, -TICKER_NAME_IDX) %>%
      rename(TICKER = ABBRE,
             DAILY_RETURN = DAILY_RETURN_IDX)
    
    M_TOT_RET_TIME <- TMP_TOT_DATA %>%
      spread(TICKER, DAILY_RETURN) %>%
      select(DATE) %>%
      mutate(DATE = as.character(DATE)) %>%
      unlist
    
    M_TOT_RET_DATA <- TMP_TOT_DATA %>%
      spread(TICKER, DAILY_RETURN) %>%
      select(-DATE) %>%
      as.matrix
    
    DAILY_RET_DATA <- zoo(M_TOT_RET_DATA, as.Date(M_TOT_RET_TIME))
    
    PORT_RET_DATA <- as.xts(DAILY_RET_DATA)
    
  })
  
  output$table.test <- DT::renderDataTable(DT::datatable({
    
    req(selected.data())
    
    return(selected.data())
    
  }))
  
  output$etf.ret.plt <- renderPlot({
    
    req(selected.asset.data())
    
    PORT_RET_DATA <- selected.asset.data()
    
    charts.PerformanceSummary(
      PORT_RET_DATA,
      main = "Performance",
      colorset = rich12equal,
      lwd = 2)
    
  })
  
  output$etf.corr.plt <- renderPlot({
    
    req(selected.asset.data())
    
    PORT_RET_DATA <- selected.asset.data()
    COR_M <- cor(PORT_RET_DATA)
    
    corrplot(COR_M, method = "color", outline = T, addgrid.col = "darkgray", type = "upper",
             diag = T, mar = c(0,0,0,0),
             order="hclust", rect.col = "black", rect.lwd = 5,
             cl.pos = "b", tl.col = "indianred4", tl.cex = 1, cl.cex = 1,
             addCoef.col = "white", number.digits = 2, number.cex = 1)
    
  })
  
  output$table.ret <- DT::renderDataTable(DT::datatable({
    
    req(selected.asset.data())
    
    PORT_RET_DATA <- selected.asset.data()
    
    ret.table <- rbind(table.AnnualizedReturns(
      PORT_RET_DATA, scale = 252, geometric = T),
      Return.cumulative(PORT_RET_DATA, geometric = T)) %>% 
      rownames_to_column() %>% 
      gather(ticker, value, -rowname) %>% 
      spread(rowname, value) %>% 
      select(ticker, `Annualized Return`, 
             `Annualized Std Dev`, 
             `Annualized Sharpe (Rf=0%)`, 
             `Cumulative Return`)
    
    ret.res <- datatable(ret.table) %>% 
      formatPercentage(
        c("Annualized Return",
          "Annualized Std Dev",
          "Cumulative Return"), 
        digits = 2) %>% 
      formatRound(
        'Annualized Sharpe (Rf=0%)', 2
      ) %>% 
      formatStyle(
        'Annualized Sharpe (Rf=0%)',
        color = "dimgrey",
        background = styleColorBar(ret.table$`Annualized Sharpe (Rf=0%)`, 
                                   'lightsteelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    
    return(ret.res)
    
  }))
  
  output$etf.ret.risk.plt <- renderPlot({
    
    req(selected.asset.data())
    
    PORT_RET_DATA <- selected.asset.data()
    
    ret.table <- rbind(table.AnnualizedReturns(
      PORT_RET_DATA, scale = 252, geometric = T),
      Return.cumulative(PORT_RET_DATA, geometric = T))
    
    ret.table %>% 
      rownames_to_column() %>% 
      gather(ticker, value, -rowname) %>% 
      filter(rowname %in% c("Annualized Return","Annualized Std Dev")) %>%
      spread(rowname, value) %>% 
      rename(Return     = `Annualized Return`, 
             Volatility = `Annualized Std Dev`) %>%
      ggplot() +
      geom_point(aes(x = Volatility, y = Return, colour = factor(ticker))) +
      geom_label_repel(
        aes(x = Volatility, y = Return, fill = factor(ticker), label = ticker),
        size  = 4,
        color = "white",
        box.padding   = unit(0.35, "lines"),
        point.padding = unit(0.5, "lines"),
        segment.color = 'grey50'
      ) +
      theme_hc() +
      labs(fill = "") +
      theme(plot.title    = element_text(color = "#666666", face = "bold", size = 20, hjust = 0),
            plot.subtitle = element_text(color = "#666666", size = 15, hjust = 0),
            axis.title    = element_text(color = "#666666", face = "bold", size = 12),
            legend.position = "none", 
            text = element_text()) +
      scale_y_continuous(labels = percent, 
                         breaks = scales::pretty_breaks(n = 10)) +
      scale_x_continuous(labels = percent, 
                         breaks = scales::pretty_breaks(n = 5)) +
      geom_hline(yintercept = 0, size = 0.5) +
      geom_vline(xintercept = 0, size = 0.5) +
      scale_fill_tableau("tableau20") +
      scale_colour_tableau("tableau20")
    
  })
  
  output$table.etf.tot.ret <- DT::renderDataTable(DT::datatable({
    
    req(tot.data())
    
    PORT_RET_DATA <- tot.data()
    
    ret.table <- rbind(table.AnnualizedReturns(
      PORT_RET_DATA, scale = 252, geometric = T),
      Return.cumulative(PORT_RET_DATA, geometric = T)) %>% 
      rownames_to_column() %>% 
      gather(ticker, value, -rowname) %>% 
      spread(rowname, value) %>% 
      select(ticker, `Annualized Return`, 
             `Annualized Std Dev`, 
             `Annualized Sharpe (Rf=0%)`, 
             `Cumulative Return`)
    
    ret.res <- datatable(ret.table) %>% 
      formatPercentage(
        c("Annualized Return",
          "Annualized Std Dev",
          "Cumulative Return"), 
        digits = 2) %>% 
      formatRound(
        'Annualized Sharpe (Rf=0%)', 2
      ) %>% 
      formatStyle(
        'Annualized Sharpe (Rf=0%)',
        color = "dimgrey",
        background = styleColorBar(ret.table$`Annualized Sharpe (Rf=0%)`, 
                                   'lightsteelblue'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
    
    return(ret.res)
    
  }))
  
  output$plt.kb.etf.tot.ret <- renderPlot({
    
    req(tot.data())
    
    PORT_RET_DATA <- tot.data()
    
    ret.table <- rbind(table.AnnualizedReturns(
      PORT_RET_DATA, scale = 252, geometric = T),
      Return.cumulative(PORT_RET_DATA, geometric = T))
    
    ret.table %>% 
      rownames_to_column() %>% 
      gather(ticker, value, -rowname) %>% 
      filter(rowname %in% c("Annualized Return","Annualized Std Dev")) %>%
      spread(rowname, value) %>% 
      rename(Return     = `Annualized Return`, 
             Volatility = `Annualized Std Dev`) %>%
      left_join(match.idx %>% select(ABBRE, CLASS), 
                by = c("ticker"="ABBRE")) %>% 
      ggplot() +
      geom_point(aes(x = Volatility, y = Return, colour = factor(CLASS))) +
      geom_label_repel(
        aes(x = Volatility, y = Return, fill = factor(CLASS), label = ticker),
        size  = 3,
        color = "white",
        box.padding   = unit(0.35, "lines"),
        point.padding = unit(0.5, "lines"),
        segment.color = 'grey50'
      ) +
      theme_hc() +
      labs(fill = "") +
      theme(plot.title    = element_text(color = "#666666", face = "bold", size = 20, hjust = 0),
            plot.subtitle = element_text(color = "#666666", size = 15, hjust = 0),
            axis.title    = element_text(color = "#666666", face = "bold", size = 12),
            legend.position = "none",
            text = element_text()) +
      scale_y_continuous(labels = percent, 
                         breaks = scales::pretty_breaks(n = 20)) +
      scale_x_continuous(labels = percent, 
                         breaks = scales::pretty_breaks(n = 10)) +
      geom_hline(yintercept = 0, size = 0.5) +
      geom_vline(xintercept = 0, size = 0.5) +
      scale_fill_tableau(name = "") +
      scale_colour_tableau()
    
  })
  
  output$plt.kb.etf.corr <- renderPlot({
    
    req(tot.data())
    
    PORT_RET_DATA <- tot.data()
    COR_M <- cor(PORT_RET_DATA)
    
    corrplot(COR_M, method = "color", outline = T,
             addgrid.col = "darkgray", order="hclust", mar = c(.5,.5,.5,.5),
             rect.col = "black", rect.lwd = 5, cl.pos = "b", tl.col = "indianred4",
             tl.cex = 1, cl.cex = 1, number.cex = .75)
    
  })
  
  data.set.input <- eventReactive(input$opt.run, {
    
    req(selected.asset.data())
    
    DAILY_RET_DATA <- selected.asset.data()
    
  }, ignoreNULL = F)
  
  output$opt.table <- DT::renderDataTable(DT::datatable({
    
    DAILY_RET_DATA <- data.set.input()
    
    ia <- create.historical.ia(DAILY_RET_DATA, 252)
    
    n <- ia$n        
    
    constraints <- new.constraints(n, lb = 0, ub = 1)
    constraints <- add.constraints(diag(n), type = '>=', b = 0, constraints)
    constraints <- add.constraints(diag(n), type = '<=', b = 1, constraints)
    
    constraints <- add.constraints(rep(1, n), 1, type = '=', constraints)        
    
    ef <- portopt(ia, constraints, 50, 'Efficient Frontier') 
    
    weight_minvar <- min.var.portfolio(ia,constraints)  
    weight_maxsharpe <- max.sharpe.portfolio()(ia,constraints) 
    weight_ew <- rep(1,n)/n
    weight_rp <- risk.parity.portfolio()(ia, constraints)
    
    ew_ret <- weight_ew %*% t(ia$hist.returns) %>% t()
    colnames(ew_ret) <- "Equal.Weighted"
    mv_ret <- weight_minvar %*% t(ia$hist.returns) %>% t()
    colnames(mv_ret) <- "Minimum.Variance"
    ms_ret <- weight_maxsharpe %*% t(ia$hist.returns) %>% t()
    colnames(ms_ret) <- "Maximum.Sharpe.Ratio"
    rp_ret <- weight_rp %*% t(ia$hist.returns) %>% t()
    colnames(rp_ret) <- "Risk.Parity"
    
    ret_data <- cbind(ew_ret, mv_ret, ms_ret, rp_ret)
    
    ret.table <- rbind(table.AnnualizedReturns(
      ret_data, scale = 252, geometric = T, Rf = .02/252),
      Return.cumulative(ret_data, geometric = T)) %>% 
      rownames_to_column() %>% 
      gather(ticker, value, -rowname) %>% 
      spread(rowname, value) %>% 
      select(ticker, `Annualized Return`, 
             `Annualized Std Dev`, 
             `Annualized Sharpe (Rf=2%)`, 
             `Cumulative Return`) %>% 
      rename(Method = ticker)
    
    ret.res <- datatable(ret.table) %>% 
      formatPercentage(
        c("Annualized Return","Annualized Std Dev","Cumulative Return"), 
        digits = 2) %>% 
      formatRound(
        'Annualized Sharpe (Rf=2%)', 2
      ) %>% 
      formatStyle(
        'Annualized Sharpe (Rf=2%)',
        color = styleInterval(c(0), c('red', 'blue')),
        backgroundColor = styleInterval(0, c('lightcoral', 'lightskyblue'))
      )
    
    return(ret.res)
    
  }))
  
  output$opt.plot <- renderPlot({
    
    DAILY_RET_DATA <- data.set.input()
    
    ia <- create.historical.ia(DAILY_RET_DATA, 252)
    
    n <- ia$n
    
    constraints <- new.constraints(n, lb = 0, ub = 1)
    constraints <- add.constraints(diag(n), type = '>=', b = 0, constraints)
    constraints <- add.constraints(diag(n), type = '<=', b = 1, constraints)
    
    constraints <- add.constraints(rep(1, n), 1, type = '=', constraints)     
    
    ef <- portopt(ia, constraints, 50, 'Efficient Frontier')
    
    weight_minvar <- min.var.portfolio(ia,constraints)
    weight_maxsharpe <- max.sharpe.portfolio()(ia,constraints)
    weight_ew <- rep(1,n)/n
    weight_rp <- risk.parity.portfolio()(ia, constraints)
    
    ew_ret <- weight_ew %*% t(ia$hist.returns) %>% t()
    colnames(ew_ret) <- "EW_RET"
    mv_ret <- weight_minvar %*% t(ia$hist.returns) %>% t()
    colnames(mv_ret) <- "MV_RET"
    ms_ret <- weight_maxsharpe %*% t(ia$hist.returns) %>% t()
    colnames(ms_ret) <- "MS_RET"
    rp_ret <- weight_rp %*% t(ia$hist.returns) %>% t()
    colnames(rp_ret) <- "RP_RET"
    
    
    ret_data <- cbind(ew_ret, mv_ret, ms_ret, rp_ret)
    port_ret <- apply(ret_data, 2, function(x) prod(1+x)^(1/length(x))-1)
    port_ret <- (1+port_ret)^252-1
    port_vol <- apply(ret_data, 2, sd) * sqrt(252)
    
    PLT_DATA_MIN_MAX <- tibble(Ticker = c("Min_Var","Max_Sharpe","Risk_Parity"),
                               Risk   = c(portfolio.risk(weight_minvar, ia), 
                                          portfolio.risk(weight_maxsharpe, ia),
                                          portfolio.risk(weight_rp, ia)),
                               Return = c(portfolio.return(weight_minvar, ia),
                                          portfolio.return(weight_maxsharpe, ia),
                                          portfolio.return(weight_rp, ia)),
                               Class  = c("OPT"))
    
    eq.data <- tibble(Ticker = "Equal_Weight", 
                      Risk   = port_vol["EW_RET"], 
                      Return = port_ret["EW_RET"], 
                      Class  = "OPT")
    
    PLT_DATA_MIN_MAX <- bind_rows(PLT_DATA_MIN_MAX, eq.data)
    
    PLT_DATA_ETF <- tibble(Ticker = ia$symbols,
                           Return = ia$expected.return,
                           Risk   = ia$risk,
                           Class  = "ETF")
    PLT_DATA_EF <- tibble(Ticker  = paste0("Port_", seq(1:50)),
                          Return  = ef$return %>% as.numeric,
                          Risk    = ef$risk,
                          Class   = "Portfolio")
    
    PLT_DATA <- bind_rows(PLT_DATA_MIN_MAX, PLT_DATA_ETF, PLT_DATA_EF)
    PLT_DATA <- PLT_DATA %>% 
      left_join(match.idx %>% select(ABBRE, CLASS), 
                by = c("Ticker" = "ABBRE"))
    
    opt.plt.res <- ggplot() + 
      geom_point(data = PLT_DATA_MIN_MAX %>% filter(Class == "OPT"),
                 aes(x = Risk, y = Return, color = Class), size = 2) +
      geom_point(data = PLT_DATA %>% filter(Class == "ETF"), 
                 aes(x = Risk, y = Return)) +
      geom_line(data = PLT_DATA %>% filter(Class == "Portfolio"),
                aes(x = Risk, y = Return), size = 1, color = "#E69F00") +
      geom_label_repel(
        data = PLT_DATA %>% filter(Class == "ETF"),
        aes(x = Risk, y = Return, fill = factor(CLASS), label = Ticker),
        size  = 3,
        color = "white",
        box.padding   = unit(0.3, "lines"),
        point.padding = unit(0.5, "lines"),
        segment.color = 'grey50'
      ) +
      geom_label_repel(
        data = PLT_DATA %>% filter(Class == "OPT"),
        aes(x = Risk, y = Return, fill = factor(Class), label = Ticker),
        size  = 3,
        color = "white",
        box.padding   = unit(0.3, "lines"),
        point.padding = unit(0.5, "lines"),
        segment.color = 'grey50'
      ) +
      theme_hc() +
      labs(title    = "Portfolio Return and Risk, Efficient Frontier",
           subtitle = "Risk free rate: 2%",
           fill     = "") +
      theme(plot.title      = element_text(color = "#666666", face = "bold", size = 20, hjust = 0),
            axis.title      = element_text(color = "#666666", face = "bold", size = 12),
            plot.subtitle   = element_text(color = "#666666", size = 15, hjust = 0),
            legend.position = "bottom", 
            text = element_text()) +
      scale_y_continuous(labels = percent, 
                         breaks = scales::pretty_breaks(n = 15)) +
      scale_x_continuous(labels = percent, 
                         breaks = scales::pretty_breaks(n = 10)) +
      geom_hline(yintercept = 0, size = 0.5) +
      geom_vline(xintercept = 0, size = 0.5) +
      scale_fill_tableau(name = "") +
      scale_colour_tableau()
    
    return(opt.plt.res)
    
  })
  
  output$opt.plot.cum <- renderPlot({
    
    DAILY_RET_DATA <- data.set.input()
    
    ia <- create.historical.ia(DAILY_RET_DATA, 252)
    
    n <- ia$n        
    
    constraints <- new.constraints(n, lb = 0, ub = 1)
    constraints <- add.constraints(diag(n), type = '>=', b = 0, constraints)
    constraints <- add.constraints(diag(n), type = '<=', b = 1, constraints)
    
    constraints <- add.constraints(rep(1, n), 1, type = '=', constraints)        
    
    ef <- portopt(ia, constraints, 50, 'Efficient Frontier') 
    
    weight_minvar <- min.var.portfolio(ia,constraints)  
    weight_maxsharpe <- max.sharpe.portfolio()(ia,constraints) 
    weight_ew <- rep(1,n)/n
    weight_rp <- risk.parity.portfolio()(ia, constraints)
    
    
    ew_ret <- weight_ew %*% t(ia$hist.returns) %>% t()
    colnames(ew_ret) <- "Equal.Weighted"
    mv_ret <- weight_minvar %*% t(ia$hist.returns) %>% t()
    colnames(mv_ret) <- "Minimum.Variance"
    ms_ret <- weight_maxsharpe %*% t(ia$hist.returns) %>% t()
    colnames(ms_ret) <- "Maximum.Sharpe.Ratio"
    rp_ret <- weight_rp %*% t(ia$hist.returns) %>% t()
    colnames(rp_ret) <- "Risk.Parity"
    
    ret_data <- cbind(ew_ret, mv_ret, ms_ret, rp_ret)
    
    ret_data <- as.xts(ret_data)
    charts.PerformanceSummary(ret_data, 
                              wealth.index = T, 
                              main = "Performance Summary")
    
  })
  
  output$opt.port.weight <- renderPlot({
    
    DAILY_RET_DATA <- data.set.input()
    
    ia <- create.historical.ia(DAILY_RET_DATA, 252)
    
    n <- ia$n        
    
    constraints <- new.constraints(n, lb = 0, ub = 1)
    constraints <- add.constraints(diag(n), type = '>=', b = 0, constraints)
    constraints <- add.constraints(diag(n), type = '<=', b = 1, constraints)
    
    constraints <- add.constraints(rep(1, n), 1, type = '=', constraints)        
    
    ef <- portopt(ia, constraints, 50, 'Efficient Frontier') 
    
    weight_minvar <- min.var.portfolio(ia,constraints)  
    weight_maxsharpe <- max.sharpe.portfolio()(ia,constraints) 
    weight_ew <- rep(1,n)/n
    weight_rp <- risk.parity.portfolio()(ia,constraints)
    
    ew_ret <- weight_ew %*% t(ia$hist.returns) %>% t()
    colnames(ew_ret) <- "EW_RET"
    mv_ret <- weight_minvar %*% t(ia$hist.returns) %>% t()
    colnames(mv_ret) <- "MV_RET"
    ms_ret <- weight_maxsharpe %*% t(ia$hist.returns) %>% t()
    colnames(ms_ret) <- "MS_RET"
    rp_ret <- weight_rp %*% t(ia$hist.returns) %>% t()
    colnames(rp_ret) <- "RP_RET"
    
    ret_data <- cbind(ew_ret, mv_ret, ms_ret, rp_ret)
    
    PLT_DATA_WEIGHT <- tibble(Risk = ef$risk)
    PLT_DATA_WEIGHT <- bind_cols(PLT_DATA_WEIGHT, ef$weight %>% as_tibble())
    
    weight.res <- PLT_DATA_WEIGHT %>% 
      gather(Ticker, Weight, -Risk) %>% 
      ggplot() + 
      geom_area(aes(x = Risk, y = Weight, fill = Ticker)) +
      scale_y_continuous(labels = percent, 
                         breaks = scales::pretty_breaks(n = 10)) +
      scale_x_continuous(labels = percent, 
                         breaks = scales::pretty_breaks(n = 10)) +
      theme(legend.position = "top", 
            legend.title    = element_blank()) +
      theme_hc() +
      scale_fill_tableau("tableau20") +
      scale_colour_tableau() +
      labs(title = "Weights on the Efficient Frontier")
    
    return(weight.res)
    
  })
  
  output$plt.aa <- renderPlot({
    
    DAILY_RET_DATA <- data.set.input()
    
    ia <- create.historical.ia(DAILY_RET_DATA, 252)
    
    n <- ia$n        
    
    constraints <- new.constraints(n, lb = 0, ub = 1)
    constraints <- add.constraints(diag(n), type = '>=', b = 0, constraints)
    constraints <- add.constraints(diag(n), type = '<=', b = 1, constraints)
    
    constraints <- add.constraints(rep(1, n), 1, type = '=', constraints)        
    
    ef <- portopt(ia, constraints, 50, 'Efficient Frontier') 
    
    weight_minvar <- min.var.portfolio(ia,constraints)  
    weight_maxsharpe <- max.sharpe.portfolio()(ia,constraints) 
    weight_ew <- rep(1,n)/n
    weight_rp <- risk.parity.portfolio()(ia,constraints)
    
    ew_ret <- weight_ew %*% t(ia$hist.returns) %>% t()
    colnames(ew_ret) <- "EW_RET"
    mv_ret <- weight_minvar %*% t(ia$hist.returns) %>% t()
    colnames(mv_ret) <- "MV_RET"
    ms_ret <- weight_maxsharpe %*% t(ia$hist.returns) %>% t()
    colnames(ms_ret) <- "MS_RET"
    rp_ret <- weight_rp %*% t(ia$hist.returns) %>% t()
    colnames(rp_ret) <- "RP_RET"
    
    ticker.name <- t(ia$hist.returns) %>% rownames()
    
    raw.pie.plt.data <- tibble(ticker = factor(ticker.name, levels = ticker.name),
                               Equal.Weight = round(weight_ew,4) * 100, 
                               Max.Sharpe   = round(weight_maxsharpe,4) * 100, 
                               Min.Variance = round(weight_minvar,4) * 100,
                               Risk.Parity  = round(weight_rp, 4) * 100)
    
    pie.plt.data <- raw.pie.plt.data %>% 
      gather(class, weight, -ticker) %>% 
      group_by(class) %>% 
      mutate(rev.weight = weight %>% rev) %>%
      mutate(rev.pos    = cumsum(rev.weight) - rev.weight/2) %>% 
      mutate(pos        = rev.pos %>% rev) %>% 
      ungroup()
    
    plt.aa <- pie.plt.data %>% 
      ggplot(aes(x = 1, y = weight, fill = ticker)) +
      geom_bar(stat = "identity", colour = 'white') +
      facet_wrap(~ class) +
      geom_text(data = pie.plt.data %>% filter(weight != 0),
                aes(label = paste0(round(weight,1),"%"), y = pos), 
                color       = "white", 
                fontface    = "bold",
                inherit.aes = T) +
      geom_label_repel(data = pie.plt.data %>% filter(weight != 0), 
                       aes(y = pos, label = ticker), 
                       color   = "white",
                       point.padding = 1, 
                       na.rm   = T,
                       nudge_x = .5, 
                       nudge_y = .5) +
      coord_polar(theta = 'y') +
      xlim(c(-0.5, 2)) +
      scale_fill_tableau("tableau20") +
      scale_colour_tableau() +
      theme(axis.title.x = element_blank(),
            axis.text.x  = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y  = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "none",
            plot.title   = element_text(size = 18),
            strip.text   = element_text(size = 14),
            panel.border = element_blank(),
            panel.grid   = element_blank()) +
      labs(title = "Asset Allocation")
    
    return(plt.aa)
    
  })
  
}

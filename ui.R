
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

main.path <- getwd()
setwd(main.path)
options(shiny.maxRequestSize = 100*1024^2) 
source("global.R")

#-----------------------------------------------------------------
# UI
#-----------------------------------------------------------------

ui <- (
  navbarPage(
    
    theme = shinytheme("simplex"),
    "KB ETF Portfolio",
    
    # 1. Load raw data
    tabPanel("Load Raw Data",
             # Left panel
             sidebarPanel(
               fileInput(inputId = "file.fnguide", 
                         label = "FnGuide File input:"),
               fileInput(inputId = "file.wisefn",  
                         label = "WiseFn File input:")
             ),
             # Right panel
             mainPanel(
               tabsetPanel(
                 tabPanel(title = "Fn Guide Data",
                          DT::dataTableOutput("table.fnguide")
                 ),
                 tabPanel(title = "Wisefn Data",
                          DT::dataTableOutput("table.wisefn")
                 )
               )
             )
    ),
    
    # 2. ETF information
    tabPanel("ETF Information",
             # Left panel
             sidebarPanel(
               fileInput(inputId = "file.etf.idx", 
                         label = "KB ETF line-up list input:"),
               # 1.Test date
               dateRangeInput(inputId ='etf.date.range',
                              label = 'Date range input:',
                              start = "2013-07-14", end = "2018-07-14")
             ),
             # Right panel
             mainPanel(
               tabsetPanel(
                 tabPanel(title = "KB ETF line-up Information",
                          DT::dataTableOutput("table.etf.idx")),
                 tabPanel(title = "KB ETF line-up Risk Return",
                          
                          h3("ETF Performance"),
                          DT::dataTableOutput("table.etf.tot.ret"),
                          hr(),
                          
                          h3("Plot: ETF Risk Return"),
                          plotOutput("plt.kb.etf.tot.ret", height = "600px"),
                          hr(),
                          
                          h3("Plot: ETF Correlation"),
                          plotOutput("plt.kb.etf.corr", height = "900px"),
                          hr())
               )
             )
    ),
    
    # 3. Portfolio optimization
    tabPanel("Portfolio Optimization",
             # Left panel
             sidebarPanel(
               # 1.Back test date
               dateRangeInput(inputId ='date.range',
                              label = 'Date range input:',
                              start = "2013-07-14", end = "2018-07-14"),
               # 2. ETF selection
               selectizeInput(inputId = 'etf.ticker.list',
                              'ETF Selection', 
                              choices = etf.ticker.name, multiple = T
               ),
               # 3. Run Optimization
               actionButton(inputId = "opt.run", 
                            label = "Run Optimization")
             ),
             # Right panel
             mainPanel(
               tabsetPanel(
                 tabPanel(title = "ETF Risk Return Profile",
                          
                          h3("ETF Return"),
                          DT::dataTableOutput("table.test"),
                          hr(),
                          
                          h3("Table: Annualized Risk Return"),
                          DT::dataTableOutput("table.ret"),
                          hr(),
                          
                          h3("Plot: Annualized Risk Return"),
                          plotOutput("etf.ret.risk.plt", height = "600px"),
                          hr(),
                          
                          h3("Performance Summary"),
                          plotOutput(outputId = 'etf.ret.plt', height = "600px"),
                          hr(),
                          
                          h3("Correlation Plot"),
                          plotOutput(outputId = "etf.corr.plt", height = "600px"),
                          hr()
                 ),
                 tabPanel(title = "Portfolio Optimization Results",
                          
                          h3("Optimization Results"),
                          DT::dataTableOutput("opt.table"),
                          hr(),
                          
                          h3("Plot: Optimization Results"),
                          plotOutput("opt.plot", height = "600px"),
                          hr(),
                          
                          h3("Plot: Asset Allocation"),
                          plotOutput("plt.aa", height = "900px"),
                          hr(),
                          
                          h3("Performance Summary"),
                          plotOutput("opt.plot.cum", height = "600px"),
                          hr(),
                          
                          h3("Plot: Weights on the Efficient Frontier"),
                          plotOutput("opt.port.weight", height = "600px"),
                          hr()
                 )
               )
             )
    )
  )
)
library(shiny)
library(tablerDash)
#library(feedeR)
#library(shinydashboard)
#library(shinydashboardPlus)
library(shinyWidgets)
library(quantmod)
#library(dygraphs)
#library(dashboardthemes)
library(DT)
library(shinyjs)
library(tidyverse)
library(tidyquant)
library(billboarder)
library(highcharter)
#library(tuichartr)
library(stringr)
library(glue)
library(edgarWebR)
library(finreportr)
library(xml2)
library(rvest)
library(tidyRSS)

options(stringsAsFactors = FALSE)

url1 <- function(x) {
  paste0('https://www.sec.gov', ((xml2::read_html(x) %>%
                                    rvest::html_nodes('table') %>% .[1] %>%
                                    rvest::html_nodes('a') %>% 
                                    rvest::html_attr('href')))[1])
}

compScrape <- function(comp.ticker) tryCatch ({
  url1 <- function(x) {
    paste0('https://www.sec.gov', ((xml2::read_html(x) %>%
                                      rvest::html_nodes('table') %>% .[1] %>%
                                      rvest::html_nodes('a') %>% 
                                      rvest::html_attr('href')))[1])
  }
  
  filingList <- data.frame(edgarWebR::company_details(comp.ticker, type = 'DEF 14A', count = 1))# %>% filter(!grepl('A', filings.type))
  if(comp.ticker == 'CHK'){
    filingList <- filingList[2:nrow(filingList),]
  }
  #accssionNo <- filingList$filings.accession_number[1]
  #checkNo <- filingList$information.cik[1]
  #filingList <- rbind(filingList, filingList1) %>% arrange(desc(filings.filing_date))
  #compInfo <- finreportr::CompanyInfo(comp.ticker)
  #filingList$Company <- compInfo$company
  #rm(filingList1)
  
  filingList$url1 <- lapply(filingList$filings.href,  url1)
  filingList$url1 <- gsub('/ix?doc=', '', filingList$url1, fixed=TRUE)
  #values$check <- filingList
  filingList <- filingList[,c('filings.filing_date', 'filings.type', 'url1')] %>% arrange(desc(filings.filing_date))
  #filingList$quarter <- lubridate::quarter(filingList$filings.filing_date) - 1
  #filingList$year <- lubridate::year(filingList$filings.filing_date)
  #filingList$quarter[filingList$quarter == 0] <- 4
  #filingList$year[filingList$quarter == 4] <- filingList$year[filingList$quarter == 4]-1
  #filingList$period <- paste0('Q', filingList$quarter, filingList$year)
  #updateSelectizeInput(session, 'Filing', choices = filingList$period)
  names(filingList)[1:3] <- c('filingDate', 'type', 'url1')
  #filingList <- filingList[,c('Company', 'period', 'filingDate', 'type', 'url1')]
  filingList <- data.frame(lapply(filingList, function(x){
    gsub("iXBRL", "", x)
  }))
  filingList <- data.frame(lapply(filingList, function(x){
    gsub("\\s+", "", x)
  }))
  
  filingList$type <- paste0('<a href="', filingList$url1, '" target="_blank">', filingList$type,'</a>')
  #values$filingList <- filingList
  #filingList <- filingList[,c('Company', 'period', 'filingDate', 'type', 'url1')]
  names(filingList) <- c('Filing Date', 'Report', 'URL')
  filingList <- as.data.frame(filingList)
  
  
  nodes <- xml2::read_html(filingList$URL[1]) %>% html_nodes('table')
  
  
  
  string1 <-  'Principal'
  strMatch <- string1
  
  signal <- FALSE
  my_tables <- list()
  #my_length <- list()
  j <- 0
  for (i in 1:length(nodes)) {
    signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    # if title signal previously set and this is a table tag
    if (signal & html_name(nodes[i]) == "table") {
      cat("Match..\n")
      print(i)
      # get the table (data frame)
      list1 <- nodes[i]# %>% html_nodes('tr')
      this_table <- list1 %>% paste(collapse='\n')
      this_table <- data.frame(list1 = this_table)
      list1 <- nodes[i] %>% html_nodes('tr')
      this_table$rows <- length(list1)
      
      j = j + 1
      my_tables[[j]] <- this_table
      
      
      
      # and reset the signal so we search for the next one
      signal <- FALSE
    }
    
    # if the signal is clear look for matching title
    if (!signal) {
      signal <- nodes[i] %>% html_text() %>% str_detect(strMatch)
    }
  }
  
  my_tables <- my_tables[[length(my_tables)]]
  return(my_tables)
},
error = function(e) {
  e
  NULL
})


stockListX <- rbind(tidyquant::tq_exchange('AMEX'),tidyquant::tq_exchange('NASDAQ'),tidyquant::tq_exchange('NYSE')) %>%
  filter(!duplicated(symbol)) %>% filter(!is.na(sector)) %>% filter(!is.na(industry)) %>% filter(!is.na(market.cap)) %>%
  filter(!duplicated(company))

labelTicker <- data.frame(symbol = list.files(path = './data/'))
labelTicker$symbol <- gsub('.rds', '', labelTicker$symbol, fixed = TRUE)
#print(head(stockListX))
stockList <- stockListX %>% filter(symbol %in% labelTicker$symbol) %>% filter(!duplicated(company))
#print(head(stockList))
cols <- c('#00a4e3', '#a31c37', '#adafb2', '#d26400', '#eaa814', '#5c1848', '#786592', '#ff4e50', '#027971', '#008542', '#5c6d00','#0D1540', '#06357a' )

  ui = tablerDashPage(
    enable_preloader = TRUE, loading_duration = 1,
    
    navbar = tablerDashNav(
      id = 'xbrl',
      src = "logo.png",
      pickerInput(
        inputId = "ticker1",
        label = "Ticker", 
        choices = sort(unique(stockList$symbol)),
        options = list(
          `live-search` = TRUE,
          `actions-box` = TRUE),
        multiple = TRUE,
        selected = sort(unique(stockList$symbol))
      ),
      pickerInput(
        inputId = "ticker",
        label = "Company", 
        choices = sort(unique(stockList$company)),
        options = list(
          `live-search` = TRUE)
      ),
     # title = 'XBRL-Data',
      navMenu = tablerNavMenu(
        tablerNavMenuItem(
          tabName = "home",
          icon = "home",
          "Home"
        ),
        tablerNavMenuItem(
          tabName = "dashboard",
          icon = "aperture",
          "Company Info"
        ),
        tablerNavMenuItem(
          tabName = "comps",
          icon = "award",
          "Market Comps"
        ),
        tablerNavMenuItem(
          tabName = "statements",
          icon = "file-text",
          "Financial Statements"
        ),
        tablerNavMenuItem(
          tabName = "graphs",
          icon = "trending-up",
          "Graphs"
        ),
        tablerNavMenuItem(
          tabName = "compensation",
          icon = "dollar-sign",
          "Executive Compensation"
        ),
        tablerNavMenuItem(
          tabName = "packages",
          icon = "info",
          "About"
        )
      )
      
    ),
    
    footer = tablerDashFooter(
      copyrights = "@xbrlData LLC, 2020"
    ),
    title = "xbrlData LLC",

    body = tablerDash::tablerDashBody(
      #theme_blue_gradient,
      shinyjs::useShinyjs(),
      # infoBoxes
      tablerTabItems(
        tablerTabItem(
          tabName = "home",
          fluidRow(
            column(width = 6,
          tablerBlogCard(
            title = img(src = 'logo.png'),
            author = "",
            date = today(),
            href = "https://www.xbrl-data.com",
            #src = "github.png",
            avatarUrl =  "dollar.PNG",
            width = 12,
            p("xbrlData is a project designed to provide
            as-reported financial data from 10-Q/10-K filings
            on the Edgar website for the SEC.  Parsing the data
            is complicated, time-intensive, and nerve-wracking,
            but can provide some great insights and analysis.  We
            provide the backup data for a minimal fee so as to support
            our continued efforts and to provide research tools to
            interested parties.  For bulk download of all companies,
              please contact us via email"),
            
            h6('Twitter:'),
            tags$a(
              href="https://twitter.com/DataXBRL", 
              tags$img(src="twitter.png", 
                       title="Twitter",
                       width="10%",
                       height="10%"), 
              target="_blank"
            ),
            h6('Source Code on Github:'),
            tags$a(
              href="https://github.com/xbrl-data/xbrl-data", 
              tags$img(src="github.png", 
                       title="Github"), 
              target="_blank"
            ),
            h6('Contact Us:'),
            a(actionButton(inputId = "email1", label = "Contact Admin", 
                           icon = icon("envelope", lib = "font-awesome")),
              href="mailto:xbrl-data@xbrl-data.com")
          )
          ),
          column(width = 6,
          p('Note:  xbrl-Data is in Beta so the Paypal link is still in Sandbox and will charge $0.00 for data access.
            Please let me know during this period how we can improve the experience.')
          )
          )
        ),
        tablerTabItem(
          tabName="dashboard",
         
            fluidRow(
              
              
              column(width = 4,
                     #tablerCard(
                      # title = "Company Profile",
                       #closable = FALSE,
                       #width = 12,
                       #status = "info",
                       #collapsible = TRUE,
                     h3('Company Profile'),
                       htmlOutput('ticker1'),
                       htmlOutput('industry'),
                       htmlOutput('address'),
                       htmlOutput('cityState'),
                       htmlOutput('mktCap1'),
                       br(),
                       h6('1. State Street Global Advisors')
                     #)
            ),
            column(width = 8,
                   
                   highchartOutput("bsData")
            ),
            column(width = 12,
                   #tablerCard(
                   #  title = "Candlestick Chart", 
                   #  closable = FALSE, 
                   #  width = 12,
                   #  status = "info", 
                   #solidHeader = FALSE, 
                   #  collapsible = TRUE,
                   #enable_dropdown = FALSE,
                   h4('Candlestick Chart'),
                   highcharter::highchartOutput('stonks'),
                   h6('Source: Quantmod/Yahoo Finance')
                   #)
            )#,
            # column(width = 7,
            #        tablerCard(
            #          title = 'Recent News',
            #          closable = FALSE,
            #          width = 12,
            #          status = 'info',
            #        #solidHeader = TRUE,
            #          collapsible = TRUE,
            #       #h4('Values in US$Millions'),
            #       DT::dataTableOutput('recent')
            #        )
            # )
            )
          ),
        tablerTabItem(
          tabName = 'comps',
          fluidRow(
            column(width = 6,
                   #tablerCard(
                  #   title = "Industry Market Cap", 
                  #   closable = FALSE, 
                  #   width = 12,
                  #   status = "info", 
                     #solidHeader = FALSE, 
                  #   collapsible = TRUE,
                     #enable_dropdown = FALSE,
                  h4('Industry Market Cap'),
                     h6('Exchanges: AMEX, NYSE, NYMEX'),
                     highchartOutput('mktCap'),
                     br(),
                     h6('Data source: TidyQuant/State Street Global Advisors')
                   #)
            ),
            column(width =6,
                   #tablerCard(
                  #   title = "Market Cap Comparables", 
                  #   closable = FALSE, 
                  #   width = 12,
                  #   status = "info", 
                     #solidHeader = FALSE, 
                  #   collapsible = TRUE,
                     #enable_dropdown = FALSE,
                  h4('Market Cap Comparables'),
                     highchartOutput('closest'),
                     br(),
                     h6('Data source: TidyQuant/State Street Global Advisors')
                   #)
            )
          )),
        tablerTabItem(
          tabName = 'statements', 
          
     
            h3('Bulk Financial Download - All Filings (Selected Company)'),
          p('XBRL Scraping in this site began in 2014, meaning the earliest data 
          will likely be from 2012 onwards. The below table shows all the available
            information in the download. Categories in download are:'),
          h6('Table: Main Table from Filing Period'),
          h6('Category:  Parent ID Label'),
          h6('MainElement: Parent ID'),
          h6('Element: Child ID'),
          h6('Label: Child ID Label'),
          h6('Order:  Order of Element within that Period/Table'),
          h6('Units: Reported Units in Filing'),
          h6('Period:  Time Period Specified for Data Point, in Months'),
          h6('endDate:  As of Date for Data Point'),
          h6('fact:  Value'),
          h6('arcrole: Value Here represents a summary line or not'),
          h6('Type:  Is table a Disclosure, a Financial Satement, or a Document?'),
          h6('PERIOD: Filing Period'),
          h6('ticker:  Stock Ticker'),
          h6('company:  Company Name'),
          h6('sector:  Industry Sector'),
          h6('industry: Industry'),
          br(),
              h5('US$2.00 Charge Per Download'),
              h6('Only one download allowed per session.  Simply refresh the app if another is desired.'),
              fluidRow(
                column(width = 6,
                       div(class="span6", align="center", #`data-display` = TRUE,
                           div(tags$script(src = "https://www.paypalobjects.com/api/checkout.js "),
                               #tags$script('<a id="Calculate">'),
                               tags$a(id = 'Calculate',
                                      tags$script("paypal.Button.render({
                                        // Configure environment
                                        env: 'sandbox',
                                        client: {
                                        sandbox: 'AUzCJ8LZWpfr-5q8RbpAbzgbnviGF01LygUugclYSUENDcN2SeN8n_hRjq8B7nFaEor1z4yWOlcgtbd_',
                                        production: 'demo_production_client_id'
                                        },
                                        // Customize button (optional)
                                        locale: 'en_US',
                                        style: {
                                        size: 'small',
                                        color: 'gold',
                                        shape: 'pill',
                                        },
                                        // Set up a payment
                                        payment: function (data, actions) {
                                        return actions.payment.create({
                                        transactions: [{
                                        amount: {
                                        total: '0.01',
                                        currency: 'USD'
                                        }
                                        }]
                                        });
                                        },
                                        // Execute the payment
                                        onAuthorize: function (data, actions) {
                                        return actions.payment.execute()
                                        .then(function () {
                                        // Show a confirmation message to the buyer
                                        //window.alert('Thank you for your purchase!');
                                        var btn = document.createElement('BUTTON');   // Create a <button> element
                                        btn.innerHTML = 'Collect Data';                   // Insert text
                                        btn.className = 'btn btn-default shiny-download-link shiny-bound-output';
                                        btn.id = 'downloadData';
                                        btn.target = '_blank';
                                        document.getElementById('calculate').appendChild(btn);               // Append <button> to <body>
                                        });
                                        }
                                        }, '#calculate')")),
                               
                               tags$div(id = "calculate")))
                ),
                column(
                  width =6,
                  hidden(div(
                    id='actions',
                    downloadButton("downloadData1", "Download")
                  )
                
              )
              )
            
          ),
          br(),
          br(),
            fluidRow(
              column(width = 4,
              tablerCard(
                title = 'Recent Filings',
                closable = FALSE,
                width = 12,
                status = 'info',
                #h4('Recent Filings'),
                collapsible = TRUE,
                DT::dataTableOutput("filingList"))
              ),
              column(width = 8,
                     tablerCard(
                       title = "Financial Statement Data", 
                       closable = FALSE, 
                       width = 12,
                       status = "info", 
                      # solidHeader = FALSE, 
                       collapsible = TRUE,
                      # enable_dropdown = FALSE,
                       fluidRow(
                         column(width = 2,
                          selectizeInput('period', 'Filing Period', choices = '')
                          ),
                         column(width = 2,
                                selectizeInput('type', 'Statement Type', choices = '')
                         ),
                       column(width = 4,
                            selectizeInput('table', 'Table', choices = '')  
                       
                        ),
                       column(width = 2,
                              selectizeInput('endDate', 'Date', choices = '')  
                              ),
                       column(width = 2,
                              selectizeInput('duration', 'Months', choices = '')
                              )
                     ),
                     fluidRow(
                       column(width =12,
                              DT::dataTableOutput('financials')
                              )
                     )
                     )
                     )
              )
          ),
          tablerTabItem(
            tabName = 'graphs',
            
            
            fluidRow(
              column(width = 6,
                     #tablerCard(
                    #   title = "Debt Maturity Schedule", 
                    #   closable = FALSE, 
                    #   width = 12,
                    #   status = "info", 
                       #solidHeader = FALSE, 
                    #   collapsible = TRUE,
                       #enable_dropdown = FALSE,
                      h4('Debt Maturity Schedule'),
                       highchartOutput('maturity'),
                       h6("Data source: SEC XBRL")
                    # )
              ),
              
              column(width = 6,
                     #tablerCard(
                    #   title = "Total Debt", 
                    #   closable = FALSE, 
                    #   width = 12,
                    #   status = "info", 
                       #solidHeader = FALSE, 
                    #   collapsible = TRUE,
                       #enable_dropdown = FALSE,
                    h4('Total Debt'),
                       highchartOutput('debt')
                     #)
              )
            ),
            fluidRow(
              column(width = 12,
                     #tablerCard(
                     # title = "Current Assets/Liabilities", 
                     #closable = FALSE, 
                     #width = 12,
                     #status = "info", 
                     #solidHeader = FALSE, 
                     #collapsible = TRUE,
                     #enable_dropdown = FALSE,
                     h4('Net Income'),
                     highchartOutput('netIncome')
                     #)
              )
              
            ),
            fluidRow(
              column(width = 12,
                     #tablerCard(
                     # title = "Current Assets/Liabilities", 
                     #closable = FALSE, 
                     #width = 12,
                     #status = "info", 
                     #solidHeader = FALSE, 
                     #collapsible = TRUE,
                     #enable_dropdown = FALSE,
                     h4('Operating Income'),
                     highchartOutput('operIncome')
                     #)
              )
              
            ),
            
            fluidRow(
              column(width = 12,
                     #tablerCard(
                      # title = "Current Assets/Liabilities", 
                       #closable = FALSE, 
                       #width = 12,
                       #status = "info", 
                       #solidHeader = FALSE, 
                       #collapsible = TRUE,
                       #enable_dropdown = FALSE,
                     h4('Current Assets/Liabilities'),
                       highchartOutput('currents')
                     #)
              )
  
            ),
           
            fluidRow(
              column(width = 12,
                     #tablerCard(
                    #   title = "Annual Cash Flow Summary", 
                    #   closable = FALSE, 
                    #   width = 12,
                    #   status = "info", 
                       #solidHeader = FALSE, 
                    #   collapsible = TRUE,
                       #enable_dropdown = FALSE,
                    h4('Quarterly Cash Flow Summary'),
                       highchartOutput('cf',  height = 800)
                     #)
              )
              
            )
          ),
        tablerTabItem(
          tabName = 'compensation',
          fluidRow(
            h4('Executive Compensation Tables'),
              htmlOutput('plot')
            
          )
        ),
          tablerTabItem(
            tabName = "packages",
            h3('Packages Used:'),
            fluidRow(
              column(width = 4,
              tablerProfileCard(
                width = 12,
                title = "Shiny",
                subtitle = "RStudio",
                background = "",
                src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png"
            ),
            tablerProfileCard(
              width = 12,
              title = "shinyjs",
              subtitle = "Dean Attali",
              background = "",
              src = "shinyjs.png"
            ),
            tablerProfileCard(
              width = 12,
              title = "edgarWebR",
              subtitle = "Micah Waldstein",
              background = "",
              src = "edgarWebR.PNG"
            ),
            tablerProfileCard(
              width = 12,
              title = "Highcharter",
              subtitle = "Highsoft",
              background = "",
              src = "highcharter.PNG"
            )
            
            ),
            column(width = 4,
                   
              tablerProfileCard(
                width = 12,
                title = "tablerDash",
                subtitle = "RinteRface",
                background = "",
                src = "https://preview.tabler.io/demo/brand/tabler.svg"
            ),
            tablerProfileCard(
              width = 12,
              title = "quantmod",
              subtitle = "Jeffrey A Ryan",
              background = "",
              src = "quantmod.PNG"
            ),
            tablerProfileCard(
              width = 12,
              title = "DT",
              subtitle = "",
              background = "",
              src = "dt.PNG"
            ),
            tablerProfileCard(
              width = 12,
              title = "shinyWidgets",
              subtitle = "Fanny Meyer/David Granjon",
              background = "",
              src = "shinyWidgets.PNG"
            )
            
            
            ),
            column(width = 4,
                   tablerProfileCard(
                     width = 12,
                     title = "Tidyverse",
                     subtitle = "Hadley Wickham",
                     background = "",
                     src = "https://user-images.githubusercontent.com/5993637/39728796-935fde9e-520d-11e8-868f-85d7a132249c.png"
                   ),
                   tablerProfileCard(
                     width = 12,
                     title = "TidyQuant",
                     subtitle = "",
                     background = "",
                     src = "tq.png"
                   ),
                   tablerProfileCard(
                     width = 12,
                     title = "finreportr",
                     subtitle = "Seward Lee",
                     background = "",
                     src = "finreportr.PNG"
                   )
                   
                   )
            )
          )
        )
    )
  )
  
  server = function(input, output, session) { 
    values <- reactiveValues()
    
    query_modal <- modalDialog(
      title = "Disclaimer",
      p('This website does not provide financial advice.
        The information contained on this Website and
        the resources available for download through
        this website is not intended as, and
        shall not be understood or construed as,
        financial advice.  I am not an attorney,
        accountant, or financial advisor, nor am
        I holding myself out to be, and the information
        contained on this Website is not a substitute
        for financial advice from a professional who
        is aware of the facts and circumstances of
        your individual station.'),
      br(),
      p('We have done our best to ensure that the
        information provided on this Website and the
        resources available for download are
        accurate and provide valuable information.
        XBRL data is painstakingly difficult to collect,
        parse, and display.  While we attempt to clean the
        data as accurately as possible, there may be errors
        due to filing mistakes or interpretation errors.  We
        charge a small fee for the backup data on this website
        due to the intense effort that
        goes into the collecting and cleaning of the data.  If
        you choose to download this data, you do so at your own 
        risk and acknowledge that it may not be 100% accurate.
        Regardless of anything to the contrary,
        nothing available on or through this
        Website should be understood as a recommendation
        that you should not consult with a financial
        professional to address your particular
        information.  We expressly recommend that
        you seek advice from a professional.'),
      br(),
      p('By pressing Accept, you acknowledge this
        Disclaimer.'),
      easyClose = F,
      footer = tagList(
        actionButton("close", "Accept")
      )
    )
    
    # Show the model on start up ...
    showModal(query_modal)
    
    observeEvent(input$close, {
      values$x <- 1
      removeModal()
      
    })
    
    observe({
      df1 <- stockList %>% filter(symbol %in% input$ticker1)
      updatePickerInput(session, 'ticker', choices = sort(unique(df1$company)))
    })
    #shinyjs::show('actions')
    stockInfo <- reactive({
      if(is.null(input$ticker1)||input$ticker1 == ''||is.null(input$ticker)||input$ticker == ''){
        (stockList %>% filter(company%in% 'Apple Inc.'))
      } else {
        (stockList %>% filter(company%in% input$ticker))
      }
    }
      
    
    )
    
    comps <- reactive(stockListX %>% 
                        filter(sector %in% stockInfo()$sector) %>%
                        filter(industry %in% stockInfo()$industry))
    

    
     observeEvent(input$ticker, {
       
       shinyjs::hide('downloadData')
       shinyjs::hide('actions')
       
    #   shinyjs::show('Calculate')
     })
     
     observeEvent(input$downloadData1, {
       shinyjs::hide('downloadData')
       
     })
    
    observe({
      if(is.null(input$ticker)||input$ticker == ''||is.null(values$x)||is.null(input$ticker1)||input$ticker1==''){
        values$stock <- NULL
      } else {
        stock <- NULL
        rm(stock)
        #print(head(comps()))
        #ticker <- input$operator
        ticker <- stockInfo()$symbol
        #print(ticker)
        stock <- getSymbols(ticker, src='yahoo', auto.assign = FALSE, setSymbolLookup('stock'))
        names(stock) <- c('Open', 'High', 'Low', 'Close', 'Volume', 'Adjusted')
        values$stock <- stock
        
      
        
        
        
      }
    })
    
    observe({
      if(is.null(input$ticker)||input$ticker == ''||is.null(values$x)||is.null(input$ticker1)||input$ticker1==''){
        NULL
      } else {
      output$ticker1 <- renderText(paste0('<b>Ticker: </b>', stockInfo()$symbol))
      output$industry <- renderText(paste0('<b>Industry: </b><br>', stockInfo()$industry))
      df1 <- CompanyInfo(stockInfo()$symbol)
      output$address <- renderText(paste0('<b>Address: </b><br>', df1$street.address))
      output$cityState <- renderText(df1$city.state)
      output$mktCap1 <- renderText(paste0('<b>Market Cap: </b>',stockInfo()$market.cap, '<sup>1</sup>'))
      }
    })

    
    output$stonks <- renderHighchart({
      #print(input$dateRange)
      if(is.null(values$stock)||
         is.null(input$ticker)||input$ticker == ''||
         is.null(values$x)||is.null(input$ticker1)||
         input$ticker1==''){
        NULL
      } else {
      #print(head(tables()))
        # dygraph(values$stock[,1:4]) %>%
        #   dyCandlestick()%>%
        #   dyAxis("y", label = "Share Price, US$") %>%
        #   dyRangeSelector(height = 20)
        hchart(values$stock[,1:4]) %>%
          hc_caption(text = "Powered by Highcharts")
        
        
        
      }
      
    })
    
    tables <- reactive({
      if(is.null(input$ticker)||input$ticker == ''||is.null(input$ticker1)||input$ticker1==''){
        readRDS('./data/AAPL.rds') %>%
          mutate(Period = replace(Period, is.na(Period), 0))
      } else {
      txt1 <- glue::glue('./data/{stockInfo()$symbol}.rds')
      readRDS(txt1) %>% 
        #filter(ticker %in% stockInfo()$symbol) %>%
        mutate(Period = replace(Period, is.na(Period), 0))
      }
      })
    
    observe(updateSelectizeInput(session, 'period', choices = unique(tables()$PERIOD)))
    observe(updateSelectizeInput(session, 'type', choices = unique((tables() %>% filter(PERIOD %in% input$period))$Type)))
    observe(updateSelectizeInput(session, 'table', choices = unique((tables() %>% filter(PERIOD %in% input$period) %>%
                                                                    filter(Type %in% input$type) )$Table)))
    observe(updateSelectizeInput(session, 'endDate', choices = unique((tables() %>% filter(PERIOD %in% input$period)%>%
                                                                         filter(Type %in% input$type) %>%
                                                                       filter(Table %in% input$table))$endDate)))
    observe(updateSelectizeInput(session, 'duration', choices = unique((tables() %>% filter(PERIOD %in% input$period)%>%
                                                                          filter(Type %in% input$type) %>%
                                                                       filter(Table %in% input$table) %>%
                                                                         filter(endDate %in% input$endDate))$Period)))
    
    tables1 <- reactive(tables() %>% filter(PERIOD %in% input$period) %>%
                          filter(Type %in% input$type) %>%
                          filter(Table %in% input$table) %>% filter(endDate %in% input$endDate) %>%
                          filter(Period %in% input$duration) %>%
                          mutate(Label = replace(Label, !is.na(arcrole), paste0('<b>',Label[!is.na(arcrole)],'</b>'))))
    

  
  output$financials <- DT::renderDataTable({
    if(is.null(input$duration)||input$duration == ''||
       is.null(input$period)||input$period == ''||
       is.null(input$table)||input$table == ''||
       is.null(input$type)||input$type == ''||
       is.null(input$endDate)||input$endDate == ''||
       is.null(tables1())||nrow(tables1()) == 0||
       is.null(input$ticker)||input$ticker == ''||
       is.null(input$ticker1)||input$ticker1==''){
      NULL
    } else {
      DT::datatable(tables1() %>% arrange(Order) %>% select(Order, Label, Value = fact, Units) %>%
                      mutate(Order = seq(1, n(), 1)), rownames = FALSE, escape=FALSE,
                    extensions = c('Scroller'), 
                    options = list(
                      dom = 'Bfrtip',
                      scrollX = TRUE,
                      scrollY = TRUE,
                      deferRender = TRUE,
                      paging = FALSE,
                      searching = FALSE
                    ))
    }
    
  })
  
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste(stockInfo()$symbol, "financials.csv", sep = "")
    },
    content = function(file) {
      write.csv(tables(), file, row.names = FALSE)
    }
  )
  
  onclick('downloadData', shinyjs::show('actions'))
  onclick('calculate', shinyjs::hide('downloadData'))
  onclick('calculate', reset('downloadData'))
  onclick('downloadData1', shinyjs::hide('calculate'))
  
  output$mktCap <- renderHighchart({
    if(is.null(input$ticker)||input$ticker == ''||is.null(values$x)||is.null(input$ticker1)||input$ticker1==''){
      NULL
    } else {
    df <- comps() %>% select(company, market.cap) %>% 
      mutate(end = str_sub(market.cap, -1, -1),
             market.cap = as.numeric(str_sub(market.cap, 2, -2))) %>%
      mutate(market.cap = replace(market.cap, end == 'B', market.cap[end == 'B']*1000000000),
             market.cap = replace(market.cap, end == 'M', market.cap[end == 'M']*1000000)) %>%
      subset(select = -c(end)) %>% arrange(desc(market.cap)) %>% mutate(market.cap = market.cap/1000000)
    
    highchart() %>% 
      hc_add_series(data = df, "pie", hcaes(x = company,y = market.cap), name = 'Market Cap') %>%
      hc_subtitle(text = paste0(comps()$industry[1], ' Market Cap: $', round(sum(df$market.cap, na.rm=TRUE)/1000,2), ' Bn'),
                  align = 'left') %>%
      hc_caption(text = "Powered by Highcharts") %>%
      hc_plotOptions(pie = list(
          dataLabels = list(enabled = FALSE)
        )) %>%
      hc_legend(show = FALSE) %>%
      hc_colors(cols)
    }

  }
    
  )
  
  output$closest <- renderHighchart({
    if(is.null(input$ticker)||input$ticker == ''||is.null(values$x)||is.null(input$ticker1)||input$ticker1==''){
      NULL
    } else {
    df <- comps() %>% select(symbol, market.cap) %>%
      mutate(end = str_sub(market.cap, -1, -1),
             market.cap = as.numeric(str_sub(market.cap, 2, -2))) %>%
      mutate(market.cap = replace(market.cap, end == 'B', market.cap[end == 'B']*1000000000),
             market.cap = replace(market.cap, end == 'M', market.cap[end == 'M']*1000000)) %>%
      subset(select = -c(end)) %>% arrange(desc(market.cap))
    #print(head(df))
    count1 <- which(df$symbol == stockInfo()$symbol)
    if(count1 <= 3){
      df <- head(df, 7)
    } else if((nrow(df)-count1) <= 2){
      df <- tail(df, 7)
    } else {
      df <- df[(count1-3):(count1+3),]
    }
    df <- df %>% mutate(market.cap = round(market.cap/1000000, 2))
    txt1 <- stockInfo()$symbol

    
    
    highchart() %>% 
      hc_add_series(data = df, "column", hcaes(name = symbol, y = market.cap), name = 'Market Cap') %>%
      hc_subtitle(text = 'Market Cap: US$Millions',
                  align = 'left') %>%
      hc_caption(text = "Powered by Highcharts") %>%
      hc_colors(cols) %>%
      hc_plotOptions(
        series = list(
          showInLegend = FALSE,
          pointFormat = "{point.y}"
        ),
        column = list(
          colorByPoint = TRUE
        )) %>%
      hc_xAxis(title = list(text = ''), categories = df$symbol, 
               labels = list(style = list(fontSize = '12px', fontWeight = 'bold'))) %>%
      hc_yAxis(title = list(text = ''), labels = list(style = list(fontSize = '12px', fontWeight = 'bold')))
    }
  }

  )
  
  output$currents <- renderHighchart({
    if(is.null(input$ticker)||input$ticker == ''||is.null(input$ticker1)||input$ticker1==''){
      NULL
    } else {
    df1 <- tables() %>% filter(grepl('us-gaap_AssetsCurrent', Element)|grepl('us-gaap_LiabilitiesCurrent', Element)) %>%
      filter(!duplicated(paste0(fact, endDate, Element))) %>% 
      mutate(quarter = as.numeric(substr(PERIOD, 2, 2))*3, year = as.numeric(substr(PERIOD, 3, 7)) ) %>%
      mutate(date = as.POSIXct(paste0(quarter, '/01/', year), format = '%m/%d/%Y')) %>%
      group_by(Element, endDate) %>% filter(date == min(date)) %>%ungroup() %>%
      group_by(Element, endDate) %>% summarise(fact = mean(fact)) %>% arrange(Element, endDate) %>%
      mutate(fact = round(fact/1000000, 0)) %>% mutate(endDate = as.POSIXct(endDate, format = '%Y-%m-%d')) %>%
      mutate(endDate = as.character(as.Date(endDate))) %>% group_by(endDate) %>%
      spread(Element, fact)
    df1[is.na(df1)] <- 0
    df1 <- df1 %>% gather(Element, fact, -endDate) %>%
      mutate(cR = fact[grepl('us-gaap_AssetsCurrent', Element)]/fact[grepl('us-gaap_LiabilitiesCurrent', Element)]) %>%
      mutate(Element = replace(Element, grepl('us-gaap_AssetsCurrent', Element), 'Current Assets'),
             Element = replace(Element, grepl('us-gaap_LiabilitiesCurrent', Element), 'Current Liabilities'),
             Element = replace(Element, grepl('cR', Element), 'Current Ratio'))
    df1$cR[is.na(df1$cR)] <- 0
    df1$cR[is.infinite(df1$cR)] <- 0
    
    if(nrow(df1) == 0){
      NULL
    } else {
    #print(head(df1))
    
    highchart() %>%
      hc_xAxis(title = list(text = ''), categories = unique(sort(df1$endDate)), 
               labels = list(style = list(fontSize = '12px', fontWeight = 'bold')))%>%
      hc_yAxis_multiples(list(title = list(text = "<b>US$Millions</b>", style = list(fontSize = '16px', fontWeight = 'bold')),opposite=FALSE,
                              labels = list(style = list(fontSize = '12px', fontWeight = 'bold'))),
                         list(title = list(text = "<b>Current Ratio</b>", style = list(fontSize = '16px', fontWeight = 'bold')),opposite=TRUE,
                              labels = list(style = list(fontSize = '12px', fontWeight = 'bold')))) %>% 
      hc_add_series(data = df1, "column", 
                    hcaes(x = endDate, y = fact, group = Element)) %>%
      hc_add_series(df1 %>% filter(Element == 'Current Assets'), 'spline',
                    hcaes(x= endDate, y = cR), name = 'Current Ratio', yAxis=1) %>%
      hc_subtitle(text = "Data Source: SEC XBRL",
                  align = 'left') %>%
      hc_caption(text = "Powered by Highcharts") %>%
      hc_colors(cols) %>%
      hc_plotOptions(
        series = list(
          showInLegend = FALSE,
          pointFormat = "{point.y}"
        ),
        column = list(
          colorByPoint = FALSE
        ))  #%>% 
    }
    }
    
  })
  
  output$netIncome <- renderHighchart({
    if(is.null(input$ticker)||input$ticker == ''||is.null(input$ticker1)||input$ticker1==''){
      NULL
    } else {
    
    df1 <- tables() %>% 
      filter(Element == 'us-gaap_NetIncomeLoss'|Element == 'us-gaap_ProfitLoss') %>%
      filter(!is.na(arcrole)) %>% filter(Period == 12) %>%
      filter(!duplicated(paste0(endDate, fact))) %>%
      mutate(Year = as.integer(substr(PERIOD, 3, 7))) %>%
      group_by(endDate, Element) %>% filter(Year == min(Year)) %>% ungroup() %>%
      select(endDate,Element,Year, fact) %>% group_by(endDate) %>% filter(Year == min(Year)) %>%
      ungroup() %>% group_by(endDate) %>% mutate(count = n()) %>% ungroup() %>%
      mutate(Element = replace(Element, count == 2 & Element == 'us-gaap_ProfitLoss', NA)) %>%
      filter(!is.na(Element)) %>% select(endDate, fact)
    
    #print(head(df1))
    if(nrow(df1) == 0){
      NULL
    } else {
    
    highchart() %>%
      hc_xAxis(title = list(text = ''), categories = unique(sort(df1$endDate)),
               labels = list(style = list(fontSize = '12px', fontWeight = 'bold')))%>%
      hc_yAxis(title = list(text = '<b>US$Millions</b>', style = list(fontSize = '16px', fontWeight = 'bold')),
               labels = list(style = list(fontSize = '12px', fontWeight = 'bold'))) %>%
      hc_add_series(data = df1, "column", hcaes(x = endDate, y = fact), name = 'Net Income') %>%
      hc_subtitle(text = "Data Source: SEC XBRL",
                  align = 'left') %>%
      hc_caption(text = "Powered by Highcharts") %>%
      hc_colors(cols) %>%
      hc_plotOptions(
        series = list(
          showInLegend = FALSE,
          pointFormat = "{point.y}"
        ),
        column = list(
          colorByPoint = FALSE
        ))  #%>% 
    }
    }
  })
  
  output$operIncome <- renderHighchart({
    if(is.null(input$ticker)||input$ticker == ''||is.null(input$ticker1)||input$ticker1==''){
      NULL
    } else {
      
      check1 <- tables() %>% 
        filter(Element == 'us-gaap_OperatingIncomeLoss') %>%
        mutate(Year = as.integer(substr(PERIOD,3,7))) %>%
        group_by(endDate, Period) %>% filter(Year == min(Year)) %>% ungroup() %>%
        filter(Period == 12) %>% filter(!duplicated(paste0(endDate, fact))) %>%
        select(Label, endDate, fact) %>% mutate(Label = 'Operating Income')
      if(nrow(check1)==0){
        check1 <- tables() %>%
          filter(grepl('IncomeLossFromCon', Element)) %>%
          filter(Period == 12) %>% filter(!is.na(arcrole)) %>%
          filter(endDate == max(endDate)) %>% filter(fact == min(fact))
        
        check1 <- tables() %>% 
          filter(Element ==check1$Element[1]) %>%
          mutate(Label = check1$Label) %>%
          filter(!is.na(arcrole)) %>%
          mutate(Year = as.integer(substr(PERIOD,3,7))) %>%
          group_by(endDate, Period) %>% filter(Year == min(Year)) %>% ungroup() %>%
          filter(Period == 12) %>% filter(!duplicated(paste0(endDate, fact))) %>%
          select(Label, endDate, fact) #%>% mutate(Label = 'Operating Income')
      }
      
      #print(head(df1))
      if(nrow(check1) == 0){
        NULL
      } else {
        
        highchart() %>%
          hc_xAxis(title = list(text = ''), categories = unique(sort(check1$endDate)),
                   labels = list(style = list(fontSize = '12px', fontWeight = 'bold')))%>%
          hc_yAxis(title = list(text = '<b>US$Millions</b>', style = list(fontSize = '16px', fontWeight = 'bold')),
                   labels = list(style = list(fontSize = '12px', fontWeight = 'bold'))) %>%
          hc_add_series(data = check1, "column", hcaes(x = endDate, y = fact), name = check1$Label[1]) %>%
          hc_subtitle(text = "Data Source: SEC XBRL",
                      align = 'left') %>%
          hc_caption(text = "Powered by Highcharts") %>%
          hc_colors(cols) %>%
          hc_plotOptions(
            series = list(
              showInLegend = FALSE,
              pointFormat = "{point.y}"
            ),
            column = list(
              colorByPoint = FALSE
            ))  #%>% 
      }
    }
  })
  
  output$debt <- renderHighchart({
    if(is.null(input$ticker)||input$ticker == ''||is.null(input$ticker1)||input$ticker1==''){
      NULL
    } else {
    
      df1 <- tables() %>% filter(Type == 'Statement') %>%
        filter(grepl('us-gaap_LongTermDebtCurrent', Element)|
                 grepl('us-gaap_LongTermDebtNoncurrent', Element)|
                 Element == 'us-gaap_LongTermDebt'|
                 Element == 'us-gaap_DebtCurrent'|
                 Element == 'us-gaap-DebtNoncurrent'|
                 Element == 'us-gaap_LongTermDebtAndCapitalLeaseObligations'|
                 Element == 'us-gaap_SecuredDebtMember'|
                 Element == 'us-gaap_UnsecuredDebtMember'|
                 Element == 'us-gaap_UnsecuredDebtCurrent') %>%
        filter(!duplicated(paste0(fact, endDate, Element))) %>% 
        mutate(quarter = as.numeric(substr(PERIOD, 2, 2))*3, year = as.numeric(substr(PERIOD, 3, 7)) ) %>%
        mutate(date = as.POSIXct(paste0(quarter, '/01/', year), format = '%m/%d/%Y')) %>%
        group_by(Element, endDate) %>% filter(date == min(date)) %>%ungroup() %>%
        group_by(Element, endDate) %>% summarise(fact = mean(fact)) %>% arrange(Element, endDate) %>%
        mutate(fact = round(fact/1000000, 0)) %>% mutate(endDate = as.POSIXct(endDate, format = '%Y-%m-%d')) %>%
        mutate(endDate = as.character(as.Date(endDate)))
      
      #print(head(df1))
      
      highchart() %>%
        hc_xAxis(title = list(text = ''), categories = unique(sort(df1$endDate)),
                 labels = list(style = list(fontSize = '12px', fontWeight = 'bold')))%>%
        hc_yAxis(title = list(text = '<b>US$Millions</b>', style = list(fontSize = '16px', fontWeight = 'bold')),
                 labels = list(style = list(fontSize = '12px', fontWeight = 'bold'))) %>%
        hc_add_series(data = df1, "column", hcaes(x = endDate, y = fact, group = Element)) %>%
        hc_subtitle(text = "Data Source: SEC XBRL",
                    align = 'left') %>%
        hc_caption(text = "Powered by Highcharts") %>%
        hc_colors(cols) %>%
        hc_plotOptions(
          series = list(
            showInLegend = FALSE,
            pointFormat = "{point.y}"
          ),
          column = list(
            colorByPoint = FALSE
          ))  #%>% 
    }
    
  })
  
  output$cf <- renderHighchart({
    if(is.null(input$ticker)||input$ticker == ''||is.null(input$ticker1)||input$ticker1==''){
      NULL
    } else {
    # df1 <- tables() %>% filter(Type == 'Statement') %>% filter(!is.na(arcrole)) %>%
    #   filter(Period == 12) %>%
    #   filter(grepl('us-gaap_NetCashProvidedByUsedInOperatingActivities', Element)|
    #            grepl('us-gaap_NetCashProvidedByUsedInInvestingActivities', Element)|
    #            grepl('us-gaap_NetCashProvidedByUsedInFinancingActivities', Element)) %>%
    #   mutate(Element = replace(Element, grepl('Operating', Element), 'Operating Activities'),
    #          Element = replace(Element, grepl('Investing', Element), 'Investing Activities'),
    #          Element = replace(Element, grepl('Financing', Element), 'Financing Activities')) %>%
    #   filter(!duplicated(paste0(fact, endDate, Element))) %>%
    #   mutate(quarter = as.numeric(substr(PERIOD, 2, 2))*3, year = as.numeric(substr(PERIOD, 3, 7)) ) %>%
    #   mutate(date = as.POSIXct(paste0(quarter, '/01/', year), format = '%m/%d/%Y')) %>%
    #   group_by(Element, endDate) %>% filter(date == min(date)) %>%ungroup() %>%
    #   group_by(Element, endDate) %>% summarise(fact = mean(fact)) %>% arrange(Element, endDate) %>%
    #   mutate(fact = round(fact/1000000, 0)) %>% filter(!duplicated(paste0(endDate, fact))) %>% arrange((Element), endDate) %>%
    #   group_by(endDate) %>% mutate(fact1 = cumsum(fact)) %>% ungroup() %>% group_by(endDate) %>%
    #   mutate(cf = sum(fact)) %>% ungroup()
    df1 <- tables() %>% filter(!is.na(arcrole)) %>% filter(grepl('us-gaap', Element)) %>% 
      filter(grepl('OperatingActi', Element)|
               grepl('InvestingAct', Element)|
               grepl('FinancingAct', Element)) %>%
      mutate(Year = as.integer(substr(PERIOD, 3, 7))) %>% filter(Period %in% c(3,6,9,12)) %>%
      group_by(endDate, Period, Element) %>% filter(Year == min(Year)) %>% ungroup() %>%
      arrange(endDate, PERIOD) %>% mutate(Month = Period) %>% group_by(endDate) %>%
      filter(Period == max(Period)) %>% ungroup() %>%
      arrange(Element, endDate) %>% mutate(Year = year(endDate)) %>%
      mutate(Month = paste0('Q', Month)) %>% group_by(Month) %>% summarise(count=n()) %>% ungroup()
    if(nrow(df1) < 4) {
      NULL
    } else {
    
      df1 <- tables() %>% filter(!is.na(arcrole)) %>% filter(grepl('us-gaap', Element)) %>% 
        filter(grepl('OperatingActi', Element)|
                 grepl('InvestingAct', Element)|
                 grepl('FinancingAct', Element)) %>% 
        filter(!grepl('Continu', Element)) %>%
        mutate(Year = as.integer(substr(PERIOD, 3, 7))) %>%
        group_by(Element, endDate) %>% filter(Year == min(Year)) %>% ungroup() %>%
        filter(Period %in% c(3,6,9,12)) %>% group_by(endDate) %>% filter(Period == max(Period)) %>% ungroup() %>%
        arrange(Element, endDate, Period) %>% 
        mutate(order = 0, order = replace(order, Period == 12, 1)) %>% group_by(Element) %>% mutate(order = cumsum(order)) %>%
        ungroup() %>% mutate(order = replace(order, Period == 12, order[Period == 12]-1)) %>%
        mutate(Period = paste0('Q',Period), Period1 = Period) %>%spread(Period, fact) %>% arrange(Element, order, endDate) %>%
        select(Element, Period1, endDate, order, Q3, Q6, Q9, Q12) %>%  distinct() %>%
        group_by(Element, order) %>% 
        mutate(Q3 = mean(Q3, na.rm=TRUE),
               Q6 = mean(Q6, na.rm =TRUE),
               Q9 = mean(Q9, na.rm=TRUE),
               Q12 = mean(Q12, na.rm=TRUE)) %>%
        ungroup() %>% mutate(Q3 = replace(Q3, is.nan(Q3), 0),
                             Q6 = replace(Q6, is.nan(Q6), 0),
                             Q9 = replace(Q9, is.nan(Q9), 0),
                             Q12 = replace(Q12, is.nan(Q12), 0)) %>%
        mutate(Q12 = Q12-Q9, Q9=Q9-Q6, Q6=Q6-Q3) %>% gather(Period, fact, -c(Element, Period1, endDate, order)) %>%
        filter(Period == Period1) %>% subset(select = -c(Period1)) %>% arrange(Element, endDate) %>%
        group_by(endDate) %>% mutate(cf = sum(fact, na.rm=TRUE)) %>% ungroup()%>%
        group_by(endDate) %>% filter(n() == 3) %>% ungroup() %>%
        mutate(Element = replace(Element, grepl('Financing', Element), 'Financing'),
               Element = replace(Element, grepl('Investing', Element), 'Investing'),
               Element = replace(Element, grepl('Operating', Element), 'Operating')) %>%
        mutate(fact = fact/1000000, cf = cf/1000000)
     #  
     #  print(head(df1))
      
      highchart() %>%
        hc_xAxis(title = list(text = ''), categories = unique(sort(df1$endDate)),
                 #type = 'datetime', dateTimeLabelFormats = list(month = "%b", year = "%y"),
                 labels = list(style = list(fontSize = '12px', fontWeight = 'bold')))%>%
        hc_yAxis(title = list(text = '<b>US$Millions</b>', style = list(fontSize = '16px', fontWeight = 'bold')),
                 labels = list(style = list(fontSize = '12px', fontWeight = 'bold'))) %>%
        hc_add_series(data = df1, "column", hcaes(x = endDate, y = fact, group = Element)) %>%
        hc_add_series(data = df1 %>% filter(Element ==Element[1]), 'spline', 
                      hcaes(x=endDate, y =cf), name = 'Cash Flow') %>%
        hc_subtitle(text = "Data Source: SEC XBRL",
                    align = 'left') %>%
        hc_caption(text = "Powered by Highcharts") %>%
        hc_colors(cols) %>%
        hc_plotOptions(
          series = list(
            showInLegend = TRUE,
            pointFormat = "{point.y}"
          ),
          column = list(
            colorByPoint = FALSE
          ))  #%>% 
       # hc_xAxis(, dateTimeLabelFormats = list(day = '%d of %b')) %>%
    }
    }
  })
  
  
  output$maturity <- renderHighchart({
    if(is.null(input$ticker)||input$ticker == ''||is.null(input$ticker1)||input$ticker1==''){
      NULL
    } else {
    df1 <- tables() %>% filter(fact > 1) %>%
      filter(grepl('2021', Label)|
               grepl('2022', Label)|
               grepl('2023', Label)) %>%
      filter(!grepl('eases', MainElement)) %>%
      filter(grepl('ebt', MainElement)) %>%
      filter(grepl('us-gaap', Element)) %>% select(Table) %>%
      left_join(tables()) %>% mutate(Year = as.integer(substr(PERIOD, 3, 7))) %>%
      filter(Year == max(Year)) %>%
      filter(endDate == max(endDate)) %>%
      distinct() %>% mutate(fact = fact/1000000) %>%
      select(Year = Label, Maturing.Debt = fact, PERIOD)
    
      if(nrow(df1) == 0){
        NULL
      } else {
    highchart() %>%
      hc_add_series(data = df1, "column", hcaes(x = Year, y = Maturing.Debt), name = 'Maturing Debt') %>%
      hc_subtitle(text = paste0('As of: ', df1$PERIOD[1]),
                  align = 'left') %>%
      hc_caption(text = "Powered by Highcharts") %>%
      hc_colors(cols) %>%
       hc_plotOptions(
         series = list(
           showInLegend = FALSE,
           pointFormat = "{point.y}"
         ),
         column = list(
           colorByPoint = FALSE
         )) %>%
      hc_xAxis(title = list(text = ''), categories = df1$Year, 
               labels = list(style = list(fontSize = '12px', fontWeight = 'bold'))) %>%
      hc_yAxis(title = list(text = '<b>US$Millions</b>', style = list(fontSize = '16px', fontWeight = 'bold')),
               labels = list(style = list(fontSize = '12px', fontWeight = 'bold')))
    
    
      }
    }
  })
  
  observe({
    if(is.null(input$ticker)||input$ticker == ''||is.null(input$ticker1)||input$ticker1==''||is.null(values$x)){
      NULL
    } else {

    operatorSelect <- stockInfo()$symbol
    #print(operatorSelect)
    filingList <- data.frame(edgarWebR::company_details(operatorSelect, type = '10-K', count = 5)) %>% filter(!grepl('A', filings.type))
    filingList1 <- data.frame(edgarWebR::company_details(operatorSelect, type = '10-Q', count = 12)) %>% filter(!grepl('A', filings.type))
    
    filingList <- rbind(filingList[1:3,], filingList1) %>% arrange(desc(filings.filing_date))
    
    compInfo <- finreportr::CompanyInfo(operatorSelect)
    filingList$Company <- compInfo$company
    rm(filingList1)
    filingList <- filingList %>% filter(!is.na(information.business_city))
    filingList$url1 <- lapply(filingList$filings.href,  url1)
    filingList$url1 <- gsub('/ix?doc=', '', filingList$url1, fixed=TRUE)
    #print(head(filingList))

    
    filingList <- filingList[,c('Company', 'filings.filing_date', 'filings.type', 'url1')] %>% arrange(desc(filings.filing_date))
    filingList$quarter <- lubridate::quarter(filingList$filings.filing_date) - 1
    filingList$year <- lubridate::year(filingList$filings.filing_date)
    filingList$quarter[filingList$quarter == 0] <- 4
    filingList$year[filingList$quarter == 4] <- filingList$year[filingList$quarter == 4]-1
    filingList$period <- paste0('Q', filingList$quarter, filingList$year)
    names(filingList)[1:4] <- c('Company', 'filingDate', 'type', 'url1')
    filingList <- filingList[,c('Company', 'period', 'filingDate', 'type', 'url1')]
    #print(head(filingList))
    filingList <- data.frame(lapply(filingList, function(x){
      gsub("iXBRL", "", x)
    }))
    filingList <- data.frame(lapply(filingList, function(x){
      gsub("\\s+", "", x)
    }))
    
    filingList$type <- paste0('<a href="', filingList$url1, '" target="_blank">', filingList$type,'</a>')

    filingList <- filingList[,c('Company', 'period', 'filingDate', 'type')]
    names(filingList) <- c('Company', 'Filing Period', 'Filing Date', 'Report')
    filingList <- as.data.frame(filingList)
    values$filingList1 <- filingList
    }
  })
  
  # output$recent <- DT::renderDataTable({
  #   if(is.null(input$ticker)||input$ticker == ''||is.null(input$ticker1)||input$ticker1==''){
  #     NULL
  #   } else {
  #   # df1 <- tables() %>% filter(!is.na(arcrole)) %>%
  #   #   group_by(endDate) %>%
  #   #   filter(n() > 8) %>% ungroup() %>%
  #   #   filter(endDate == max(endDate)) %>%
  #   #   mutate(fact = round(fact/1000000,2)) %>%
  #   #   filter(!duplicated(paste0(Element,Period))) %>%
  #   #   select(Table, Label, Date = endDate, Months = Period,
  #   #          Value = fact, Period = PERIOD)
  #   #names(df1)[ncol(df1)] <- 'Filing Period'
  #   df1 <- tidyfeed(glue::glue("https://news.google.com/rss/search?q=${stockInfo()$symbol}"))
  #   df1 <- df1 %>% arrange(desc(feed_pub_date))
  #   df1 <- df1[1:10,]
  #   df1$url1 <- paste0('<a href=\"', df1$feed_link, '\" target=\"_blank\">', df1$item_title, '</a>&nbsp;&nbsp;<font color=\"#6f6f6f\">', df1$feed_description, '</font>')
  #   df1 <- df1 %>% select(date = feed_pub_date, url1) %>% mutate(date = as.Date(date))
  #   names(df1) <- c('', '')
  #   DT::datatable(df1, escape = FALSE, rownames = FALSE,  options = list(paging = FALSE, searching = FALSE))
  #   }
  # })
  
  output$filingList <- DT::renderDataTable({
    if(is.null(values$filingList1)){
      NULL
    } else {
      filingList <- values$filingList1
      filingList <- subset(filingList, select = -c(Company))
      DT::datatable(filingList, escape = FALSE, rownames = FALSE, options = list(paging = FALSE, searching = FALSE))
    }
  })
  
  output$bsData <- renderHighchart({
    if(is.null(input$ticker)||input$ticker == ''||is.null(input$ticker1)||input$ticker1==''){
      NULL
    } else {
    check1 <- tables() %>% 
      filter(Element == 'us-gaap_Assets'|Element == 'us-gaap_AssetsCurrent'|
               Element == 'us-gaap_LiabilitiesCurrent'|
               Element == 'us-gaap_StockholdersEquity') %>%
      filter(!is.na(arcrole)) %>% filter(endDate == max(endDate)) %>%
      filter(!duplicated(Element)) %>% mutate(Element = gsub('us-gaap_', '', Element, fixed=TRUE)) %>%
      select(Element,endDate, fact) 
    
    if(nrow(check1)<4){
      NULL
    } else {
      
      check1 <- check1 %>% spread(Element, fact) %>%
        mutate(Liabilities = Assets - StockholdersEquity, AssetsNonCurrent = Assets-AssetsCurrent,
               LiabilitiesNonCurrent = Liabilities - LiabilitiesCurrent) %>%
        gather(Component, Value, -endDate) %>% filter(Component != 'Assets') %>% 
        filter(Component != 'Liabilities') %>% arrange(Component) %>%
        mutate(Component = replace(Component, Component == 'AssetsCurrent', 'Current Assets'),
               Component = replace(Component, Component == 'AssetsNonCurrent', 'NonCurrent Assets'),
               Component = replace(Component, Component == 'LiabilitiesCurrent', 'Current Liabilities'),
               Component = replace(Component, Component == 'LiabilitiesNonCurrent', 'NonCurrent Liabilities'),
               Component = replace(Component, Component == 'StockholdersEquity', 'Stockholders Equity')) %>%
        mutate(Value = round(Value/1000000, 0))
      
    highchart() %>%
      hc_add_series(check1, type = 'pie', hcaes(x = Component, y = Value, name = Component), name = '') %>%
      hc_title(text = paste0('Balance Sheet as of: ', check1$endDate[1]), align = 'left',
                  style = list(color = cols[6], fontWeight = 'bold')) %>%
      hc_subtitle(text = 'US$Millions',
               style = list(color = cols[10], fontWeight = 'bold'), align = 'left') %>%
      hc_colors(cols[6:13])
    }
    }
  })
  
  output$plot <- renderUI({
    if(is.null(input$ticker)||input$ticker == ''||is.null(input$ticker1)||input$ticker1==''){
      NULL
    } else {
    comp.ticker <- stockInfo()$symbol
    #comp.ticker <- comp.ticker$ticker[1]
    df <- compScrape(comp.ticker)
    
    HTML(df$list1)
    }
  })
  
  }



shinyApp(ui, server)
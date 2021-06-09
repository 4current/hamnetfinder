#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(httr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Ham Net Finder"),
  
  tabsetPanel(
    tabPanel(
      "D-Star, Echolink and D-Rats Nets",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              "modeFilter",
              "Mode",
              c("D-Star","D-Rats", "Echolink")),
            selectInput(
              "zone",
              "Time Zone",
              c("Eastern","Central","Mountain","Pacific")),
            dateInput("date", "Day", value = NULL, min = NULL, max = NULL,
                      format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                      language = "en", width = NULL)
          ),
          
          # Show a plot of the generated distribution
          mainPanel(
            strong(h4(textOutput("dayLabel"))),
            htmlOutput("infotext"),
            tableOutput("listings")
          )
      )
    ),
    tabPanel(
      "ARRL Net Search",
      "Under Construction"
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  days <- c("Su","Mo","Tu","We","Th","Fr","Sa")
  src_page <- "https://www.theweatherwonder.com/elk.htm"
  wx4qz_qrz_page <- "https://www.qrz.com/lookup?tquery=WX4QZ&mode=callsign"
  output$infotext <- renderText(
    as.character(
      span("This data is taken from", a(src_page,href=src_page),
          ", where you can download the complete spreadsheets, maintained courtesy of", 
          a("WX4QZ", href=wx4qz_qrz_page),
          "."
        )
      )
    )
  
  get_todays_ham_nets <- function(zone) {
    # Gets today's xlsx file by time zone if it is not already on disk
    # Always returns the name of the data file
    thisDay <- Sys.Date()
    data_file <- paste0(zone,"_ham_nets_",thisDay,".Rda")
    if ( !file.exists(data_file) ) {
      xlsx_url <- paste0('https://www.theweatherwonder.com/Net_List_Spreadsheet_',zone,'_Time.xlsx')
      GET(xlsx_url, write_disk(tf <- tempfile(fileext = ".xlsx")))
      test <- read_excel(tf, sheet = 1)
      col_type_vec <- c("text", rep("guess", times=7), "date", "date", rep("text", times=3),"skip")
      xlsx <- read_excel(tf, sheet=1, col_types=col_type_vec[1:ncol(test)], skip =1)
      xlsx[,2:8] <- !is.na(xlsx[,2:8])
      xlsx[[zone]] <- format(xlsx[[zone]], format='%-I:%M_%p')
      xlsx[["UTC"]] <- format(xlsx[["UTC"]], format='%H%M')
      xlsx[["Node"]] <- gsub("\\s", "", xlsx[["Node"]])
      xlsx[["Node"]] <- gsub("&", " ", xlsx[["Node"]])
      xlsx[["Node"]] <- sub("^([1-9][A-Z])", "REF00\\1", xlsx[["Node"]])
      xlsx[["Node"]] <- sub("^([1-9][0-9][A-Z])", "REF0\\1", xlsx[["Node"]])
      xlsx[["Node"]] <- sub("^([1-9][0-9]{2}[A-Z])", "REF\\1", xlsx[["Node"]])
      xlsx[["Node"]] <- sub("^REF([1-9][A-Z])", "REF00\\1", xlsx[["Node"]])
      xlsx[["Node"]] <- sub("^REF([1-9][0-9][A-Z])", "REF0\\1", xlsx[["Node"]])
      save(xlsx, file=data_file)
    }
    data_file
  }
  
  output$dayLabel <- reactive({
    paste(
      input$modeFilter,
      "nets for",
      format(input$date, format="%A, %B %-d, %Y")
    )
  })
  
  output$listings <- renderTable({
    load(file = get_todays_ham_nets(input$zone))
    byMode <- xlsx[xlsx["Mode"]==input$modeFilter,]
    byDayAndMode <- byMode[byMode[days[as.POSIXlt(input$date)$wday + 1]]==TRUE,]
    byDayAndMode[,c(input$zone, "UTC", "Net name","Node","Comment")]
  }, na="", align='r??r?')
}

# Run the application 
shinyApp(ui = ui, server = server)

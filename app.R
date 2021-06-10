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
library(sf)
library(spData)
library(dplyr)
library(rvest)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  tags$script('
  $(document).ready(function () {
    navigator.geolocation.getCurrentPosition(onSuccess, onError);

    function onError (err) {
    Shiny.onInputChange("geolocation", false);
    }
    
   function onSuccess (position) {
      setTimeout(function () {
          var coords = position.coords;
          console.log(coords.latitude + ", " + coords.longitude);
          Shiny.onInputChange("geolocation", true);
          Shiny.onInputChange("lat", coords.latitude);
          Shiny.onInputChange("long", coords.longitude);
      }, 1100)
  }
  });
  '),
  
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
     sidebarLayout(
        sidebarPanel(
          selectInput(
            "arrl_state",
            "State",
            state.name
          ),
          selectInput(
            "arrl_band",
            "Band",
            c("160M"="1800-2000-HF",
              "80M"="3500-4000-HF",
              "40M"="7000-7300-HF",
              "30M"="10100-10150-HF",
              "20M"="14000-14350-HF",
              "17M"="18068-18168-HF",
              "15M"="21000-21450-HF",
              "10M"="28000-29700-HF",
              "6M"="50-54-VHF",
              "2M"="144-148-VHF",
              "1.25M"="222-225-VHF",
              "70CM"="420-450-UHF",
              "23CM"="1240-1300-UHF")
          ),
          dateInput("arrl_date", "Day", value = NULL, min = NULL, max = NULL,
                    format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                    language = "en", width = NULL)
        ),
        # Show a plot of the generated distribution
        mainPanel(
          strong(h4(textOutput("arrl_heading"))),
          htmlOutput("arrl_info"),
          tableOutput("arrl_table")
        )
      )
    )
    
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
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
  
  ## pointsDF: A data.frame whose first column contains longitudes and
  ##           whose second column contains latitudes.
  ##
  ## states:   An sf MULTIPOLYGON object with 50 states plus DC.
  ##
  ## name_col: Name of a column in `states` that supplies the states'
  ##           names.
  gps_to_state <- function(latitude,
                           longitude,
                           states = spData::us_states,
                           name_col = "NAME") {
    ## Convert points data.frame to an sf POINTS object
    pts <- st_as_sf(data_frame(long=longitude,lat=latitude), coords = 1:2, crs = 4326)
    
    ## Transform spatial data to some planar coordinate system
    ## (e.g. Web Mercator) as required for geometric operations
    states <- st_transform(states, crs = 3857)
    pts <- st_transform(pts, crs = 3857)
    
    ## Find names of state (if any) intersected by each point
    state_names <- states[[name_col]]
    ii <- as.integer(st_intersects(pts, states))
    state_names[ii]
  }
  
  observe(
    if (!(is.null(input$lat) || is.null(input$long)))  {
      updateSelectInput(session, "arrl_state",
                        selected = gps_to_state(input$lat, input$long)
      )
    }
  )
  
  arrl_src <- "http://www.arrl.org/arrl-net-directory-search"
  output$arrl_info <- renderText(
    as.character(
      span("This data is queried from", a(arrl_src,href=arrl_src),
           ", where you will find more query features.", "Dy = Daily, Sn = Sunday"
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
  
  query_arrl_nets <- function(dayFilter, stateFilter, bandFilter) {
    search_client_url <- "http://www.arrl.org/resources/nets/client/netsearch.html"
    vals <- list(
      netype = "%L%",
      state = stateFilter,
      netname = "",
      dow = dayFilter,
      freq = bandFilter,
      ntfs = "%%"
    )
    html <- read_html(search_client_url)
    search <- html_form(html)[[1]] %>% html_form_set(!!!vals)
    resp <- html_form_submit(search)
    net_table <- read_html(resp) %>% 
      html_element("table") %>% 
      html_table() %>%
      select(-5) %>%
      rename("Local Days" = ends_with("Local Days")) %>%
      select("Local Time", "Net Name", "Frequency", "Local Days")
    net_table
  }
  
  output$arrl_heading <- reactive({
    paste(
      "Local nets for",
      format(input$arrl_date, format="%A, %B %-d, %Y"),
      "in",
      input$arrl_state
    )
  })
  
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
  
  output$arrl_table <- renderTable({
    days <- c("%Sn%","%M%","%T","%W%","%Th%","%F%","%S")
    query_arrl_nets(
      days[as.POSIXlt(input$arrl_date)$wday + 1],
      input$arrl_state,
      input$arrl_band
    )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

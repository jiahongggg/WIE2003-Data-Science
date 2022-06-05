library(shiny)
library(DT)


html_result_table <- '<div class="result_table">_html</div>'
html_result <- '<div class="result">
            <div class="left_div">
                <div class="left_top">
                    <div class="lt_1">
                        <img id="image" src="_imglink">
                    </div>
                    <div class="lt_2">
                        <div class="lt_2_t">
                            _time
                        </div>
                        <div class="lt_2_b">
                            _location
                        </div>
                    </div>
                    <div class="lt_3">
                        _period
                    </div>
                </div>
                <div class="left_bottom">
                    _airline
                </div>
            </div>
            <div class="right_div">
                <div class="right_top">
                    _price
                </div>
                <div class="right_bottom">
                    <a href="_airline_website" target="_blank">
                        <button class="button">
                            View Deal
                        </button>
                    </a>
                </div>
            </div>
        </div>
        '

# Reading a CSV File
economyData <-
  read.csv("/Users/simjiahong/RStudio/Group Project/economy.csv")
businessData <-
  read.csv("/Users/simjiahong/RStudio/Group Project/business.csv")

img_link  <- c('')

# Replace all the matches of a Pattern from a String
economyData$price <- gsub(",", "", economyData$price)
businessData$price <- gsub(",", "", businessData$price)

# Convert Strings to Integers
economyData$price <- strtoi(economyData$price)
businessData$price <- strtoi(businessData$price)

# Convert to RM and round to two decimal places
# Find min and max
min <- round(min(economyData$price) * 0.056, 0) - 1
max <- round(max(businessData$price) * 0.056, 0) + 1


airline_name <-
  c(
    "SpiceJet" ,
    "AirAsia"  ,
    "Vistara"  ,
    "GO FIRST"  ,
    "Indigo"   ,
    "Air India",
    "Trujet"   ,
    "StarAir"
  )

image <-
  c(
    "https://github.com/jiahongggg/WIE2003-Data-Science/blob/main/Group%20Project/spicejet.jpg?raw=true",
    "https://github.com/jiahongggg/WIE2003-Data-Science/blob/main/Group%20Project/airasia%20logo.png?raw=true",
    "https://github.com/jiahongggg/WIE2003-Data-Science/blob/main/Group%20Project/vistara.png?raw=true",
    "https://github.com/jiahongggg/WIE2003-Data-Science/blob/main/Group%20Project/go%20first.png?raw=true",
    "https://github.com/jiahongggg/WIE2003-Data-Science/blob/main/Group%20Project/indigo.jpg?raw=true",
    "https://raw.githubusercontent.com/jiahongggg/WIE2003-Data-Science/d0a22ee86482539f0aecf512a2b6800647be8a30/Group%20Project/air%20india.png",
    "https://github.com/jiahongggg/WIE2003-Data-Science/blob/main/Group%20Project/trujet.png?raw=true",
    "https://github.com/jiahongggg/WIE2003-Data-Science/blob/main/Group%20Project/starair.png?raw=true"
  )

web_link <- c(
  "https://www.spicejet.com/",
  "https://www.airasia.com/en/gb",
  "https://www.airvistara.com/sg/en",
  "https://www.flygofirst.com",
  "https://www.goindigo.in/",
  "https://www.airindia.in/",
  "https://www.kiwi.com/en/airline/2t/trujet/",
  "https://starair.in/"
)

img_info <-
  data.frame(airline_name = airline_name,
             image = image,
             web_link = web_link)


total_origin <- unique(economyData$from)
total_destination <- unique(economyData$to)
total_airline <- unique(economyData$airline)

# UI
ui <- fluidPage(
  includeCSS("/Users/simjiahong/RStudio/Group Project/main.css"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "origin",
        label = "Origin",
        choices = total_origin
      ),
      selectInput(
        inputId = 'destination',
        label = "Destination",
        choices = total_destination
      ),
      dateInput(
        inputId = 'des_date',
        label = "Depart",
        min = "2022-02-11",
        max = "2022-03-31",
        format = "dd/mm/yyyy"
      ),
      selectInput(
        inputId = 'type',
        label = "Cabin class",
        choices = c('Economy', 'Business')
      ),
      checkboxGroupInput(
        inputId = 'airline',
        label = "choose airline",
        choiceNames = total_airline,
        choiceValues = total_airline,
        inline = TRUE
      ),
      sliderInput(
        inputId = 'range_price',
        label = 'Price (RM)',
        value = c(min, max),
        min = min,
        max = max,
        dragRange = TRUE
      ),
      selectInput(
        inputId = 'sort_para',
        label = '',
        choices = c('Price', 'Time taken', 'Departure time')
      ),
      actionButton(inputId = 'sort',
                   label = "Sort")
    ),
    mainPanel(tabsetPanel(
      tabPanel("Flights", uiOutput("below_table")),
      tabPanel("Summary", verbatimTextOutput("summary")),
      tabPanel("Dataset", dataTableOutput("table"))
    ))
  )
)

# Server logic
server <- function(input, output) {
  observeEvent(input$sort,  {
    output$below_table <- renderUI({
      origin <- input$origin
      destination <- input$destination
      des_date <- input$des_date
      type <- input$type
      airline <- input$airline
      sort_para <- input$sort_para
      range_price <-  input$range_price
      temp_dataset <- NULL
      
      if (type == "Economy") {
        temp_dataset <- economyData[which(economyData$from == origin),]
        temp_dataset <-
          temp_dataset[which(temp_dataset$to == destination), ]
        temp_dataset <-
          temp_dataset[which(temp_dataset$date == format(des_date, format = "%d/%m/%Y")), ]
        print(format(des_date, format = "%d/%m/%Y"))
        temp_dataset <-
          temp_dataset[which(strtoi(temp_dataset$price) > strtoi(range_price[1]) /
                               0.056), ]
        temp_dataset <-
          temp_dataset[which(strtoi(temp_dataset$price) < strtoi(range_price[2]) /
                               0.056), ]
        temp_dataset <-
          temp_dataset[which(temp_dataset$airline == airline),]
      }
      else{
        temp_dataset <- businessData[which(businessData$from == origin), ]
        temp_dataset <-
          temp_dataset[which(temp_dataset$to == destination),]
        temp_dataset <-
          temp_dataset[which(temp_dataset$date == format(des_date, format = "%d/%m/%Y")),]
        print(format(des_date, format = "%d/%m/%Y"))
        temp_dataset <-
          temp_dataset[which(strtoi(temp_dataset$price) > strtoi(range_price[1]) /
                               0.056),]
        temp_dataset <-
          temp_dataset[which(strtoi(temp_dataset$price) < strtoi(range_price[2]) /
                               0.056),]
        temp_dataset <-
          temp_dataset[which(temp_dataset$airline == airline), ]
      }
      if (sort_para == 'Price') {
        temp_dataset <- temp_dataset[order(temp_dataset$price), ]
      } else if (sort_para == 'Time taken') {
        temp_dataset <- temp_dataset[order(temp_dataset$time_taken), ]
      }
      else if (sort_para == 'Departure time') {
        temp_dataset <- temp_dataset[order(temp_dataset$dep_time), ]
      }
      
      result_contain <- ''
      if (nrow(temp_dataset) != 0) {
        for (y in 1:nrow(temp_dataset)) {
          temp <- html_result
          temp <-
            gsub('_location',
                 paste(temp_dataset[y,]$from, paste('-', temp_dataset[y,]$to)),
                 temp)
          temp <-
            gsub('_price', paste('RM', formatC(
              round(strtoi(temp_dataset[y,]$price) * 0.056, 2), 2, format = 'f'
            )) , temp)
          temp <-
            gsub('_period', paste0(
              as.integer(temp_dataset[y,]$time_taken / 60),
              paste("h", paste0(
                as.integer(temp_dataset[y,]$time_taken %% 60), 'm'
              ))
            ), temp)
          temp <-
            gsub('_time',
                 paste(
                   temp_dataset[y,]$dep_time,
                   paste(' - ', temp_dataset[y,]$arr_time)
                 ),
                 temp)
          temp <-
            gsub('_imglink', img_info[which(img_info$airline_name == temp_dataset[y,]$airline),]$image, temp)
          temp <-
            gsub('_airline_website', img_info[which(img_info$airline_name == temp_dataset[y,]$airline),]$web_link, temp)
          temp <- gsub('_airline', temp_dataset[y,]$airline, temp)
          
          result_contain <- paste(result_contain, temp, sep = ' ')
        }
      }
      result_contain <-
        gsub('_html', result_contain, html_result_table)
      HTML(result_contain)
      
    })
  })
  output$summary <- renderPrint({
    summary(economyData)
  })
  
  output$table = DT::renderDataTable({
    economyData
  })
  
}

# Complete app with UI and server components
shinyApp(ui, server)

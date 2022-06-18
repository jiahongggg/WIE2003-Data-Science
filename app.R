library(dplyr)
library(DT)
library(ggplot2)
library(rjson)
library(rsconnect)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(sodium)

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
  read.csv(
    "https://raw.githubusercontent.com/jiahongggg/WIE2003-Data-Science/main/data/economy.csv"
  )
businessData <-
  read.csv(
    "https://raw.githubusercontent.com/jiahongggg/WIE2003-Data-Science/main/data/business.csv"
  )
#very important not remove it
colnames(economyData)[1] <- "date"
colnames(businessData)[1] <- "date"

img_link  <- c('')
passkey <- sha256(charToRaw("password123"))
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
    "https://github.com/jiahongggg/WIE2003-Data-Science/blob/main/image/spicejet.jpg?raw=true",
    "https://github.com/jiahongggg/WIE2003-Data-Science/blob/main/image/airasia%20logo.png?raw=true",
    "https://github.com/jiahongggg/WIE2003-Data-Science/blob/main/image/vistara.png?raw=true",
    "https://github.com/jiahongggg/WIE2003-Data-Science/blob/main/image/go%20first.png?raw=true",
    "https://github.com/jiahongggg/WIE2003-Data-Science/blob/main/image/indigo.jpg?raw=true",
    "https://github.com/jiahongggg/WIE2003-Data-Science/blob/main/image/air%20india.png?raw=true",
    "https://github.com/jiahongggg/WIE2003-Data-Science/blob/main/image/trujet.png?raw=true",
    "https://github.com/jiahongggg/WIE2003-Data-Science/blob/main/image/starair.png?raw=true"
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
# Main login screen
loginpage <-
  div(
    id = "loginpage",
    style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
    wellPanel(
      tags$h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
      textInput(
        "userName",
        placeholder = "Username",
        label = tagList(icon("user"), "Username")
      ),
      passwordInput(
        "passwd",
        placeholder = "Password",
        label = tagList(icon("unlock-alt"), "Password")
      ),
      br(),
      div(
        style = "text-align: center;",
        actionButton(
          "login",
          "SIGN IN",
          style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"
        ),
        shinyjs::hidden(div(
          id = "nomatch",
          tags$p(
            "Oops! Incorrect username or password!",
            style = "color: red; font-weight: 600;
                                            padding-top: 5px;font-size:16px;",
            class = "text-center"
          )
        )),
      ),
      br(),
      div(
        style = "text-align: center;",
        actionButton(
          "register",
          "SIGN UP",
          style = "color: white; background-color:#3c8dbc;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"
        )
      )
    )
  )


header <-
  dashboardHeader(title = "Simple Dashboard", uiOutput("logoutbtn"))

sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))
body <- dashboardBody(shinyjs::useShinyjs(), uiOutput("body"))
ui <- dashboardPage(header, sidebar, body, skin = "blue")

server <- function(input, output, session) {
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
        temp_dataset <- economyData[which(economyData$from == origin), ]
        temp_dataset <-
          temp_dataset[which(temp_dataset$to == destination),]
        
        temp_dataset <-
          temp_dataset[which(temp_dataset$date == format(des_date, format = "%d/%m/%Y")),]
        print(format(des_date, format = "%d/%m/%Y"))
        print(nrow(temp_dataset))
        temp_dataset <-
          temp_dataset[which(strtoi(temp_dataset$price) > strtoi(range_price[1])),]
        temp_dataset <-
          temp_dataset[which(strtoi(temp_dataset$price) < strtoi(range_price[2])),]
        temp_dataset <-
          temp_dataset[temp_dataset$airline %in% airline,]
      }
      else{
        temp_dataset <- businessData[which(businessData$from == origin),]
        temp_dataset <-
          temp_dataset[which(temp_dataset$to == destination), ]
        temp_dataset <-
          temp_dataset[which(temp_dataset$date == format(des_date, format = "%d/%m/%Y")), ]
        print(format(des_date, format = "%d/%m/%Y"))
        print(nrow(temp_dataset))
        temp_dataset <-
          temp_dataset[which(strtoi(temp_dataset$price) > strtoi(range_price[1]) /
                               0.056), ]
        temp_dataset <-
          temp_dataset[which(strtoi(temp_dataset$price) < strtoi(range_price[2]) /
                               0.056), ]
        temp_dataset <-
          temp_dataset[temp_dataset$airline %in% airline, ]
      }
      if (sort_para == 'Price') {
        temp_dataset <- temp_dataset[order(temp_dataset$price),]
      } else if (sort_para == 'Time taken') {
        temp_dataset <- temp_dataset[order(temp_dataset$time_taken),]
      }
      else if (sort_para == 'Departure time') {
        temp_dataset <- temp_dataset[order(temp_dataset$dep_time),]
      }
      
      result_contain <- ''
      
      if (nrow(temp_dataset) != 0) {
        for (y in 1:nrow(temp_dataset)) {
          temp <- html_result
          temp <-
            gsub('_location',
                 paste(temp_dataset[y, ]$from, paste('-', temp_dataset[y, ]$to)),
                 temp)
          temp <-
            gsub('_price', paste('RM', formatC(round(
              strtoi(temp_dataset[y, ]$price) * 1, 2
            ), 2, format = 'f')) , temp)
          temp <-
            gsub('_period', paste0(
              as.integer(temp_dataset[y, ]$time_taken / 60),
              paste("h", paste0(
                as.integer(temp_dataset[y, ]$time_taken %% 60), 'm'
              ))
            ), temp)
          temp <-
            gsub('_time',
                 paste(
                   temp_dataset[y, ]$dep_time,
                   paste(' - ', temp_dataset[y, ]$arr_time)
                 ),
                 temp)
          temp <-
            gsub('_imglink', img_info[which(img_info$airline_name == temp_dataset[y, ]$airline), ]$image, temp)
          temp <-
            gsub('_airline_website', img_info[which(img_info$airline_name == temp_dataset[y, ]$airline), ]$web_link, temp)
          temp <- gsub('_airline', temp_dataset[y, ]$airline, temp)
          
          result_contain <- paste(result_contain, temp, sep = ' ')
        }
      }
      result_contain <-
        gsub('_html', result_contain, html_result_table)
      HTML(result_contain)
      
    })
  })
  economyDF = data.frame(table(economyData$airline))
  colnames(economyDF) <- c("Airline", "Frequency")
  businessDF = data.frame(table(businessData$airline))
  colnames(businessDF) <- c("Airline", "Frequency")
  output$text1 <- renderText({
    "Summary of Economy flight"
  })
  output$Esummary <- renderPrint({
    summary(economyData)
  })
  output$text2 <- renderText({
    "Summary of Business flight"
  })
  output$Bsummary <- renderPrint({
    print("Summary of Business flight")
    summary(businessData)
  })
  output$EDistribution <- renderPlot({
    par(mar = c(5.1, 4.1, 4.1, 2.1))
    economyBarPlot = barplot(
      height = economyDF$Frequency,
      names = economyDF$Airline,
      xlab = "Airline",
      ylab = "Frequency",
      col = rgb(0.8, 0.1, 0.1, 0.6),
      main = "Airline Distribution (Economy)"
    )
  })
  output$BDistribution <- renderPlot({
    businessBarPlot = barplot(
      height = businessDF$Frequency,
      names = businessDF$Airline,
      xlab = "Airline",
      ylab = "Frequency",
      col = "#FFF5BA",
      main = "Airline Distribution (Business)"
    )
  })
  economyData$price <- gsub(",", "", economyData$price)
  economyData$price <- round(strtoi(economyData$price) * 0.056, 0)
  economyBoxPlot = arrange(
    filter(
      economyData ,
      date == "11/02/2022",
      from == "Delhi",
      to == "Mumbai"
    ) %>% select(airline, price),
    airline
  )
  output$Economy_Box_Plot <- renderPlot({
    ggplot(economyBoxPlot,
           aes(x = airline,
               y = price,
               fill = airline)) +
      geom_boxplot() + ggtitle("Flight Price by Airline (Economy)") + xlab("airline") + ylab("price (RM)")
  })
  businessData$price <- gsub(",", "", businessData$price)
  businessData$price <- round(strtoi(businessData$price) * 0.056, 0)
  businessBoxPlot = arrange(
    filter(
      businessData ,
      date == "11/02/2022",
      from == "Delhi",
      to == "Mumbai"
    ) %>% select(airline, price),
    airline
  )
  output$Business_Box_Plot <- renderPlot({
    ggplot(businessBoxPlot,
           aes(x = airline,
               y = price,
               fill = airline)) +
      geom_boxplot() + ggtitle("Flight Price by Airline (Business)") + xlab("airline") + ylab("price (RM)")
  })
  
  output$table = DT::renderDataTable({
    economyData
  })
  
  login = FALSE
  USER <- reactiveValues(login = login)
  
  observe({
    if (USER$login == FALSE) {
      if (!is.null(input$login)) {
        if (input$login > 0) {
          result <-
            fromJSON(file = "https://raw.githubusercontent.com/jiahongggg/WIE2003-Data-Science/main/myJSON.json")
          credentials = data.frame(
            username_id = result[[1]],
            passod   = result[[2]],
            permission  = result[[3]],
            stringsAsFactors = F
          )
          
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if (length(which(credentials$username_id == Username)) == 1) {
            print(credentials$passod)
            pasmatch  <-
              credentials["passod"][which(credentials$username_id == Username), ]
            print(pasmatch)
            pasverify <- password_verify(pasmatch, Password)
            print(pasverify)
            if (pasverify) {
              USER$login <- TRUE
            } else {
              shinyjs::toggle(
                id = "nomatch",
                anim = TRUE,
                time = 1,
                animType = "fade"
              )
              shinyjs::delay(
                3000,
                shinyjs::toggle(
                  id = "nomatch",
                  anim = TRUE,
                  time = 1,
                  animType = "fade"
                )
              )
            }
          } else {
            shinyjs::toggle(
              id = "nomatch",
              anim = TRUE,
              time = 1,
              animType = "fade"
            )
            shinyjs::delay(
              3000,
              shinyjs::toggle(
                id = "nomatch",
                anim = TRUE,
                time = 1,
                animType = "fade"
              )
            )
          }
        }
        if (input$register > 0) {
          USER$login <- TRUE
          
          list_1 = vector(mode = "list", length = 3)
          result <-
            fromJSON(file = "https://raw.githubusercontent.com/jiahongggg/WIE2003-Data-Science/main/myJSON.json")
          credentials = data.frame(
            username_id = result[[1]],
            passod   = result[[2]],
            permission  = result[[3]],
            stringsAsFactors = F
          )
          # assigning different objects to the list
          list_1[[1]] = c(credentials$username_id, input$userName)
          list_1[[2]] = c(credentials$passod,
                          password_store(input$passwd))
          list_1[[3]] = c(credentials$permission, "basic")
          myfile = toJSON(list_1)
          write(
            myfile,
            "https://raw.githubusercontent.com/jiahongggg/WIE2003-Data-Science/main/myJSON.json"
          )
        }
      }
    }
  })
  
  output$logoutbtn <- renderUI({
    req(USER$login)
    tags$li(
      a(icon("fa fa-sign-out"), "Logout",
        href = "javascript:window.location.reload(true)"),
      class = "dropdown",
      style = "background-color: #eee !important; border: 0;
                    font-weight: bold; margin:5px; padding: 10px;"
    )
  })
  
  output$sidebarpanel <- renderUI({
    if (USER$login == TRUE) {
      sidebarMenu(
        menuItem("Main Page", tabName = "dashboard"),
        menuItem("Summary", tabName = "summary"),
        menuItem("Dataset", tabName = "dataset")
        
      )
    }
  })
  
  output$body <- renderUI({
    if (USER$login == TRUE) {
      tabItems(
        # First tab
        tabItem(
          tabName = "dashboard",
          class = "active",
          fluidPage(
            includeCSS(
              "https://raw.githubusercontent.com/jiahongggg/WIE2003-Data-Science/main/main.css"
            ),
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
                selectInput(
                  inputId = 'type',
                  label = "Cabin class",
                  choices = c('Economy', 'Business')
                ),
                dateInput(
                  inputId = 'des_date',
                  label = "Depart",
                  min = "2022-02-11",
                  max = "2022-03-31",
                  format = "dd/mm/yyyy"
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
              mainPanel(uiOutput("below_table"))
              
            )
          )
        ),
        
        # Second tab
        tabItem(
          tabName = "summary",
          fluidRow(
            textOutput("text1"),
            verbatimTextOutput("Esummary"),
            textOutput("text2"),
            verbatimTextOutput("Bsummary"),
            plotOutput("EDistribution"),
            plotOutput("BDistribution"),
            plotOutput("Economy_Box_Plot"),
            plotOutput("Business_Box_Plot")
          )
        ),
        tabItem(tabName = "dataset",
                fluidRow(dataTableOutput("table")))
      )
    }
    else {
      loginpage
    }
  })
  
  output$results <-  DT::renderDataTable({
    datatable(iris, options = list(autoWidth = TRUE,
                                   searching = FALSE))
  })
  
  output$results2 <-  DT::renderDataTable({
    datatable(mtcars, options = list(autoWidth = TRUE,
                                     searching = FALSE))
  })
  
  
}

runApp(list(ui = ui, server = server), launch.browser = TRUE)
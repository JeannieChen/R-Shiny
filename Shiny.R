### Twitter Sentiment Analysis for Stock Market Prediction ###

# Load packages
library(readxl)
library(readr)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(wordcloud2)
library(tm)
library(ngram)
library(tidytext)
library(plotly)
library(plyr)
library(devtools)
library(stringr)
library(stringi)
library(sentimentr)
library(tidyr)
library(reshape2)
library(DT)
library(RColorBrewer)
library(markdown)

#### GLOBAL ####
# Define common words that will be removed from wordclouds
commonWords = c("and","i","the","a","of","for","to","in","by","they","is","it","has","not",
                "this","than","on","that","just","from","was","with","when","be","are","at",
                "had","or","you","am","0","1","copied","aen97","then","have","but","do","my",
                "me","as","an","car", "https", "rt", "t")

# Get sentiment words/scores from NRC lexicon
# sentiment_nrc = get_sentiments("nrc")
sentiment_nrc = read_csv("data/sentiment_nrc.csv")

# Change column name to match on table later
colnames(sentiment_nrc)[1] = "."

df <- read_csv("data/tweet_score_per_row_v2_filtered.csv",
               col_types = cols(date = col_date(format = "%Y-%m-%d"),
                                date_influence = col_date(format = "%Y-%m-%d"),
                                time = col_time(format = "%H:%M:%S"),
                                Ticker = col_factor(),
                                sentiment = col_factor(),
                                quote_count = col_integer(),
                                reply_count = col_integer(),
                                retweet_count = col_integer(),
                                favorite_count = col_integer(),
                                followers_count = col_integer()
               )) %>%
  as.data.frame()

# Read word sentiment data
allSentiment3 <- read_csv("data/allSentiment3_v2.csv")
allSentiment3 = allSentiment3[,-1]

# Read stock price data; format date
stock <- read_excel("data/Stock_Price_v2.xlsx")
stock$Date = as.Date(stock$Date, format="%Y-%m-%d")

# Read pre-runned model output; format date
model <- read_excel("data/model_summary_v2.xlsx")
model$Date = as.Date(model$Date, format="%Y-%m-%d")

# cat(pryr::mem_used())

#### UI ####

# Define dashboard header
header <- dashboardHeader(title="Stock Price Prediction using Tweets Sentiment")

# Define sidebars
sidebar <- dashboardSidebar(
  width = 240,
  sidebarMenu(
    # Tab 1: EDA
    menuItem("Exploratory Data Analysis", icon = icon("search"),
             # Subtab 1: tweets analysis
             menuSubItem("Tweets", tabName = "search_by_tweets"),
             # Subtab 2: stock price analysis
             menuSubItem("Stock", tabName = "search_by_stock")
    ),
    
    # Tab 2: Prediction
    menuItem("Prediction", icon = icon("magic"),tabName = "prediction"),
    # Tab 3: About
    menuItem("About", icon = icon("copyright"),tabName = "about")
  ))

# Define dashboard body
body <- dashboardBody(
  tabItems(
    # For tab 1
    tabItem(tabName = "search_by_tweets",
            fluidRow(
              # Filter boxes
              box(width = 12, collapsible = TRUE, status = "primary", solidHeader = FALSE,
                  fluidRow(
                    # Date range picker
                    column(12,
                           # Help text for incorrect filter input
                           helpText("Please make sure the left date input is prior to the right.", br(),
                                    "If loading takes too long or app crashes, please reload the app and try again."),
                           hr(),
                           dateRangeInput('date', 'Date Range',
                                          start = "2020-01-01", end = "2020-01-31",
                                          min = min(df$date), max = max(df$date),
                                          separator = " - ", format = "yyyy-mm-dd"
                           )),
                    # Ticker picker
                    column(6, 
                           pickerInput("Ticker","Ticker", 
                                       selected = c('AAL'),
                                       choices=unique(as.character(df$Ticker)), 
                                       options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                       multiple = T)),
                    # Sentiment category picker
                    column(6, 
                           pickerInput("sentiment","Sentiment", 
                                       selected = 'positive',
                                       choices=unique(as.character(df$sentiment)), 
                                       options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                       multiple = T)),
                    # DT Table: Show filtered tweets
                    box(width = 12, collapsible = TRUE, status = "success", title = "Tweets", solidHeader = FALSE,
                        DT::dataTableOutput("tweets_search")),
                    # Wordcloud
                    box(width = 4, collapsible = TRUE, status = "success", title = "Word Cloud based on Word's Frequency", solidHeader = FALSE,
                        plotOutput("wordcloud" ,width = "500px", height="500px"
                        )),
                    # Sentiment analysis - overall feeling
                    box(width = 8, collapsible = TRUE, status = "success", title = "Opinion Mining - Overall Feeling", solidHeader = FALSE,
                        plotlyOutput("main_sentiment", width = "700px", height="500px"))
                    
                  )
              )
            )),
    # For tab 2
    tabItem(tabName = "search_by_stock",
            fluidRow(
              # Filter boxes
              box(width = 12, collapsible = FALSE, status = "primary", solidHeader = FALSE,
                  fluidRow(
                    # Date range picker
                    column(12,
                           # Help text for incorrect filter input
                           helpText("Please make sure the left date input is prior to the right.", br(),
                                    "If loading takes too long or app crashes, please reload the app and try again."),
                           hr(),
                           dateRangeInput('date_stock', 'Date Range',
                                          start = "2020-01-02", end = "2020-04-10",
                                          min = min(stock$Date), max = max(stock$Date),
                                          separator = " - ", format = "yyyy-mm-dd"
                           )),
                    # Ticket picker
                    column(6, 
                           pickerInput("Ticker_stock","Ticker", 
                                       selected = 'TSLA',
                                       choices=unique(as.character(df$Ticker)), 
                                       options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                       multiple = F)),
                    # Price type picker
                    column(6, 
                           checkboxGroupInput("history_stock","Show History", 
                                              selected = c("Close", "Open"),
                                              choices = list("Close", "High", "Low", "Open"),
                                              inline = TRUE
                           )),
                    # Show price line chart and candlestick plot
                    box(width = 12, collapsible = TRUE, status = "success", solidHeader = FALSE,
                        plotlyOutput("stock_price"), br(),
                        plotlyOutput("stock_candlestick"))
                  ))
            )), # End of stock tab
    # For tab 3
    tabItem(tabName = "prediction",
            fluidRow(
              # Filter boxes
              box(width = 12, collapsible = FALSE, status = "primary", solidHeader = FALSE,
                  fluidRow(
                    # Date picker
                    column(6, dateInput('Date_pred', 'Pick A Date:',
                                        value = '2020-01-30', 
                                        min = min(model$Date), max = max(model$Date),
                                        format = "yyyy-mm-dd")
                    ),
                    # Ticket picker
                    column(6, 
                           pickerInput("Ticker_pred","Ticker", 
                                       selected = 'JPM',
                                       choices=unique(as.character(model$Ticker)), 
                                       options = list(`actions-box` = TRUE, `live-search` = TRUE),
                                       multiple = F)),
                    # Value box: show model output
                    valueBoxOutput("rf_output"),
                    
                    # Value box: tweets sentiment
                    valueBoxOutput("sentiment_pred_date"),
                    
                    # Value box: best ticker of the day
                    valueBoxOutput("best_ticker_pred_date"),
                    
                    # Actual price in the following week
                    column(12, plotlyOutput("stock_candlestick_next7")),
                    
                    # Tweets
                    column(12, DT::dataTableOutput("tweets_pred_date")),
                    
                    # Wordcloud
                    column(5, plotOutput("wordcloud_pred_date" ,width = "450px", height="500px")),
                    
                    # Overal Feeling
                    column(7, plotlyOutput("main_sentiment_pred_date", width = "700px", height="500px"))
                    
                  ) # End of fluid row
              )# End of box
            )), # End of prediction tab
    # For tab 4
    tabItem(tabName = "about",
            fluidRow(
              column(12, 
                     helpText("This app is built for CSE 6242: Data and Visual Analytics (2020 Fall)
                              offered by Georgia Tech.", br(),
                              
                              br(),
                              "Copyright (c) belongs to Team 61 - Cuckoo.", br(),
                              
                              br(),
                              "Team Members & Contact Info: ", br(),
                              HTML("<b> Jeanie Chen </b> ychen3387@gatech.edu <br> 
                                   ") # End of HTML
                              ) # End of helptext
                              )
                              )
                              ) # End of about tab
                              ) # End of tabitems
                              ) # End of body


# Define dashboard theme
# shinyUI <- dashboardPage(header,sidebar, body, skin="black")
ui <- dashboardPage(header,sidebar, body, skin="black")

#### SERVER ####
server <- function(input, output) { 
    #### 1) Filter tweets ####
    tweets_search <- reactive({
      df %>% dplyr::filter(Ticker %in% input$Ticker,
                           date >= input$date[1] & date <= input$date[2],
                           sentiment %in% input$sentiment) })
    
    # Render filtered tweets
    output$tweets_search = DT::renderDataTable({ 
      datatable(tweets_search()[order(tweets_search()$sentiment_score),][,c('created_at','screen_name','Ticker','text','sentiment_score','sentiment')],
                colnames = c("Date", "User", "Ticker", "Tweets", "Sentiment Score", "Sentiment"),
                options = list(pageLength = 5))
    })
    
    # Wordcloud #
    # Clean text: remove puncturation, digits, commonwords, etc.
    wordCloud1.1 = reactive({
      tweets_search()$text %>%
        na.omit() %>%
        tolower() %>%
        gsub("[[:punct:]]","",.) %>%
        gsub('[[:digit:]]+', '',.) %>% 
        removeWords(.,commonWords) %>%
        strsplit(split = " ") %>% 
        unlist() %>%
        table() %>%
        sort(decreasing = TRUE)
    })
    # Remove NAs. Coerce to data frame
    wordCloud1.2 = reactive({ as.data.frame(na.omit(wordCloud1.1()) ) })
    wordCloud1.3 = reactive({ wordCloud1.2() %>% dplyr::filter(wordCloud1.2()$. !="")  })
    # Sort dataset by words' frequency
    wordCloud1.4 = reactive({ wordCloud1.3()[order(wordCloud1.3()$Freq),] })
    # Display wordcloud
    output$wordcloud = renderPlot({ 
      wordcloud(words = wordCloud1.4()$.,
                freq = wordCloud1.4()$Freq, 
                scale = c(2,0.5),
                min.freq = 1, 
                max.words=200, 
                random.order=FALSE, 
                rot.per=0, 
                colors=brewer.pal(8, "Dark2") )
    })
    
    # Bar chart - overall feeling #
    # Associate word with sentiment
    sentiment1.5 <- reactive({
      wordCloud1.4() %>% inner_join(sentiment_nrc)})
    
    output$main_sentiment = renderPlotly({
      sentiment1.6 =  ggplot(sentiment1.5(), aes(x=sentiment)) +
        geom_bar(aes(fill= sentiment)) + 
        scale_fill_brewer(palette= "Spectral") + labs(y = NULL, x = NULL) +
        theme_minimal()
      ggplotly(sentiment1.6) %>%
        layout(margin = list(b = 50, l = 10) )
    })
    
    # 2) Filter stocks ####
    stock_search <- reactive({
      stock %>% dplyr::filter(Ticker %in% input$Ticker_stock, 
                              Date >= input$date_stock[1] & Date <= input$date_stock[2]) 
    })
    
    stock_agg = reactive({
      stock = stock_search()[, which(names(stock_search()) %in% c("Date", "Ticker", "Volume", input$history_stock ))]
      melt(stock, id.vars = c("Date", "Ticker", "Volume")) #Date, Ticker, Volume, variable, value
    })
    
    # Stock price line chart
    output$stock_price = renderPlotly({
      stock_price = plot_ly(stock_agg(),x=~Date, y=~value, color=~variable)
      stock_price %>% add_lines() %>% layout(
        title = paste0(
          "Stock Price History for ", input$Ticker_stock,
          " (", input$date_stock[1]," to ", input$date_stock[2],") "),
        xaxis = list(title = ''),
        yaxis = list(title = ''))
    })
    # Candilestick chart
    output$stock_candlestick = renderPlotly({
      stock_candile = stock_search() %>% plot_ly(
        x = ~Date, type = "candlestick",
        open = ~ Open, close = ~ Close, high = ~ High, low = ~ Low)
      stock_candile %>% 
        layout(title = paste0(
          "Candlestick Chart for ", input$Ticker_stock,
          " (", input$date_stock[1], " to ", input$date_stock[2],") "))
    })
    
    # 3) Filter pred values ####
    model_pred_search <- reactive({
      model %>% dplyr::filter(Date == input$Date_pred, 
                              Ticker %in% input$Ticker_pred) 
    })
    
    model_best_pred_search <- reactive({
      model %>% dplyr::filter(Date == input$Date_pred)
    })
    
    tweets_pred_search <- reactive({
      df %>% dplyr::filter(date_influence == input$Date_pred,
                           Ticker %in% input$Ticker_pred)
    })
    stock_pred_search <- reactive({
      stock %>% dplyr::filter(Date >= input$Date_pred & Date < input$Date_pred + 14,
                              Ticker %in% input$Ticker_pred) 
    })
    
    # Model output
    output$rf_output <- renderValueBox({
      valueBox(
        ifelse(model_pred_search()$svm_prob>=0.5, "Buy", "Sell"),
        HTML(paste("Buy in probability of ",input$Ticker_pred,": ",
                   round(model_pred_search()$svm_prob*100,1), " % <br>
                   predicted by SVM classifier")),
        icon = icon("history"),
        color = "yellow", width=12 )
    })
    
    # Sentiment score
    output$sentiment_pred_date <- renderValueBox({
      temp = aggregate(tweets_pred_search()$sentiment_score, by=list(tweets_pred_search()$date_influence, tweets_pred_search()$Ticker), sum)
      colnames(temp) = c("Date", "Ticker", "Total_sentiment_score")
      
      valueBox(round(temp$Total_sentiment_score,2),
               HTML(paste(input$Ticker_pred," sentiment score from tweets"), "<br> 
                    (Larger score means more positive)"),
               icon = icon("twitter"),
               color = "aqua", width=12)
    })
    
    # Best ticker of the day
    output$best_ticker_pred_date <- renderValueBox({
      temp = model_best_pred_search()[order(-model_best_pred_search()$svm_prob), ][1,]
      
      valueBox(temp$Ticker,
               HTML(paste("Ticker that's most likely to win of the day<br>
                          Buy in probability: ", round(temp$svm_prob*100,1), " %") ),
               icon = icon("angellist"),
               color = "teal", width=12)
      
    })
    
    
    # Candilestick chart for the next 7 days (actual)
    output$stock_candlestick_next7 = renderPlotly({
      stock_candile = stock_pred_search() %>% plot_ly(
        x = ~Date, type = "candlestick",
        open = ~ Open, close = ~ Close, high = ~ High, low = ~ Low)
      stock_candile %>% 
        layout(title = paste0(
          "Actual Price of ", input$Ticker_pred, " in the Following 2 Weeks: ",
          " (", input$Date_pred, " ~ ", input$Date_pred + 14,") "))
    })
    
    # Render filtered tweets for that day
    output$tweets_pred_date = DT::renderDataTable({ 
      datatable(tweets_pred_search()[order(tweets_pred_search()$sentiment_score),][,c('created_at','screen_name','Ticker','text','sentiment_score','sentiment')],
                colnames = c("Date", "User", "Ticker", "Tweets", "Sentiment Score", "Sentiment"),
                options = list(pageLength = 5))
    })
    
    # Wordcloud #
    wordCloud3.1 = reactive({
      tweets_pred_search()$text %>%
        na.omit() %>%
        tolower() %>%
        gsub("[[:punct:]]","",.) %>%
        gsub('[[:digit:]]+', '',.) %>% 
        removeWords(.,commonWords) %>%
        strsplit(split = " ") %>% 
        unlist() %>%
        table() %>%
        sort(decreasing = TRUE)
    })
    # Remove NAs. Coerce to data frame
    wordCloud3.2 = reactive({ as.data.frame(na.omit(wordCloud3.1()) ) })
    wordCloud3.3 = reactive({ wordCloud3.2() %>% dplyr::filter(wordCloud3.2()$. !="")  })
    # Sort dataset by words' frequency
    wordCloud3.4 = reactive({ wordCloud3.3()[order(wordCloud3.3()$Freq),] })
    # Display wordcloud
    output$wordcloud_pred_date = renderPlot({ 
      wordcloud(words = wordCloud3.4()$.,
                freq = wordCloud3.4()$Freq, 
                scale = c(2,0.5),
                min.freq = 1, 
                max.words=200, 
                random.order=FALSE, 
                rot.per=0, 
                colors=brewer.pal(8, "Dark2") )
    })
    
    # Bar chart - overall feeling #
    sentiment3.5 <- reactive({
      wordCloud3.4() %>% inner_join(sentiment_nrc)})
    
    output$main_sentiment_pred_date = renderPlotly({
      sentiment3.6 =  ggplot(sentiment3.5(), aes(x=sentiment)) +
        geom_bar(aes(fill= sentiment)) + 
        scale_fill_brewer(palette= "Spectral") + labs(y = NULL, x = NULL) +
        theme_minimal()
      ggplotly(sentiment3.6) %>%
        layout(margin = list(b = 50, l = 10) )
    })
    
  }

#Create Shiny Object
shinyApp(ui,server)



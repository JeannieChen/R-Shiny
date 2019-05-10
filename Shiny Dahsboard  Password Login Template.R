require(shiny)
require(shinydashboard)

header <- dashboardHeader(title = "my heading")
sidebar <- dashboardSidebar(uiOutput("sidebarpanel"))
body <- dashboardBody(uiOutput("body"))
ui <- dashboardPage(header, sidebar, body)

login_details <- data.frame(user = c("sam", "pam", "ron"),
                            pswd = c("123", "1234", "12345")) #require different pswd. If duplicate only the 1st username/pswd will be used
login <- box(
  title = "Login",
  textInput("userName", "Username"),
  passwordInput("passwd", "Password"),
  br(),
  actionButton("Login", "Log in")
)

server <- function(input, output, session) {
  # Record user loging time 
  users_data <- data.frame(START = Sys.time())
  # To logout back to login page
  login.page = paste(
    isolate(session$clientData$url_protocol),
    "//",
    isolate(session$clientData$url_hostname),
    ":",
    isolate(session$clientData$url_port),
    sep = ""
  )
  USER <- reactiveValues(Logged = F)
  observe({
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          Id.username <- which(login_details$user %in% Username)
          Id.password <- which(login_details$pswd %in% Password)
          if (length(Id.username) > 0 & length(Id.password) > 0){
            if (Id.username == Id.password) {
              USER$Logged <- TRUE } }
        } } }
  })
  output$sidebarpanel <- renderUI({
    if (USER$Logged == TRUE) {
      div(
        sidebarUserPanel(
          isolate(input$userName), #display username
          subtitle = a(icon("usr"), "Logout", href = login.page)
        ),

        sidebarMenu( #Test content--sidebarmenu
          menuItem("Item 1",
            tabName = "t_item1",
            icon = icon("line-chart")
          ),
          menuItem("Item 2",
                   tabName = "t_item2",
                   icon = icon("dollar"))
        )
      )}
  })
  
  output$body <- renderUI({
    if (USER$Logged == TRUE) {
      tabItems(
        # Tab content
          tabItem(tabName = "t_item1"),
          tabItem(tabName = 't_item2')
      )
    } else {
      login
    }
  })
  
  ## Other content..i.e filters, reactive, tableOutput
  
  #log file
  session$onSessionEnded(function() {
    users_data$END = Sys.time()
    users_data = cbind(users_data,isolate(input$userName))
    write.table(x = users_data, file = file.path(getwd(), "users_data.txt"),append = TRUE, row.names = FALSE, col.names = FALSE, sep = "\t")
  })

}
shinyApp(ui, server)
###############################################################################
### Shiny App for AWS EC2 Instance Submission
### Kyle Ueyama
### Borrows heavily from work by Dean Attali
### http://deanattali.com/2015/06/14/mimicking-google-form-shiny/
###############################################################################

# load packages
library(shiny)
library(shinyjs)
library(aws.s3)
library(dplyr)

# set aws credentials
tryCatch({
  aws.signature::use_credentials()
}, error = function(e) {
  Sys.setenv("AWS_ACCESS_KEY_ID" = aws.signature::locate_credentials()$key,
             "AWS_SECRET_ACCESS_KEY" = aws.signature::locate_credentials()$secret,
             "AWS_DEFAULT_REGION" = "us-east-1",
             "AWS_SESSION_TOKEN" = aws.signature::locate_credentials()$session_token)
})

# set current s3_bucket
s3_bucket <- "ui-elastic-analytics"

# helper function to validate email
validateEmail <- function(email) {
  return(grepl("@urban.org$", email))
}

# helper function to timestamp submission
getFormattedTimestamp <- function() {
  format(Sys.time(), "%Y-%m-%d-%H-%M-%OS")
}

# names of the fields to save
names <- c("email",
           "project",
           "os",
           "instanceType",
           "storage",
           "spotInstance")

# set table of instance details
instance_df <- tribble(
  ~Instance, ~vCPU, ~`Memory (GB)`, ~`Cost per Hour (Linux)`, ~`Cost per Hour (Windows)`,
  "c5.xlarge", 4, 8, 0.17, 0.36,
  "c5.2xlarge",8, 16, 0.34, 0.71,
  "c5.4xlarge",16, 32, 0.68, 1.42,
  "c5.9xlarge",36, 72, 1.53, 3.19,
  "c5.18xlarge", 72, 144, 3.06, 6.38,
  "x1.16xlarge", 64, 976, 6.67, 9.61,
  "x1.32xlarge", 128, 1952, 13.34, 19.23
)

instance_table <- tribble(
  ~Instance, ~vCPU, ~`Memory (GB)`, ~`Cost per Hour (Linux)`, ~`Cost per Hour (Windows)`,
  "c5.xlarge", 4, 8, "$0.17", "$0.36",
  "c5.2xlarge",8, 16, "$0.34", "$0.71",
  "c5.4xlarge",16, 32, "$0.68", "$1.42",
  "c5.9xlarge",36, 72, "$1.53", "$3.19",
  "c5.18xlarge", 72, 144, "$3.06", "$6.38",
  "x1.16xlarge", 64, 976, "$6.67", "$9.61",
  "x1.32xlarge", 128, 1952, "$13.34", "$19.23"
)

################################################################################
### define the ui
################################################################################
ui <- fluidPage(

  # use Shinyjs package
  useShinyjs(),

  # set message handler
  tags$head(tags$script(src = "message-handler.js")),

  # application title
  h1("Urban Institute Cloud Computing Submission Form"),

  # information for user
  fluidRow(
    column(12,
           br(),
           h2("Not Suitable for Confidential Data"),
           br(),
           h3("The Following Operating Systems are Available for Use:"),
           tags$ul(
             tags$li(h4("Linux (includes R and Python)")),
             tags$li(h4("Windows (includes R, Python, and Stata)"))
           ),
           br(),
           h3("The Following Cloud Computing Instances are Available for Use:")
    )
  ),

  # info on instances available
  tableOutput("instance_table"),
  h4("Note that storage will add an additional cost"),
  br(),


  # make instance recommendation
  h3("Which Instance Type do I Need?"),
  h4("Computing needs vary due to a variety of factors,
      including data size and computational complexity"),
  h4("Enter the size of your raw, uncompressed data for a general recommendation"),
  numericInput(inputId = "data_size",
               label = "Size of Uncompressed Data (GB)",
               value = 1,
               min = 0,
               step = 1),
  uiOutput("instance_rec"),

  # form fields for instance spinup request
  hr(),
  hr(),
  h3("Please Fill Out All Fields to Request an Instance"),

  textInput(inputId = "email",
            label = "Urban Email Address"),

  uiOutput(outputId = "emailError"),

  selectInput(inputId = "project",
              label = "Project Code",
              choices = c("DI"),
              selected = "DI"),

  selectInput(inputId = "os",
              label = "Operating System",
              choices = c("",
                          "Linux",
                          "Windows"),
              selected = ""),

  selectInput(inputId = "instanceType",
              label = "Instance Type",
              choices = c("",
                          "c5.xlarge",
                          "c5.2xlarge",
                          "c5.4xlarge",
                          "c5.9xlarge",
                          "c5.18xlarge",
                          "x1.16xlarge",
                          "x1.32xlarge"),
              selected = ""),

  sliderInput(inputId = "storage",
              label = "Add Storage (GB)",
              min = 100,
              max = 1000,
              value = 500,
              step = 50),

  uiOutput('storage_info'),

  fluidRow(
    column(3,
           checkboxInput(inputId = "spotInstance",
                         label = "Request a spot instance?",
                         value = FALSE)
           ),
    column(8,
           p("Note: Spot instances will generally cost 50% less than a standard instance,
             but may be terminated without warning due to surge pricing. Only
             available on Linux Instances.")
    )
  ),

  uiOutput('cost_info'),

  actionButton(inputId = "submit",
               label = "Submit"),

  # form fields to terminate instance
  fluidRow(
    column(12,
           hr(),
           hr(),
           h3("Done With Your Instance?"),
           h4("Complete the Following to Terminate"),
           h4("Warning! Termination is Permanent")
    )
  ),

  textInput(inputId = "termination_email",
            label = "Urban Email Address"),

  uiOutput("terminationEmailError"),

  textInput(inputId = "instance_id",
            label = "Instance ID"),

  actionButton(inputId = "terminate",
               label = "Terminate My Instance")

)

################################################################################
### define the server
################################################################################
server <- function(input, output, session) {

  # submission form must be completely filled in order to submit
  observe({
    if (input$email == "" ||
        !validateEmail(input$email) ||
        input$project == "" ||
        input$os == "" ||
        input$instanceType == "") {
      disable("submit")
    }
    else {
      enable("submit")
    }
  })

  # termination form must be completely filled in order to submit
  observe({
    if (input$instance_id == "" ||
        input$termination_email == "" ||
        !validateEmail(input$termination_email)) {
      disable("terminate")
    }
    else {
      enable("terminate")
    }
  })

  observe({
    if (input$os == 'Windows') {
      disable('spotInstance')
    } else {
      enable('spotInstance')
    }
  })

  # require an "@urban.org" email address
  output$emailError <- renderUI({
    validate(
      need(validateEmail(input$email),
           "Please enter your urban.org email address")
    )
  })

  output$terminationEmailError <- renderUI({
    validate(
      need(validateEmail(input$termination_email),
           "Please enter the urban.org email address associated with the instance")
    )
  })

  # display instance type information
  output$instance_table <- renderTable(instance_table,
                                       align = 'c',
                                       digits = 0)

  # make instance recommendation
  output$instance_rec <- renderUI({
    expanded <- input$data_size * 4
    if(expanded < 8) {
      h4("Recommended Instance: c5.xlarge")
    } else if(expanded < 16) {
      h4("Recommended Instance: c5.2xlarge")
    } else if(expanded < 32) {
      h4("Recommended Instance: c5.4xlarge")
    } else if(expanded < 72) {
      h4("Recommended Instance: c5.9xlarge")
    } else if(expanded < 144) {
      h4("Recommended Instance: c5.18xlarge")
    } else if(expanded < 976) {
      h4("Recommended Instance: x1.16xlarge")
    } else if(expanded < 1952) {
      h4("Recommended Instance: x1.32xlarge")
    } else {
      h4("Contact research programming for further assistance.")
    }
  }
  )

  # display storage information
  output$storage_info <- renderUI({
    if(input$os == 'Windows') {
      helpText("Storage for Windows instances is locked.")
    }

  })

  # display cost information
  output$cost_info <- renderUI({
    if(input$os == "Linux" & input$instanceType != "" & input$spotInstance != T) {
      storage_cost <- 0.10 * input$storage / (24*30)
      instance_cost <- instance_df %>%
        filter(Instance == input$instanceType) %>%
        select(`Cost per Hour (Linux)`) %>%
        pull()
      cost <- round(storage_cost + instance_cost, 2)
      h4(paste0("Your instance will cost $", cost, " per hour to run. ",
                "(NOTE: usage is currently free during this trial period)"))
    } else if(input$os == "Windows" & input$instanceType != "" & input$spotInstance != T) {
      storage_cost <- 0.10 * 501 / (24*30)
      instance_cost <- instance_df %>%
        filter(Instance == input$instanceType) %>%
        select(`Cost per Hour (Windows)`) %>%
        pull()
      cost <- round(storage_cost + instance_cost, 2)
      h4(paste0("Your instance will cost $", cost, " per hour to run. ",
                "(NOTE: usage is currently free during this trial period)"))
    } else if(input$os == "Linux" & input$instanceType != "" & input$spotInstance == T) {
      storage_cost <- 0.10 * input$storage / (24*30)
      instance_cost <- instance_df %>%
        filter(Instance == input$instanceType) %>%
        select(`Cost per Hour (Linux)`) %>%
        pull() * 0.40
      cost <- round(storage_cost + instance_cost, 2)
      h4(paste0("Your instance will cost approximately $", cost, " per hour to run. ",
                "(NOTE: usage is currently free during this trial period)"))
    } else if(input$os == "Windows" & input$instanceType != "" & input$spotInstance == T) {
      storage_cost <- 0.10 * 501 / (24*30)
      instance_cost <- instance_df %>%
        filter(Instance == input$instanceType) %>%
        select(`Cost per Hour (Windows)`) %>%
        pull() * 0.60
      cost <- round(storage_cost + instance_cost, 2)
      h4(paste0("Your instance will cost approximately $", cost, " per hour to run. ",
                "(NOTE: usage is currently free during this trial period)"))
    } else {
    }
  })

  # but don't display storage option for windows instances
  observe({
    toggleState(id = 'storage', condition = input$os != 'Windows')
  })


  # spin up an instance
  observeEvent(input$submit, {

    # lock form
    disable("submit")
    disable("terminate")

    # get input data for instance
    data <- sapply(names, function(x) input[[x]])
    data <- as.data.frame(t(data))
    data$timestamp <- getFormattedTimestamp()
    data$project <- gsub(" ", "-", data$project)

    # set filepaths for s3 submission
    submission_file <- sprintf("submissions/%s-%s.json",
                               gsub('@urban.org', '',input$email),
                               data$timestamp)

    # write instance information to s3 to trigger lambda
    s3write_using(data,
                  FUN = jsonlite::write_json,
                  object = submission_file,
                  bucket = s3_bucket,
                  check_region = FALSE)

    # confirmation to
    if (input$os == 'Windows') {
      minutes <- 1
    } else {
      minutes <- 5
    }

    showModal(modalDialog(
      title = "Instance Status",
      paste("Thank you for your submission. Your cloud computing details will
      be emailed to you when your instance is ready, which should take
      about", minutes, "-" (minutes*2), "minutes. You may close this window."),
      easyClose = FALSE,
      footer = modalButton("Dismiss")))


    })

  # terminate an instance
  observeEvent(input$terminate, {

    # lock form
    disable("stop")
    disable("terminate")

    # write details to s3 to trigger lambda
    instance_id <- gsub(" ", "", input$instance_id)
    termination_time <- getFormattedTimestamp()
    termination_data <- data.frame(instance_id = instance_id,
                                   email = input$termination_email,
                                   timestamp = termination_time)

    termination_file <- sprintf("terminations/%s-%s.json",
                                gsub('@urban.org', '',input$termination_email),
                                termination_time)

    s3write_using(termination_data,
                  FUN = jsonlite::write_json,
                  object = termination_file,
                  bucket = s3_bucket)

    # confirmation to user
    showModal(modalDialog(
      title = "Termination Confirmation",
      paste0("Thank you for your request. You will receive a confirmation email
             shortly. You may close this window."),
      easyClose = FALSE,
      footer = modalButton("Dismiss")))
  })

  }

################################################################################
### run app
################################################################################
shinyApp(ui = ui, server = server)

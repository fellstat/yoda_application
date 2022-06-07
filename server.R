if(!file.exists("yoda_code")){
  setwd("..")
}

library(aws.s3)
try(source("secret.R"))
write_bucket <- Sys.getenv("TEST_BUCKET_WRITE")


library(shiny)
library(futile.logger)
source("globals.R")
source("s3-connect.R")

USG_USERS = c("Agency", "Interagency", "Global Agency", "Global")
PARTNER_USERS = c("Global Partner", "Partner")



refresh_countries <- function(){
  any_added <- FALSE
  country_names <- list.dirs("application/countries", recursive = FALSE, full.names = FALSE)
  buckets <- aws.s3::get_bucket(prefix = "system_yoda/",bucket = Sys.getenv("TEST_BUCKET_WRITE"))
  files <- sapply(buckets, function(x) x$Key)
  fnames <- sapply(str_split(files,"/"), function(x) x[[2]])
  cnames <- sapply(str_split(fnames,"\\."), function(x) x[[1]])
  dir.create("tmp__")
  setwd("application/countries")
  for(i in 1:length(cnames)){
    if(!(cnames[i] %in% country_names)){
      save_object(
        bucket = write_bucket,
        object = paste0("system_yoda/",cnames[i],".zip"), 
        file = "../../tmp__/country_file.zip"
      )
      unzip("../../tmp__/country_file.zip")
      any_added <- TRUE
    }
  }
  for(i in seq_along(country_names)){
    if(!(country_names[i] %in% cnames)){
      unlink(country_names[i])
    }
  }
  setwd("../..")
  unlink("tmp__", recursive = TRUE)
  any_added
}

if(!dir.exists("runs"))
  dir.create("runs")
if(!dir.exists("application"))
  dir.create("application")
if(!dir.exists("application/countries"))
  dir.create("application/countries")


shinyServer(function(input, output, session) {
  
  refreshTrigger <- reactiveVal(1)
  
  # user information
  user_input  <-  reactiveValues(authenticated = FALSE,
                                 status = "",
                                 d2_session = NULL,
                                 memo_authorized = FALSE)
  
  auth_ui <- function(){
    wellPanel(
      fluidRow(
        h4("Please login with your DATIM credentials:"),
        br()
      ),
      fluidRow(
        textInput("user_name", "Username: ", width = "500px"),
        passwordInput("password", "Password:", width = "500px"),
        actionButton("login_button", "Log in!")
      )
    )
  }
  
  main_ui <- function(){
    any_added <- refresh_countries()
    countries <<- "application/countries" %>% 
      list.dirs(full.names = FALSE, recursive = FALSE) %>%
      str_replace_all("_"," ")
    fluidPage(
      
      # Application title
      titlePanel("YODA HIV Testing Program Optimizer"),
      sidebarLayout(
        sidebarPanel(
          selectInput("country","Country",countries),
          numericInput("total_tests_target", "Target Total HIV Tests Per Quarter",NA, 0),
          numericInput("max_diff", "Maximum Allowable % Change in # of Tests",20,0),
          numericInput("index_ratio", "% Change in Index Tests Per Non-Index Test",25, 0),
          textInput("run_name","Name of Run","My Run"),
          actionButton("run","Run")#,
          #p(any_added)
        ),
        mainPanel(
          selectInput("outrun", "Run", runs),
          actionButton("refresh","Refresh"),
          h3(),
          textOutput("status"),
          h3(),
          downloadLink("results_download","Download Results"),
          h3(),
          verbatimTextOutput("run_log")
        )
      )
    )
  }
  
  output$ui <- renderUI({
    if(!user_input$authenticated){
      auth_ui()#main_ui()#
    }else{
      main_ui()      
    }
  })
  
  # User and mechanisms reactive value pulled only once ----
  user <- reactiveValues(type = NULL)
  mechanisms <- reactiveValues(my_cat_ops = NULL)
  userGroups <- reactiveValues(streams = NULL)
  
  # Login process ----
  observeEvent(input$login_button, {
    tryCatch({
      datimutils::loginToDATIM(base_url = Sys.getenv("BASE_URL"),
                               username = input$user_name,
                               password = input$password,
                               d2_session_envir = parent.env(environment())
      )
      
      # DISALLOW USER ACCESS TO THE APP-----
      
      # store data so call is made only once
      userGroups$streams <-  datimutils::getMyStreams()
      user$type <- datimutils::getMyUserType()
      mechanisms$my_cat_ops <- datimutils::listMechs()
      
      # if a user is not to be allowed deny them entry
      #if (!user$type %in% c(USG_USERS, PARTNER_USERS)) {
      if (!user$type %in% c(USG_USERS)) {  # only allow USG
        # alert the user they cannot access the app
        sendSweetAlert(
          session,
          title = "YOU CANNOT LOG IN",
          text = "You are not authorized to use this application",
          type = "error"
        )
        
        # log them out
        Sys.sleep(3)
        flog.info(paste0("User ",user$type," logged out."))
        user_input$authenticated  <-  FALSE
        user_input$user_name <- ""
        user_input$authorized  <-  FALSE
        user_input$d2_session  <-  NULL
        d2_default_session <- NULL
        gc()
        session$reload()
        
      }
    },
    # This function throws an error if the login is not successful
    error = function(e) {
      flog.info(paste0("User ", input$user_name, " login failed."), name = "datapack")
    }
    )
    
    if (exists("d2_default_session")) {
      if (any(class(d2_default_session) == "d2Session")) {
        user_input$authenticated  <-  TRUE
        user_input$d2_session  <-  d2_default_session$clone()
        d2_default_session <- NULL
        
        
        # Need to check the user is a member of the PRIME Data Systems Group, COP Memo group, or a super user
        user_input$memo_authorized  <-
          grepl("VDEqY8YeCEk|ezh8nmc4JbX", user_input$d2_session$me$userGroups) |
          grepl(
            "jtzbVV4ZmdP",
            user_input$d2_session$me$userCredentials$userRoles
          )
        flog.info(
          paste0(
            "User ",
            user_input$d2_session$me$userCredentials$username,
            " logged in."
          ),
          name = "datapack"
        )
      }
    } else {
      sendSweetAlert(
        session,
        title = "Login failed",
        text = "Please check your username/password!",
        type = "error"
      )
    }
  })
  
  observeEvent(input$country, {
    fit_location <- paste0("application/countries/", input$country,"/results/")
    
    #filename <- paste0(fit_location, "results",".RData")
    #load(filename)
  })
  
  observeEvent(input$run,{
    print("run")
    run_id <- paste0(input$run_name,"___",Sys.time())
    run_id <- str_replace_all(run_id," ","_")
    print(run_id)
    blaa <- "123"
    country <- input$country
    ir <- (input$index_ratio + 100) / 100
    index_ratio <- function(ratio, pediatric){
      ratio[is.na(ratio)] <- 0
      ratio <- ir * ratio
      ratio[is.infinite(ratio)] <- 1.5
      ratio
    }
    total_tests_target <- NULL
    if(!is.na(input$total_tests_target))
      total_tests_target <- input$total_tests_target
    max_diff <- (input$max_diff + 100) / 100
    dir.create(paste0("runs/",run_id))
    save(list=ls(), file=paste0("runs/",run_id,"/env.RData"))
    system(
      paste0("/opt/R/4.1.1/bin/Rscript -e \"source('run-script.R', echo=TRUE)\" --verbose --no-save --no-restore ", run_id, " > ", "runs/", run_id, "/out.txt"), 
      intern=FALSE, 
      wait=FALSE
    )
    print("blaa blaa")
  })
  
  output$run_log <- renderText({
    refreshTrigger()
    try({
      lin <- readLines(paste0("runs/", input$outrun,"/out.txt"))
      lin <- paste(lin, collapse = "\n")
    })
    if(exists("lin")) lin else ""
  })
  
  output$results_download <- downloadHandler(
    filename = function(){
      "yoda_results.zip"
    },
    content = function(con){
      file.copy(paste0("runs/", input$outrun,"/output.zip"), con)
    },
    contentType = "application/zip"
  )
  
  output$status <- renderText({
    refreshTrigger()
    try({
    st <- readLines(paste0("runs/", input$outrun,"/status.txt"))
    st <- paste(st, collapse = "\n")
    st <- paste0("Status: ", st)
    })
    if(exists("st")) st else "Status: Error"
  })
  
  observeEvent(input$refresh, {
    runs <<- dir("runs")
    updateSelectInput(session, "outrun", choices = runs, selected = input$outrun)
    refreshTrigger(rnorm(1))
  })
  
})

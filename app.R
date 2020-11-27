library(shiny)
library(RSQLite)
library(DBI)
library(DT)
library(tidyverse)

dbcon <- dbConnect(SQLite(), dbname="5003Project.db")

# Combine Tables
ProfClassCourse <- "SELECT C.classid, C.cid, P.pname, CR.ctitle, C.semester, C.year, C.seats, C.available, C.cday, C.ctime, CR.ccredit, 
          C.dsubject, CR.clevel FROM Professor AS P INNER JOIN Class AS C ON P.pid = C.pid INNER JOIN Course AS CR ON C.cid = CR.cid"
tempTable <- dbGetQuery(dbcon, ProfClassCourse)

# Create Table "Register"
dbSendQuery(dbcon, "CREATE TABLE Register( sid char(20), classid char(20), PRIMARY KEY (sid, classid) FOREIGN KEY (sid) REFERENCES Student, 
FOREIGN KEY (classid) REFERENCES Class)")


ui <- navbarPage(title="DBMS Project", id="navbar", 
                 
                 tabPanel("About", 
                          wellPanel(
                                p(strong(h4("The idea of this application is to simulate the actual student registration system using DBMS, and 
                                the data in our database are self-simulated.", 
                                            align="center"))),
                                h3("To close this application, you MUST click the QUIT tab panel. (Otherwise the system might CRASH!)"), align="center",
                                p("You can find the codes for this application using the following link."), 
                                HTML('<p> <a href="https://github.com/junzhang6/DBMSProject" 
                                      target="_blank">Link</a><br/> </p>')
                          ), style='width: 1300px'
                          
                 ),
                 
                 tabPanel("Tables", 
                          sidebarLayout(
                                sidebarPanel(
                                      p(strong("Hello there! Here you can browse some tables stored in our database. The first one
                                               contains the information of relevant courses, the second one is the information of our 
                                               faculty members, and the last one has the information of courses that offered in the next 
                                               semester. Have a look and go to the next page.")),
                                      radioButtons(inputId="Tables", choices=list("Course", "Prof", "Class"), label="Choose a dataset")
                                ),
                                
                                mainPanel(
                                      conditionalPanel("input.Tables == 'Course'",
                                                       dataTableOutput("CourseTable")),
                                      conditionalPanel("input.Tables == 'Prof'",
                                                       dataTableOutput("ProfTable")),
                                      conditionalPanel("input.Tables == 'Class'",
                                                       dataTableOutput("ClassTable"))
                                )
                          )
                 ),
                 
                 tabPanel("Welcome",
                          sidebarLayout(
                                sidebarPanel(
                                      numericInput(inputId="StudentID", label="Enter your ID number here", value=1, min=1),
                                      
                                      conditionalPanel(condition="input.StudentID > 23",
                                                       helpText("It looks like you are a new student here."),
                                                       # Insertion: a new student wants to create his/her ID
                                                       numericInput(inputId="NewID", label="Enter a ID number if you want to create one",
                                                                    value=FALSE, min=24),
                                                       selectInput(inputId="Gender", label="Choose your gender", choices=c("Male" = "M", "Female" = "F"),
                                                                   selected="Male"),
                                                       textInput(inputId="StudentName", label="Enter your name here"),
                                                       hr(),
                                                       actionButton("Confirm", label="Submit your changes")
                                      )
                                ),
                                
                                mainPanel(
                                      p(strong("Welcome student! Please enter your student ID in the text box and click the button to apply
                                           your changes. A regular student ID is either 1 or 2 digits with numbers only and it is BETWEEN 1 AND 23.
                                           An example of student ID is 5.")),
                                      p(h3("If you are a new student, type in a student number larger than 23 to create your account.")),
                                      textOutput(outputId="WelcomeText"),
                                      conditionalPanel(condition="input.StudentID <= 23",
                                                       tableOutput(outputId="Student")
                                      ),
                                      conditionalPanel(condition="input.Confirm",
                                                       textOutput(outputId="StudentInserted"),
                                                       tableOutput(outputId="NewStudent"))
                                )
                          )
                          
                 ),
                 
                 tabPanel("Enroll",
                          navlistPanel(
                                tabPanel("Open",
                                         numericInput(inputId="StudentIDCheck", label="Enter your student ID to begin course registration",
                                                      min=1, value=FALSE),
                                         conditionalPanel(condition="input.StudentIDCheck",
                                                          uiOutput(outputId="HelloStudent"),
                                                          hr(),
                                                          dataTableOutput(outputId="NextTermTable")
                                         )
                                ),
                                tabPanel("Enroll",
                                         p(h4("Choose a course you want to take by entering the class ID")),
                                         sidebarLayout(
                                               sidebarPanel(
                                                     numericInput(inputId="ClassID", label="Enter classid (You can select more than one course 
                                                                  by entering the different IDs)", value=FALSE), 
                                                     actionButton("ClassIDConfirm", label="Submit")
                                               ), # !!!!!! MOST IMPORTANT CONCEPT HERE !!!!!!
                                               mainPanel(
                                                     conditionalPanel(condition="input.ClassIDConfirm", 
                                                                      textOutput(outputId="EnrollMessage"),
                                                                      tableOutput(outputId="EnrollOut"))
                                               )
                                         )
                                ),
                                # tabPanel("Drop",
                                #          numericInput(inputId="CidDrop", label="Type in the course number that you want to drop"),
                                #          textOutput(outputId="DropMessage")
                                # ),
                                tabPanel("Your Schedule",
                                         textOutput(outputId="StudentSchedule"),
                                         tableOutput(outputId="StudentScheduleTable")
                                )
                                
                          ), style='width: 900px; height: 1000px'
                 ),
                 
                 tabPanel("Quit", value="stop", icon=icon("circle-o-notch"))
                 
)


server <- function(input, output) {
      
      # First Tab
      output$CourseTable <- renderDataTable({
            datatable(dbGetQuery(dbcon, "SELECT * FROM Course"), options=list(lengthMenu=c(10, 20, 30), pageLength=10))
      })
      output$ProfTable <- renderDataTable({
            datatable(dbGetQuery(dbcon, "SELECT * FROM Professor"), options=list(lengthMenu=c(10, 20, 30), pageLength=10))
      })
      output$ClassTable <- renderDataTable({
            datatable(dbGetQuery(dbcon, "SELECT * FROM Class"), options=list(lengthMenu=c(10, 20, 30), pageLength=10))
      })
      
      
      # Second Tab 
      output$WelcomeText <- renderText({ 
            # Invalid input
            if(input$StudentID < 1){
                  print("Please enter a valid ID number!")
            }
            # New student, ask the user whether he/she wants to create a sid 
            else if(input$StudentID > 23){
                  print("Do you want to create a student ID for yourself? If so, please enter a number greater than 23.")
            }
            else{
                  paste("Your ID number is:", input$StudentID)
            }
      })
      # Record user's sid
      UserSid <- reactive({
            dbGetQuery(dbcon, paste("SELECT * FROM Student WHERE sid =", input$StudentID))
      })
      output$Student <- renderTable({
            data.frame(UserSid())
      })
      # Insert info of the new student to our database
      output$StudentInserted <- renderText({
            print("We have successfully updated you to the system.")
      })
      StudentCreated <- eventReactive(input$Confirm, {
            ID <- input$NewID
            name <- input$StudentName
            gender <- input$Gender
            email <- str_replace_all(string=name, pattern=" ", repl="")
            email <- paste0(email, "-c@my.cityu.edu.hk")
            InsertStudent <- paste0("INSERT INTO Student (sid,sname,semail,sgender,DateOfBirth,phone,address) VALUES ('",
                                    ID, "','", name, "','", email, "','", gender, "','','','')")
            dbSendQuery(dbcon, InsertStudent)
            dbGetQuery(dbcon, paste("SELECT * FROM Student WHERE sid =", ID))
      })
      
      output$NewStudent <- renderTable({
            data.frame(StudentCreated())
      })
      
      
      # Third Tab
      
      # Open TAB
      output$HelloStudent <- renderUI({
            allsid <- dbGetQuery(dbcon, "SELECT sid FROM Student")
            # If student ID is not in our system yet:
            if((input$StudentIDCheck %in% allsid$sid) == F){
                  print("You have not created your account yet! Please go back to the last page if you want to create one.")
            }else{
                  StudentName <- dbGetQuery(dbcon, paste0("SELECT sname FROM Student WHERE sid = ", input$StudentIDCheck))
                  print(paste0("Hello, ", StudentName$sname, ". Here are the courses offered in the next semester."))
            }
      })
      output$NextTermTable <- renderDataTable({
            datatable(dbGetQuery(dbcon, ProfClassCourse), options=list(lengthMenu=c(15, 20), pageLength=15))
      })
      
      # Enroll TAB
      observeEvent(input$ClassIDConfirm, {
            
            # Operations in data frame
            TakenTable <- dbGetQuery(dbcon, "SELECT * FROM Taken")
            # RegisTable <- data.frame(sid=input$StudentIDCheck)
            
            # If user input class id correctly
            if((input$ClassID %in% tempTable$classid)==T){
                  # Check whether the class is full
                  if((filter(tempTable, classid==input$ClassID)$available) == 0){
                        output$EnrollMessage <- renderText("Sorry, the course you chose is full.")
                  }else{
                        # Check whether the student has enrolled the course before
                        if((input$StudentIDCheck %in% TakenTable$sid)==T){ # If student in our record
                              # If we can find the record
                              if((as.numeric(filter(TakenTable, sid==input$StudentIDCheck)$cid) == filter(tempTable, classid==input$ClassID)$cid)==T){
                                    output$EnrollMessage <- renderText("Sorry, you have previously enrolled this course and here is your record:")
                                    output$EnrollOut <- renderTable(filter(TakenTable, sid==as.numeric(input$StudentIDCheck)))
                              }else{
                                    # Else means that student can enroll the course and we need to update his/her schedule
                                    output$EnrollMessage <- renderText("You have successfully enrolled the course.")
                                    dbSendQuery(dbcon, paste0("INSERT INTO Register (sid,classid) VALUES ('", input$StudentIDCheck, "','", 
                                                              input$ClassID, "')"))
                              }
                        }else{ # New student 
                              output$EnrollMessage <- renderText("You have successfully enrolled the course.")
                              dbSendQuery(dbcon, paste0("INSERT INTO Register (sid,classid) VALUES ('", input$StudentIDCheck, "','", 
                                                        input$ClassID, "')"))
                        }
                  }
            }
            # User input classid is not valid
            else{
                  output$EnrollMessage <- renderText("You have entered a invalid class ID!")
            }
      })
      
      
      
      # Drop TAB (Leave it for now!)
      # output$DropMessage <- renderText({
      #       if(input$Confirm){
      #             print(paste0("Hi, ", input$StudentName, ". Are you sure you want to drop this class?"))
      #       }
      # })
      
      # Schedule TAB
      output$StudentSchedule <- renderText({
            print("Here is your schedule next term.")
      })
      observeEvent(input$ClassIDConfirm, {
            tempRegTable <- dbGetQuery(dbcon, "SELECT classid FROM Register")
            tempMerge <- merge(tempTable, tempRegTable, by="classid")
            tempMerge <- tempMerge %>% select(-c(seats, available))
            output$StudentScheduleTable <- renderTable(tempMerge)
      })
      
      
      
      # Quit
      observe({
            if(input$navbar == "stop"){
                  # Remove any new insertion at the end
                  dbSendQuery(dbcon, "DELETE FROM Student WHERE sid > 23")
                  stopApp()
            }
      })
}

dbRemoveTable(dbcon, "Register")
# dbDisconnect(dbcon)

# Run the application 
shinyApp(ui = ui, server = server)

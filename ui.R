
shinyUI(fluidPage(
  titlePanel("Bears On the Go"),
  
  sidebarLayout(
    sidebarPanel(
#   fluidRow(
#       column(3,
             textInput("building", "Building: Wheeler, Evans, etc."), 
             textInput("room", "Room: Aud, 10, etc. "),
             textInput("controlnumber", "Control Number: 00507, 12345, etc."),
             textInput("department", "Department: STATISTICS, PSYCHOLOGY, BENGALI, etc."),
             textInput("coursenumber", "Course Nummber: 1A, H7A, 103, 160B, 25AC, etc."),
             textInput("section", "Section: 105 DIS, 001 LEC, 202 LAB, 1-42 TUT, etc."),
             textInput("title", "Title Keywords: PERFORMANCE AND HISTORY, BACTERIAL PATHOGENESIS, etc."),
             textInput("credit", "Credits: 2, 4, 1-4, 2: PF, 1-4: PF, etc."),
             textInput("instructor", "Instructor: DENERO, THE STAFF, RAO, S, etc."),
             textInput("group", "Final Exam Group: 6, 8, NONE, etc."),
             submitButton("submit")
             
      ),
    
#       column(3,
#              h3("Advanced Search"),
#              textInput("controlnumber", "Control Number: 00507, 12345"),
#              textInput("department", "Department: (for now) STATISTICS, PSYCHOLOGY, BENGALI"),
#              textInput("coursenumber", "1A, H7A, 103, 160B, 25AC"),
#              textInput("section", "105 DIS, 001 LEC, 202 LAB, 1-42 TUT")
#             
#       ),
#       column(3,
#              textInput("title", "PERFORMANCE AND HISTORY, BACTERIAL PATHOGENESIS"),
#              textInput("credit", "2, 4, 1-4, 2: PF, 1-4: PF"),
#              textInput("instructor", "BRAUN,N       THE STAFF      DE KOSNIK, A T"),
#              textInput("group", "6, 8, NONE")
#              
#       ),
#       column(3,
#              submitButton("submit")
#       )
      
            
             
      
    
  mainPanel(textOutput("annotation"),
            tableOutput("advancedclasses")
            )
  )
))

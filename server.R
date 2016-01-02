source("helpers.R")
data = readRDS("classes.rds")
classesDF = data

for (i in 1:ncol(classesDF)) {
  classesDF[, i] = toupper(classesDF[, i])
}
classesDF[["Hours"]] = gsub("[^0-9-]", "", classesDF[["Day-Hour"]])
classesDF[["Hours Converted"]] = strsplit(classesDF[["Hours"]],  "-")
classesDF[["Hours As Decimals"]] = convertClassesTimetoDecimal(classesDF)

shinyServer(
  function(input, output) {
    
    #output$classes <- renderTable({getClassFromCurrentTime(input$building, input$room, classesDF, data)
    
    #})
    
      output$advancedclasses <- renderTable({advancedSearch(input$building, input$room, input$controlnumber,
                                                            input$department, input$coursenumber, input$section,
                                                            input$title, input$credit, input$instructor,
                                                            input$group, classesDF, data)[[1]]})
      
      output$annotation      <- renderText({advancedSearch(input$building, input$room, input$controlnumber,
                                                           input$department, input$coursenumber, input$section,
                                                           input$title, input$credit, input$instructor,
                                                           input$group, classesDF, data)[[2]]})
      
    
    
   
    
    
    
  }
)

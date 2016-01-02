is.finite.data.frame <- function(obj){
  sapply(obj,FUN = function(x) all(is.finite(x)))
}

shortenDay = function(d) {
  day = toupper(d)
  if (day == "MONDAY") {
    return("M")
  } else if (day == "TUESDAY") {
    return("TU")
  } else if (day == "WEDNESDAY") {
    return("W")
  } else if (day == "THURSDAY") {
    return("TH")
  } else if (day == "FRIDAY") {
    return("F")
  } else if (day == "SATURDAY") {
    return("SA")
  } else {
    return(d)
  }
}

convertTimetoDecimal = function(time) {
  t = sapply(strsplit(time, ":"), as.numeric)
  return(t[1] + t[2] / 60) 
}

convertClassesTimetoDecimal = function(classes) {
  hours = list()
  for (i in 1: nrow(classes)) {
    hour = classes[i, "Hours Converted"][[1]]
    beginning = 0
    end = 0
    if (length(hour) == 2) {
      if (nchar(hour[1]) == 3) {
        beginning = paste0(substr(hour[1], 1, 1), ":", substr(hour[1], 2, 3))
        beginning = convertTimetoDecimal(beginning)
      } else if (nchar(hour[1]) == 4) {
        beginning = paste0(substr(hour[1], 1, 2), ":", substr(hour[1], 3, 4))
        beginning = convertTimetoDecimal(beginning)
      } else {
        beginning = as.numeric((hour[1]))
      }
      if (nchar(hour[2]) == 3) {
        end = paste0(substr(hour[2], 1, 1), ":", substr(hour[2], 2, 3))
        end = convertTimetoDecimal(end)
      } else if (nchar(hour[2]) == 4) {
        end = paste0(substr(hour[2], 1, 2), ":", substr(hour[2], 3, 4))
        end = convertTimetoDecimal(end)
      } else {
        end = as.numeric(hour[2])
      }
      if (end < beginning) {
        end = end + 12
      } else if (beginning < end && grepl("P", classes[i, "Day-Hour"]) && !grepl("12[30]*P", classes[i, "Day-Hour"])) {
        beginning = beginning + 12
        end = end + 12
      } 
    }
    hours = c(hours, list(c(beginning, end)))
  }
  return(hours)
}

inRange = function(classTime, time) {
  return(time >= classTime[1] && time <= classTime[2])
}

advancedSearch = function(building, room, control, department, course, section,
                       title, credit, instructor, group, classes, data) {
  
  #make uppercase just in case user enters in lower case
  building = toupper(building)
  room = toupper(room)
  
  department = toupper(department)
  course = toupper(course)
  section = toupper(section)
  title = toupper(title)
  credit = toupper(credit)
  instructor = toupper(instructor)
  group = toupper(group)

  pb.txt = Sys.time()
  pb.date = as.POSIXct(pb.txt, tz = Sys.timezone())
  attributes(pb.date)$tzone = "America/Los_Angeles"
  d = weekdays(Sys.Date())
  day = shortenDay(d)
  time = substr(as.character(pb.date), 12, 16)
  time = convertTimetoDecimal(time)
  
  index = which(
                sapply(FUN = inRange, X = classes[["Hours As Decimals"]], time = time)
                & grepl(day, classes[, "Day-Hour"])
                & grepl(building, classes[, "Room"])
                & (grepl(paste("^", room, " ", sep = ""), classes[, "Room"]) | grepl(paste("", room), classes[, "Room"]))
                & grepl(control, classes[, "Control Number"])
                & grepl(department, classes[, "Department"])
                & grepl(course, classes[, "Course Number"])
                & grepl(section, classes[, "Section"])
                & grepl(title, classes[, "Course Title"])
                & grepl(credit, classes[, "Unit Credit"])
                & grepl(instructor, classes[, "Instructor"])
                & grepl(group, classes[, "Exam Group"])
                
  )
  

  matches = data[index, ]
  annotation = paste0("There are ", as.character(nrow(matches)), " matches.")
  
  return(list(matches, annotation))
  
}






#CODE BELOW NOT IN USE:
getClassFromCurrentTime = function(building, room, classes, originalClasses) {
  
  #building = 1
  #room = 2
  
  building = toupper(building)
  room = toupper(room)
  pb.txt = Sys.time()
  pb.date = as.POSIXct(pb.txt, tz = Sys.timezone())
  attributes(pb.date)$tzone = "America/Los_Angeles"
  d = weekdays(Sys.Date())
  day = shortenDay(d)
  time = substr(as.character(pb.date), 12, 16)
  time = convertTimetoDecimal(time)
  
  #print statement for testing
  print("Current time: ")
  print(time)
  
  
  a <- sapply(FUN = inRange, X= classes[["Hours As Decimals"]], time = time)
  b <- grepl(day, classes[, "Day-Hour"])
  c <- grepl(building, classes[, "Room"])
  d <- grepl(paste("^", room, " ", sep = ""), classes[, "Room"]) | grepl(paste("", room), classes[, "Room"])
  
  
  index <- a & b & c & d
  
  print(class(classes[index, ]))
  #return(classes[index,])
  return(originalClasses[index, ])
  
  
}
readBuilding = function() {
  building = toupper(readline(prompt = "Enter Building: "))
  wantRoom = "no"
  wantRoom = tolower(readline(prompt = "Do you want a specific room? "))
  if (wantRoom == "no") {
    return(building)
  } 
  room = toupper(readline(prompt = "Enter Room Number: "))
  return(c(building, room))
}

getClasses =  function() {
  location = readBuilding()
  if (length(location) == 1) {
    index = which(grepl(location, classes[, "Room"]))
    matches = classes[index, ]
  } else {
    index = which(grepl(location[1], classes[, "Room"]) & grepl(location[2], classes[, "Room"]))
    matches = classes[index, ]
  }
  print("Classes matching query: ") 
  i = 1
  while (i < nrow(matches)) {
    print(paste0("Control Number: ", matches[i, "Control Number"]))
    print(paste0("Course Number: ", matches[i, "Course Number"]))
    print(paste0("Section: ", matches[i, "Section"]))
    print(paste0("Day Hour: ", matches[i, "Day Hour"]))
    print(paste0("Room: ", matches[i, "Room"]))
    print(paste0("Course Title: ", matches[i, "Course Title"]))
    print(paste0("Instructor: ", matches[i, "Instructor"]))
    print(paste0("Department: ", matches[i, "Department"]))
    print(" ")
    i = i + 1
  }
}

readAll = function(control, department, course, section, day, time, building,
                   room, title, credit, instructor, group) {
  #control = toupper(readline(prompt = "Enter Control Number:  "))
  #department = toupper(readline(prompt = "Enter Department: "))
  #course = toupper(readline(prompt = "Enter Course Number:  "))
  #section = toupper(readline(prompt = "Enter Section: "))
  #day = toupper(readline(prompt = "Enter Day: "))
  #time = toupper(readline(prompt = "Enter Time: "))
  #building = toupper(readline(prompt = "Enter Building: "))
  #room = toupper(readline(prompt = "Enter Room Number: "))
  #title = toupper(readline(prompt = "Enter Title Keywords: "))
  #credit = toupper(readline(prompt = "Enter Credit Units: "))
  #instructor = toupper(readline(prompt = "Enter Instructor: "))
  group = toupper(readline(prompt = "Enter Exam Group: "))
  return(c(control, department, course, section, day, time, building, room, title, credit,
           instructor, group))
}

#Potentially useful code
#i = 1
#while (i <= nrow(matches)) {
#  print(paste0("Control Number: ", matches[i, "Control Number"]))
#  print(paste0("Course Number: ", matches[i, "Course Number"]))
#  print(paste0("Section: ", matches[i, "Section"]))
#  print(paste0("Day Hour: ", matches[i, "Day Hour"]))
#  print(paste0("Room: ", matches[i, "Room"]))
#  print(paste0("Course Title: ", matches[i, "Course Title"]))
#  print(paste0("Instructor: ", matches[i, "Instructor"]))
#  print(paste0("Department: ", matches[i, "Department"]))
#  print(" ")
#  i = i + 1
#}

#i = 1
#while (i < nrow(matches)) {
#  print(paste0("Control Number: ", matches[i, "Control Number"]))
#  print(paste0("Course Number: ", matches[i, "Course Number"]))
#  print(paste0("Section: ", matches[i, "Section"]))
#  print(paste0("Day Hour: ", matches[i, "Day Hour"]))
#  print(paste0("Room: ", matches[i, "Room"]))
#  print(paste0("Course Title: ", matches[i, "Course Title"]))
#  print(paste0("Instructor: ", matches[i, "Instructor"]))
#  print(paste0("Department: ", matches[i, "Department"]))
#  print(" ")
#  i = i + 1
#}

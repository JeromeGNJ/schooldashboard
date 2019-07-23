rm(list = ls())
library(shiny, warn.conflicts = FALSE)
library(ggplot2, warn.conflicts = FALSE)
library(readxl)
library(readr)
library(plotly, warn.conflicts = FALSE)
library(markdown)
library(shinydashboard)
library(shinyBS)


packageVersion('plotly')

Course_1613221 <- read_csv("data/Course_1613221.csv")
Course_1613221 <- Course_1613221[Course_1613221$`Instructor id`==123456,]
Course_1613221$`Exam  grades`<-toupper(Course_1613221$`Exam  grades`)
Course_1613221$`Exam  grades`<-sub(" ","",Course_1613221$`Exam  grades`)
assign1_avg=table(Course_1613221$`Assignment submitted`)
assign1_avg=data.frame(assign1_avg)
assign1_avg=round(mean(assign1_avg$Freq))
students <- data.frame(read_csv("students.csv", 
                                col_types = cols(id = col_character())))
stukey=setNames(as.list(students$pass), students$id)
Logged = FALSE;
my_username <- "admin"
my_password <- "admin"
access= FALSE

ui1 <- function(){ 
  tagList(
    div(id = "login",
        tags$img(div(img(src = "https://base-edu.in/upload/iit-madras-1%20(1).jpg", width="400",height="200"), style="text-align: center; ")),
        wellPanel(
          textInput("userName", "Username"),
                  passwordInput("passwd", "Password"),
                  br(),actionButton("Login", "Log in")),
        textOutput("status")),
    tags$style(type="text/css", "#login {font-size:10px;   text-align: left;position:absolute;top: 20%;left: 50%;margin-top: -100px;margin-left: -150px;}")
  )}

ui2 <- function(){tagList(
  mainPanel(
 fluidRow({  
   column(12,
          tags$a(href="javascript:history.go(0)", 
                 popify(
                   tags$i(class="fa fa-sign-out fa-3x"),
                   title = "Logout", 
                   content = "Click here to Logout session",
                   placement = "left"))
          ,align="right")
 
   }),
    tags$head(
      tags$style(HTML("/*
                      * Component: Info Box
                      * -------------------
                      */
                      
                      .info-box {
                      display: block;
                      min-height: 90px;
                      background: #adc7db;
                      width: 100%;
                      box-shadow: 0 1px 1px rgba(0, 0, 0, 0.1);
                      border-radius: 2px;
                      margin-bottom: 15px;
                      }
                      .info-box small {
                      font-size: 20px;
                      }
                      .info-box .progress {
                      background: rgba(0, 0, 0, 0.2);
                      margin: 5px -10px 5px -10px;
                      height: 2px;
                      }
                      .info-box .progress,
                      .info-box .progress .progress-bar {
                      border-radius: 0;
                      }
                      .info-box .progress .progress-bar {
                      background: #fff;
                      }
                      .info-box-icon {
                      border-top-left-radius: 2px;
                      border-top-right-radius: 0;
                      border-bottom-right-radius: 0;
                      border-bottom-left-radius: 2px;
                      display: block;
                      float: left;
                      height: 90px;
                      width: 90px;
                      text-align: center;
                      font-size: 45px;
                      line-height: 90px;
                      background: rgba(0, 0, 0, 0.2);
                      }
                      .info-box-icon > img {
                      max-width: 100%;
                      }
                      
                      .info-box-number {
                      display: block;
                      font-weight: bold;
                      font-size: 18px;
                      }
                      .progress-description,
                      .info-box-text {
                      display: block;
                      font-size: 16px;
                      white-space: wrap;
                      overflow: hidden;
                      text-overflow: ellipsis;
                      }
                      
                      .info-box-more {
                      display: block;
                      }
                      .progress-description {
                      margin: 0;
                      }
                      
                      .bg-yellow,
                      .callout.callout-warning,
                      .alert-warning,
                      .label-warning,
                      .modal-warning .modal-body {
                      background-color: #f39c12 !important;
                      }
                      
                      "))
      )
    #,fluidRow(column(12,actionButton("logout", "Logout"),align="right"))
  ),

  tabsetPanel(type="pills",
   tabPanel(
  fluidRow(
  h4(HTML("&nbsp &nbsp"),"Aggregate Performance of all students in course",HTML("&nbsp &nbsp"))),
  fluidRow(br()),
  fluidRow(
    infoBox(title = paste("On an average",assign1_avg,"assignments are submitted by students per day"))
    ),
 br(),
  fluidRow(
    box(
      plotlyOutput("engagement" ),
      textOutput("engagement_info")),
    box(
      plotlyOutput("grade"),
      textOutput("grade_info")
    )
  ),
  
  fluidRow(
    box(
      plotlyOutput("activity"),
      textOutput("activity_info")
    ),
    box(textOutput("assignment1_info"),
        plotlyOutput("assignment1")
    )
    
  ),
  fluidRow(
    box(
      plotlyOutput("submitted_date"),
      textOutput("submitted_day_info")
    )
  )),
  tabPanel(
  fluidRow(
    h4(HTML("&nbsp &nbsp"),"Performance of a Selected student",HTML("&nbsp &nbsp"))),
  # student Performance
  fluidRow(
    box(
      uiOutput("student_select"),width=5),
    box(
      textOutput("risk_score"),imageOutput("risk"),width = 2,height = 190),
    box(valueBoxOutput("stu_status",width = 5),width = 5)
    
  ),br(),
 fluidRow(
   infoBox(textOutput("box1")),
   infoBox(textOutput("box2"))
 ),
 fluidRow(
   infoBox(textOutput("box3")),
   infoBox(textOutput("box4"))
 ),br(),
 fluidRow(
   box(
     plotlyOutput("student_libvisit"),
     textOutput("student_libvisit_info")
   ),
   box(  plotlyOutput("assignment1_stu"),
         textOutput("assignment1_stu_info")
   )
   
   
 ),br(),
 fluidRow(
   box(
     plotlyOutput("engagement_stu"),
     textOutput("engagement_stu_info")
   ),
   box(  
     plotlyOutput("student_libuse"),
     textOutput("student_libuse_info")
   )
   
 ),br(),br(),br()
  
  )))}

ui3 <- function(){tagList(mainPanel(
  fluidRow({  
    column(12,
           tags$a(href="javascript:history.go(0)", 
                  popify(
                    tags$i(class="fa fa-sign-out fa-3x"),
                    title = "Logout", 
                    content = "Click here to Logout session",
                    placement = "left"))
           ,align="right")
    
  }),
  tags$head(
    tags$style(HTML("/*
                    * Component: Info Box
                    * -------------------
                    */
                    .info-box {
                    display: block;
                    min-height: 90px;
                    background: #adc7db;
                    width: 100%;
                    box-shadow: 0 1px 1px rgba(0, 0, 0, 0.1);
                    border-radius: 2px;
                    margin-bottom: 15px;
                    }
                    .info-box small {
                    font-size: 20px;
                    }
                    .info-box .progress {
                    background: rgba(0, 0, 0, 0.2);
                    margin: 5px -10px 5px -10px;
                    height: 2px;
                    }
                    .info-box .progress,
                    .info-box .progress .progress-bar {
                    border-radius: 0;
                    }
                    .info-box .progress .progress-bar {
                    background: #fff;
                    }
                    .info-box-icon {
                    border-top-left-radius: 2px;
                    border-top-right-radius: 0;
                    border-bottom-right-radius: 0;
                    border-bottom-left-radius: 2px;
                    display: block;
                    float: left;
                    height: 90px;
                    width: 90px;
                    text-align: center;
                    font-size: 45px;
                    line-height: 90px;
                    background: rgba(0, 0, 0, 0.2);
                    }
                    .info-box-icon > img {
                    max-width: 100%;
                    }
                    
                    .info-box-number {
                    display: block;
                    font-weight: bold;
                    font-size: 18px;
                    }
                    .progress-description,
                    .info-box-text {
                    display: block;
                    font-size: 16px;
                    white-space: wrap;
                    overflow: hidden;
                    text-overflow: ellipsis;
                    }
                    
                    .info-box-more {
                    display: block;
                    }
                    .progress-description {
                    margin: 0;
                    }
                    
                    .bg-yellow,
                    .callout.callout-warning,
                    .alert-warning,
                    .label-warning,
                    .modal-warning .modal-body {
                    background-color: #f39c12 !important;
                    }
                    
                    "))
    ),
 # fluidRow(column(12,actionButton("logout", "Logout"),align="right")),
  fluidRow(
 h4(
   (textOutput("hi"))
   )),
  fluidRow(
    h2("Student Performance ")),
  # student Performance
 fluidRow(
   infoBox(textOutput("box1")),
   infoBox(textOutput("box2"))
 ),
 fluidRow(
   infoBox(textOutput("box3")),
   infoBox(textOutput("box4"))
 ),br(),
  fluidRow(
    box(
        plotlyOutput("student_libvisit"),
        textOutput("student_libvisit_info")
    ),
    box(  plotlyOutput("assignment1_stu"),
          textOutput("assignment1_stu_info")
    )
    
    
  ),br(),
  fluidRow(
    box(
        plotlyOutput("engagement_stu"),
        textOutput("engagement_stu_info")
    ),
    box(  
          plotlyOutput("student_libuse"),
          textOutput("student_libuse_info")
    )
    
  ),br(),br(),br()
  
))}

ui = (  htmlOutput("page")  )
  
server = (function(input, output,session) {
  
  USER <- reactiveValues(Logged = Logged,access=access)
  
  observeEvent(input$logout, print(input$logout[[1]]))  
  
  observe({ 
 #   print(input$logout[[1]])
    if (USER$Logged == FALSE) {
      
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- input$userName
          Password <- input$passwd
         
          if (Username==my_username & Password==my_password) {
            USER$access="staff"
              USER$Logged <- TRUE
              Logged<-"TRUE"}
             else if(is.element(Username,students$id)){
                  if(Password==stukey[Username][[1]]){
                  USER$Logged <- TRUE
                  USER$access="student"
                  }else{
                    output$status <- renderText("Please enter valid Credentials")
                    
                  }
              }
            
          else {
            output$status <- renderText("Please enter valid Credentials")
          }
        } 
      }
    }    
  })
  observe({
    if (USER$Logged == FALSE) {
      
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",ui1())))
      })
      print(ui)
    }
    if (USER$Logged == TRUE) 
    {
      if(USER$access=="staff"){
      output$page <- renderUI({
        div(class="outer",do.call(navbarPage,c(inverse=TRUE,title = "Staff Dashboard",ui2())))
      })
      print(ui)}
      if(USER$access=="student"){
        output$page <- renderUI({
          div(class="outer",do.call(navbarPage,c(inverse=TRUE,title = "Student Dashboard",ui3())))
        })
        print(ui)}
    }
  })
  
  
  
  INPUT <- reactiveValues()
  
  observe({
    INPUT$Course_1613221<- Course_1613221
    INPUT$assign<-0
    INPUT$libvis<-0
    INPUT$engage<-0
    #INPUT$libusa<-0
    INPUT$assignstu<-0
    INPUT$engage<-0
  })
  
 
  
  output$student_select <- renderUI({
    
    selectInput("selectStudent",label="StudentID",choices = INPUT$Course_1613221$StudentID )
  })
  
  output$stu_status <- renderValueBox({
    grade<-Course_1613221[Course_1613221$StudentID==input$selectStudent,][8]
    if(grade=="HIGHRISK"){
      vcolor="red"
    }else if(grade=="MEDIUMRISK"){
      vcolor="orange"
    }else if(grade=="LOWRISK"){
      vcolor="green"
    }  
    stu_grade<-Course_1613221[Course_1613221$StudentID==input$selectStudent,][7]
    valueBox(value=stu_grade, subtitle="Predicted final exam score", icon =NULL, color = vcolor,
             href = NULL
    )
  })
  
  output$risk_score<-renderText({
    b<- "Predicted Final Exam Risk :"
  })   
  
  output$risk<-renderImage({
    grade<-Course_1613221[Course_1613221$StudentID==input$selectStudent,][8]
    if(grade=="HIGHRISK"){
      img_code="high"
    }else if(grade=="MEDIUMRISK"){
      img_code="medium"
    }else if(grade=="LOWRISK"){
      img_code="low"
    }  
    filename <- normalizePath(file.path('./data',
                                        paste(img_code,'.png', sep='')))
    
    list(src = filename,
         alt = paste("Image"))
  }, deleteFile = FALSE)
  
  output$engagement <- renderPlotly({
    engage_class = Course_1613221[,c(39,41,43,45,47,49,51)][1,]
    Rating <- t(engage_class)
    Weeks <- c(1:7)
    engage_class <- data.frame(Weeks, Rating)
    INPUT$engage<- round(mean(engage_class$Rating))
    p <- plot_ly(engage_class, x = ~Weeks, y = ~Rating, name = 'Engagement Rating', type = 'scatter', mode = 'lines+markers')%>%layout(title = "Engagement Rating", yaxis = list(range = c(0, 10)))
  })
  
  
  output$grade <- renderPlotly({
    
    p <- plot_ly(x =Course_1613221$`Exam  grades`, type = "histogram")%>%layout(title = "Distribution of Exam Grades")
  })
  
  
  output$activity <- renderPlotly({
    
    behavior <- Course_1613221[,c(53,55,57,59,61,63,65)]
    x=1
    for (i in behavior) {
      behavior[[x]]= toupper(behavior[[x]])
      behavior[[x]]=sub(" ","",behavior[[x]]) 
      x=x+1 }
    
    weeks<- c("week1","week2","week3","week4","week5","week6","week7")
    status<-c("VeryActive","Active","LessActive")
    behavior_data<- data.frame("weeks"=weeks,"veryactive"=c(1:7),"active"=c(1:7),"lessactive"=c(1:7))
    x=1
    for (i in behavior) {
      veryactive=nrow(behavior[i=="VERYACTIVE",])
      active=nrow(behavior[i=="ACTIVE",])
      lessactive=nrow(behavior[i=="LESSACTIVE",])
      behavior_data[x,2:4]=c(veryactive,active,lessactive)
      # print(c(veryactive,active,lessactive))
      x=x+1
    }
    
    p <- plot_ly(behavior_data, x = ~veryactive, y = ~weeks, type = 'bar', name = 'Very Active', orientation='h') %>%
      add_trace(x = ~active, name = 'Active') %>%
      add_trace(x = ~lessactive, name = 'Less Active') %>%
      layout( barmode = 'stack',xaxis = list(title = ""),title="Activity Behaviour",
              yaxis = list(title =""))
    p
  })
  
  
  output$assignment1 <- renderPlotly({
    
    assignment1 <- Course_1613221[,c(53,55,57,59,61,63,65)]
    Score<-Course_1613221$Assignment1*100
    INPUT$assign<-mean(Course_1613221$Assignment1)
    ggplot(Course_1613221,aes(Score))+geom_histogram(binwidth = 10, fill=rgb(0.0,0.4,0.7,0.9))+ggtitle("Distribution of Grades in Assignment 1")+ylab("Count")
  })
  
  output$submitted_date <- renderPlotly({
    submitted_date <- Course_1613221[,67]
    submitted_dayofweek <- strftime(as.Date(submitted_date$`Assignment submitted`,format = "%m/%d/%Y"),"%u")
    submitted_dayofweek_percent <- data.frame(table(
      submitted_dayofweek)*100/sum(table(submitted_dayofweek)))
    ggplot(submitted_dayofweek_percent,
           aes(x = submitted_dayofweek, y = Freq))+
      geom_bar(stat = "identity",fill=rgb(0.0,0.4,0.7,0.9))+
      ggtitle("Distribution of Assignment Submission Day")+
      ylab("Submission %") + xlab("Day of Week (Monday = 1)")
    
  })
  
  
  
  output$student_libvisit <- renderPlotly({
    if(USER$access=="staff"){
      Username=input$selectStudent
    }else{
      Username=input$userName
    }
    engage_class = Course_1613221[,c(24,26,28,30,32,34,36)][1,]
    engage_stu =  Course_1613221[Course_1613221$StudentID==Username,c(23,25,27,29,31,33,35)][1,]
    Visits <- t(engage_class)
    rating_stu <- t(engage_stu)
    Weeks <- c(1:7)
    engage_class <- data.frame(Weeks, Visits,rating_stu)
    ratingstu<-data.frame(rating_stu)
    INPUT$stulibvis<-round(mean(ratingstu$rating_stu))
    p <- plot_ly(engage_class, x = ~Weeks, y = ~Visits, name = 'Class Average', type = 'bar')%>%
      add_trace(y = ~rating_stu, name = 'Student')%>%layout(title="Library Visits", yaxis=list(title="No of  Visits"),xaxis=list(dtick=1))
  })
  
  
  output$assignment1_stu <- renderPlotly({
    if(USER$access=="staff"){
      Username=input$selectStudent
    }else{
      Username=input$userName
    }
    assignment1 <- Course_1613221[,c(53,55,57,59,61,63,65)]
    score <- Course_1613221[Course_1613221 == Username,5][[1]][1]*100
    Score<-Course_1613221$Assignment1*100
    INPUT$assignstu<-score
    ggplot(Course_1613221,aes(Score))+geom_histogram(binwidth = 10, fill=rgb(0.0,0.4,0.7,0.9))+geom_vline(aes(xintercept= score ),   # Ignore NA values for mean
                                                                                                          color="red", size=10)+ggtitle("Assignment 1 grade of Student w.r.t Class")+ylab("Count")
    
  })
  
  output$engagement_stu <- renderPlotly({
    if(USER$access=="staff"){
      Username=input$selectStudent
    }else{
      Username=input$userName
    }
    engage_class = Course_1613221[,c(39,41,43,45,47,49,51)][1,]
    engage_stu =  Course_1613221[Course_1613221$StudentID==Username,c(38,40,42,44,46,48,50)][1,]
    Rating <- t(engage_class)
    rating_stu <- t(engage_stu)
    Weeks <- c(1:7)
    engage_class <- data.frame(Weeks, Rating,rating_stu)
    classrate<-data.frame(Rating)
    sturate<-data.frame(rating_stu)
    INPUT$classrate<-mean(classrate$Rating)
    INPUT$sturate<-mean(sturate$rating_stu)
    p <- plot_ly(engage_class, x = ~Weeks, y = ~Rating, name = 'Class Average', type = 'scatter', mode = 'lines+markers')%>%
      add_trace(y = ~rating_stu, name = 'Student', mode = 'lines+markers')%>%layout(title="Engagement Rating", yaxis = list(range = c(0, 10)),xaxis=list(dtick=1))
  })
  
  output$student_libuse <- renderPlotly({
    if(USER$access=="staff"){
      Username=input$selectStudent
    }else{
      Username=input$userName
    }
    libuse_class = Course_1613221[,c(10,12,14,16,18,20,22)][1,]
    libuse_stu =  Course_1613221[Course_1613221$StudentID==Username,c(9,11,13,15,17,19,21)][1,]
    Usage <- t(libuse_class)
    rating_stu <- t(libuse_stu)
    Weeks <- c(1:7)
    libuse_class <- data.frame(Weeks, Usage,rating_stu)
    ratingstu<-data.frame(rating_stu)
    INPUT$stulibuse<-round(mean(ratingstu$rating_stu))
    p <- plot_ly(libuse_class, x = ~Usage, y = ~Weeks, name = 'Class Average', type = 'bar', orientation = 'h')%>%
      add_trace(x = ~rating_stu, name = 'student')%>%layout(title="Library Usage", xaxis = list(range = c(0, 7),dtick=1))
  })
  
  output$hi<-renderText({
    a<-paste("   Logged in User: ",input$userName)
  })
  
  output$engagement_info<-renderText({
    
    b<-"INFO : Average Engagement rating of Class over the weeks" })
  
  output$grade_info<-renderText({
    
    b<-"INFO : Count of students in each risk bucket based on predicted exam grades" })
  
  output$activity_info<-renderText({
    
    b<-"INFO : Count of students with different Activity levels by Week" })
  output$submitted_day_info<-renderText({
    
    b<-"INFO : Percentage of submissions by Day of the Week" })
  output$assignment1_info<-renderText({
    
    b<-paste("Average Assignment 1 grade distribution of class is",round(INPUT$assign)) })
  
  output$box1<-renderText({
    b<-paste("Student has visited the library",INPUT$stulibvis,"days per week on average") })
  
  output$box2<-renderText({
    b<-paste("Assignment 1 grade of student is",round(INPUT$assignstu))})
  
  output$box3<-renderText({
    if(INPUT$classrate>INPUT$sturate){
      comp="less"
    }else{
      comp="more"
    }
    b<-paste("The engagement rating of the student is",comp,"than class average") })
  
  output$box4<-renderText({
    b<-paste("The library usage of the student is",INPUT$stulibuse,"days per week on average") })
  
  
  output$student_libvisit_info<-renderText({
    
    b<-"INFO : Student library visit compared with class average"})

  output$assignment1_stu_info<-renderText({
    
    b<-"INFO : Assignment 1 grade of student compared with grade distribution of class average" })
  
  
  output$engagement_stu_info<-renderText({
    
    b<-"INFO : Student engagement rating compared with class average"  })
  
  output$student_libuse_info<-renderText({
    
    b<-"INFO : Library Usage of student compared with class average" })
  
  output$staff_box<-renderText({
    b<-paste("On an average",assign1_avg,"assignments are submitted by students per day")
  })
  

 })

  

runApp(list(ui = ui, server = server))
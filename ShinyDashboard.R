library(shinydashboard)
library(ggplot2)
library(data.table)

setwd("F:/IE Big Data & Business Analytics/Term 3/R/Group Project")
train = data.table(read.csv("BankCamp_train.csv"))

ui = dashboardPage(  skin = "purple",
                     dashboardHeader(title = "Assignment - Group E",
                                     dropdownMenu(type = "message",
                                                  messageItem( from = "Dashboard Update",
                                                               message = "Use the Dashboard for Sample EDA"),
                                                  messageItem(from = "Group 5",
                                                              message = "R Workgroup Presentation at 6 PM",
                                                              icon = icon("handshake-o"),time = "07-06-2019")
                                                  
                                     ),
                                     dropdownMenu(type = "notifications",
                                                  notificationItem(
                                                    text = "Server are Running Fine",
                                                    icon = icon("warning"),
                                                    status = "success"
                                                  ))
                                     
                                     
                     ),
                     dashboardSidebar(
                       sidebarMenu(
                         sidebarSearchForm("searchText" , "buttonSearch", "Search"),
                         menuItem("Dashboards", tabName = "EDA"
                                  ,icon = icon("dashboard")),
                         menuSubItem("Dashboard - I" ,tabName = "dashboard1"),
                         menuSubItem("Dashboard - II",tabName = "dashboard2"),
                         # menuItem("Detailed Analysis"),
                         menuItem("Raw Data", tabName = "rawdata", badgeLabel = "New" ,badgeColor = "green")
                       )),  
                     dashboardBody(
                       tabItems(
                         tabItem( tabName = "EDA" , 
                                  h1("Shiny Dashboard"),
                                  fluidRow(
                                    infoBox("Volume of Data", 36169 , color= "lime",icon = icon("thumbs-up")),
                                    infoBox("Clinets With Loan", "62.1 % ", color = "navy",icon = icon("thumbs-up")),
                                    infoBoxOutput("Percentage")
                                  ),
                                  fluidRow(
                                    
                                    box(title = "Overall Distribution of Duration", status ="primary",
                                        solidHeader = T, background = "aqua" ,
                                        plotOutput("histogram")),
                                    box(title = "Number of Bins for the Histogram" , status = "warning",
                                        solidHeader = T, 
                                        "Change the slider to control the number of bins in the graph",
                                        sliderInput("bins", "Number of Breaks",2,20,10))
                                  )),
                         tabItem(tabName = "dashboard1" , 
                                 h1("Dashboard - I"),
                                 fluidRow(
                                   tabBox(
                                   tabPanel( title = "Campaigns By Marital Status", status = "primary",
                                        solidHeader =  T, background = "purple",
                                        plotOutput("barplot1")
                                        
                                   ),
                                   
                                   tabPanel( title = "Campaigns Across Education by Marital Status", status = "primary",
                                        solidHeader =  T, background = "purple",
                                        plotOutput("barplot2")
                                        
                                   )),
                                   tabBox(
                                    tabPanel( title = "Average Banlance By Marital Status", status = "primary",
                                        solidHeader =  T, background = "purple",
                                        plotOutput("barplot3")),
                                    
                                    tabPanel( title = "Average Banlance  Across Education by Marital Status", status = "primary",
                                        solidHeader =  T, background = "purple",
                                        plotOutput("barplot4")    
                                   ))
                                 )
                         ),
                         tabItem(tabName = "rawdata" , 
                                 h1("Raw Backend Data"),
                                 fluidRow(
                                   column(width = 8,
                                          titlePanel( "Filters"),
                                          sidebarLayout(
                                            sidebarPanel(
                                              selectInput("instance", "Select Level of Education", 
                                                          choices = train$education)
                                            ),
                                            mainPanel(
                                              tableOutput("traindata")
                                            )
                                          )
                                   ))
                                 
                                 
                         ),
                         tabItem(tabName = "dashboard2" , 
                                 h1("Dashboard - II"))
                         
                       )
                       
                       
                     )
)




server = function(input, output) { version
  
  output$histogram = renderPlot({
    hist(train$duration, breaks = input$bins , main ="" , xlab = "Duration"
         , ylab= "Frequency" , axes = T , col = "red")
  })
  
  output$barplot1 = renderPlot({
    ggplot(data=train, aes(x=marital, y=campaign , color = marital)) +
      geom_bar(stat="identity" , fill = "white") + 
      theme_classic() + 
      labs (x = "Marital   Status" , y = "Number   of   Campaigns" ) +
      theme(
        
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"), 
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black")) +
      scale_color_discrete(name = "Marital Status") +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$barplot2 = renderPlot({
    ggplot(data=train, aes(x=education,y=campaign , color = marital)) +
      geom_bar(stat="identity" , fill = "white") + 
      theme_classic() + 
      labs (x = "Education" , y = "Number   of   Campaigns" ) +
      theme(
        
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"), 
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black")) +
      scale_color_discrete(name = "Marital Status") +
      scale_y_continuous(labels = scales::comma)
  })
  
  
  
  output$barplot3 = renderPlot({
    ggplot(data=train, aes(x=marital, y=balance , color = marital)) +
      geom_bar(stat="identity" , fill = "white") + 
      theme_classic() + 
      labs (x = "Marital   Status" , y = "Average Balance" ) +
      theme(
        
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"), 
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black")) +
      scale_color_discrete(name = "Marital Status") +
      scale_y_continuous(labels = scales::comma)
  })
  
  output$barplot4 = renderPlot({
    ggplot(data=train, aes(x=education,y=balance , color = marital)) +
      geom_bar(stat="identity" , fill = "white") + 
      theme_classic() + 
      labs (x = "Education" , y = "Average Balance" ) +
      theme(
        
        axis.title.x = element_text(color="black", size=14, face="bold"),
        axis.title.y = element_text(color="black", size=14, face="bold"), 
        legend.title = element_text(color = "black", size = 10),
        legend.text = element_text(color = "black")) +
      scale_color_discrete(name = "Marital Status") +
      scale_y_continuous(labels = scales::comma)
  })
  
  
  output$traindata = renderTable({
    statefilter = subset(train, train$education == input$instance
                         , select = c("age"   ,    "job"    ,   "marital" ,
                                      "default" , "balance" , "previous",
                                      "campaign" , "pdays","poutcome"  ,"y" ))
  })
  
  output$Percentage = renderInfoBox({
    infoBox("% Bought Term Deposit" , 
            round(nrow(train[y=="yes",])/nrow(train) * 100,2),  color = "red",
            icon = icon("warning"))
  })  
}

shinyApp(ui, server)

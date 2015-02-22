shinyUI(fluidPage(theme="bootstrap.css",
    headerPanel("What's your future car?"),
    
    sidebarPanel(
      selectInput("year", 
      label = "When do you want to buy the car in the next 15 years?",
      choices = c(2015:2030),
      selected = "2015"),
#       selectInput("var", 
#       label = "Which region are you from?",
# #       choices = levels(pp$Region),
#       choices = state.name,
#       selected = "California"),
#       selectInput("member", 
#       label = "What's the size of your family unit?",
#       choices = c(1,2,3,"4+"),
#       selected = "2"),
     selectInput("driver", 
     label = "What kind of driver are you?",
     choices = c("I drive a lot of miles everyday, and I have no public transit access", "I take the car to and from work mostly", "I take the car only to the grocery store!"),
     selected = "I take the car to and from work mostly"),
     selectInput("tech", 
     label = "Are you a technology freak?",
     choices = c("I will try anything new!", "I will consider if my friends have it", "Technology Schmechnology"),
     selected = "If my friends are using it, I will consider it"),
     selectInput("home", 
     label = "Can you charge at home?",
     choices = c("Yes", "No"),
     selected = "No"),
     selectInput("work", 
     label = "Can you charge at work?",
     choices = c("Yes", "No"),
      selected = "No")
     ),

    mainPanel(
      tabsetPanel(
        tabPanel("Business As Usual Scenario",
      h3('Can you contribute to the reduction of greenhouse gas emissions through your car purchase?'),
      h5('Choose your buyer attributes on the left, and the model will
          predict the feasible alternative vehicle choices for you.This is the scenario where the 
          infrastructure availability is business-as-usual, 
         i.e. stations grow at a steady rate without
         much government intervention.'),
#       h4('Your inputs:'),
#       textOutput("inputValue"),
      br(),
      h4('Your first choice is'),
      textOutput("prediction1"),
      br(),
      htmlOutput('firstCarImage'),
      h4('Your second choice is'),
      textOutput("prediction2"),
      br(),
      htmlOutput('secondCarImage'),
      h4('Your third choice is'),
      textOutput("prediction3"),
      br(),
      htmlOutput('thirdCarImage')
      ),
      tabPanel("Good Infrastructure Scenario",
      h3('What if you had good recharging and fuel infrastructure?'),
      h5("Some things are not under your control. What if government invests in a decent
         public recharging and refueling infrastructure for advanced vehicles? Will it change
         your affordability of zero emissions cars?"),
#       textOutput("inputValue2"),
      br(),
      h4('Your first choice is'),
      textOutput("predictionI1"),
      br(),
      htmlOutput('firstCarImageI1'),
      h4('Your second choice is'),
      textOutput("predictionI2"),
      br(),
      htmlOutput('secondCarImageI2'),
      h4('Your third choice is'),
      textOutput("predictionI3"),
      br(),
      htmlOutput('thirdCarImageI3')),
      tabPanel('Timeseries Plots',
      h3("Purchasing Behavior Over Time for the Scenarios"),
      h5("If you continue to follow the same buying attributes, this will be your buyer profile 
         over time. The plots show the purchase probability shares of vehicle technologies, i.e. how likely you are to
         buy a certain car..longer bars mean 'more likely', and shorter bars mean 'less likely'."),
      br(),
      plotOutput("refplot"),
      br(),
      plotOutput("infplot"))
      )
    )
    ))
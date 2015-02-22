
shinyServer( 
  function(input, output, session) {
    
#   output$inputValue <- renderText({
#       input$var
#   }) 
  
    refplotting <- reactive({
      if(input$tech == "I will try anything new!"){
        tt <- "EarlyAdopter"
      }else if(input$tech == "Technology Schmechnology"){
        tt <- "LateMajority"
      }else {
        tt <- "EarlyMajority"
      }
      if(input$driver == "I drive a lot of miles everyday, and I have no public transit access"){
        dd <- "FrequentDriver"
      }else if(input$driver == "I take the car to and from work mostly"){
        dd <- "AverageDriver"
      }else {
        dd <- "ModestDriver"
      }
      if(input$home == "Yes"){
        hh <- "Home L1"
      } else {
        hh <- "Neither"
      }
      if(input$work == "Yes"){
        ww <- "WorkRecharg"
      }else {
        ww <- "NoWorkRecharge"
      }
      
      a <- subset(pp, Risk == tt & Driver == dd & HomeCharging == hh & WorkCharging == ww)
      aa <- a[3:20,]
      
      techNum <- order(aa[,as.numeric(input$year)-2007],decreasing=TRUE)[1:10]
      aa[techNum,7:23]
    })  
    
    infplotting <- reactive({
      if(input$tech == "I will try anything new!"){
        tt <- "EarlyAdopter"
      }else if(input$tech == "Technology Schmechnology"){
        tt <- "LateMajority"
      }else {
        tt <- "EarlyMajority"
      }
      if(input$driver == "I drive a lot of miles everyday, and I have no public transit access"){
        dd <- "FrequentDriver"
      }else if(input$driver == "I take the car to and from work mostly"){
        dd <- "AverageDriver"
      }else {
        dd <- "ModestDriver"
      }
      if(input$home == "Yes"){
        hh <- "Home L1"
      } else {
        hh <- "Neither"
      }
      if(input$work == "Yes"){
        ww <- "WorkRecharg"
      }else {
        ww <- "NoWorkRecharge"
      }
      
      a <- subset(ii, Risk == tt & Driver == dd & HomeCharging == hh & WorkCharging == ww)
      aa <- a[3:20,]
      
      techNum <- order(aa[,as.numeric(input$year)-2007],decreasing=TRUE)[1:10]
      aa[techNum,7:23]
    })  
    
  firstCarChoice <- reactive({
    if(input$tech == "I will try anything new!"){
      tt <- "EarlyAdopter"
    }else if(input$tech == "Technology Schmechnology"){
      tt <- "LateMajority"
    }else {
      tt <- "EarlyMajority"
    }
    if(input$driver == "I drive a lot of miles everyday, and I have no public transit access"){
      dd <- "FrequentDriver"
    }else if(input$driver == "I take the car to and from work mostly"){
      dd <- "AverageDriver"
    }else {
      dd <- "ModestDriver"
    }
    if(input$home == "Yes"){
      hh <- "Home L1"
    } else {
      hh <- "Neither"
    }
    if(input$work == "Yes"){
      ww <- "WorkRecharg"
    }else {
      ww <- "NoWorkRecharge"
    }
    
    a <- subset(pp, Risk == tt & Driver == dd & HomeCharging == hh & WorkCharging == ww)
    aa <- a[3:20,]
    
    techNum <- order(aa[,as.numeric(input$year)-2007],decreasing=TRUE)[1:3]
    aa[techNum[1],7]
  })

secondCarChoice <- reactive({
  if(input$tech == "I will try anything new!"){
    tt <- "EarlyAdopter"
  }else if(input$tech == "Technology Schmechnology"){
    tt <- "LateMajority"
  }else {
    tt <- "EarlyMajority"
  }
  if(input$driver == "I drive a lot of miles everyday, and I have no public transit access"){
    dd <- "FrequentDriver"
  }else if(input$driver == "I take the car to and from work mostly"){
    dd <- "AverageDriver"
  }else {
    dd <- "ModestDriver"
  }
  if(input$home == "Yes"){
    hh <- "Home L1"
  } else {
    hh <- "Neither"
  }
  if(input$work == "Yes"){
    ww <- "WorkRecharg"
  }else {
    ww <- "NoWorkRecharge"
  }
  
  a <- subset(pp, Risk == tt & Driver == dd & HomeCharging == hh & WorkCharging == ww)
  aa <- a[3:20,]
  
  techNum <- order(aa[,as.numeric(input$year)-2007],decreasing=TRUE)[1:3]
  aa[techNum[2],7]
})

thirdCarChoice <- reactive({
  if(input$tech == "I will try anything new!"){
    tt <- "EarlyAdopter"
  }else if(input$tech == "Technology Schmechnology"){
    tt <- "LateMajority"
  }else {
    tt <- "EarlyMajority"
  }
  if(input$driver == "I drive a lot of miles everyday, and I have no public transit access"){
    dd <- "FrequentDriver"
  }else if(input$driver == "I take the car to and from work mostly"){
    dd <- "AverageDriver"
  }else {
    dd <- "ModestDriver"
  }
  if(input$home == "Yes"){
    hh <- "Home L1"
  } else {
    hh <- "Neither"
  }
  if(input$work == "Yes"){
    ww <- "WorkRecharg"
  }else {
    ww <- "NoWorkRecharge"
  }
  
  a <- subset(pp, Risk == tt & Driver == dd & HomeCharging == hh & WorkCharging == ww)
  aa <- a[3:20,]
  
  techNum <- order(aa[,as.numeric(input$year)-2007],decreasing=TRUE)[1:3]
  aa[techNum[3],7]
})

  output$refplot <- renderPlot({
    ref_value <- refplotting()
    colnames(ref_value) <- c("Tech", 2015:2030)
    melted <- melt(ref_value, id.vars="Tech")
    d <- ggplot(melted, aes(x=variable, y=value, fill=Tech)) + geom_bar(stat='identity')
    coll <- rainbow(10)
    d + theme_bw()+ scale_fill_manual(values = coll, breaks=(ref_value$Tech)) + xlab("")+ylab("Purchase Probability")+ ggtitle("Top 10 Vehicles: Business-As-Usual Case") + theme(plot.title = element_text(size = 20, face="bold")) + theme(axis.title = element_text(size = 20, face="bold"))+ theme(axis.text = element_text(size = 18, face="bold"))+ theme(axis.text.x=element_text(angle=90, vjust=1))
  })
  output$infplot <- renderPlot({
    inf_value <- infplotting()
    colnames(inf_value) <- c("Tech", 2015:2030)
    melted <- melt(inf_value, id.vars="Tech")
    d <- ggplot(melted, aes(x=variable, y=value, fill=Tech)) + geom_bar(stat='identity')
    coll <- rainbow(10)
    d + theme_bw()+ scale_fill_manual(values = coll, breaks=(inf_value$Tech)) + xlab("")+ylab("Purchase Probability")+ ggtitle("Top 10 Vehicles: Good Infrastructure Scenario") + theme(plot.title = element_text(size = 20, face="bold")) + theme(axis.title = element_text(size = 20, face="bold"))+ theme(axis.text = element_text(size = 18, face="bold"))+ theme(axis.text.x=element_text(angle=90, vjust=1))
  })
  output$prediction1 <- renderText({
    if(firstCarChoice()=="GasolineHybridCar"){
      print("The model predicted Gasoline Hybrid Car to be the first feasible choice. 
            Something like Toyota Prius, Honda Insight, Chevrolet Spark, or Ford Focus. 
            You might still have to use the dirty gasoline, but it is much better 
            to use a hybrid vehicle than a gasoline car.")
    } else if(firstCarChoice()=="BatteryElectric100Car"){
      print("The model predicted Battery Electric Car! Wow, you actually are in a good position
            to invest in an electric car at this point. Your driving miles, your love for 
            experimenting new technologies and the availability to charge at home or work, 
            they all have helped. You can invest in something like Nissan Leaf, or dare I say, Tesla?!
            Check for vehicle rebates too when you buy the car!")
    } else if(firstCarChoice()=="BatteryElectric200Car"){
      print("The model predicted Battery Electric Car with 200-mile range! Wow, you actually are in a good position
            to invest in an electric car at this point. Your driving miles, your love for 
            experimenting new technologies and the availability to charge at home or work, 
            they all have helped. You can invest in something like, dare I say, Tesla?!
            Check for vehicle rebates too when you buy the car!")
    }else if(firstCarChoice()=="Plugin10Car") {
      print("The model predicted Plugin Electric Vehicle with 10 mile range on electric drive! 
            Something like a Toyota Plug-in Prius. This car can use both electric
            and gasoline whenever needed. You get to experience the advantage of electric vehicles
            and at the same time you don't have to worry about 'running out of fuel'. Hmm, it may 
            be because you drive a lot of miles, am I right? Have you considered driving less? Or...
            you can talk to your workplace about installing more charging stations!")
    } else if(firstCarChoice()=="Plugin20Car") {
      print("The model predicted Plugin Electric Vehicle with 20 mile range on electric drive! 
            Something like a Ford C-Max Energi. This car can use both electric
            and gasoline whenever needed. This uses more electricity than the 10-mile range plugins. 
            You get to experience the advantage of electric vehicles
            and at the same time you don't have to worry about 'running out of fuel'. Hmm, it may 
            be because you drive a lot of miles, am I right? Have you considered driving less? Or...
            you can talk to your workplace about installing more charging stations!")
    } else if(firstCarChoice()=="Plugin40Car") {
      print("The model predicted Plugin Electric Vehicle with 40 mile range on electric drive! 
            Something like a Chevrolet Volt. This car can use both electric
            and gasoline whenever needed. This uses more electricity than the 10-mile and 20-mile range plugins.
            You get to experience the advantage of electric vehicles
            and at the same time you don't have to worry about 'running out of fuel'. Hmm, it may 
            be because you drive a lot of miles, am I right? Have you considered driving less? Or...
            you can talk to your workplace about installing more charging stations!")
    }
  })

  output$prediction2 <- renderText({
    if(secondCarChoice()=="GasolineHybridCar"){
      print("The model predicted Gasoline Hybrid Car to be the second feasible choice. 
            Something like Toyota Prius, Honda Insight, Chevrolet Spark, or Ford Focus. 
            You might still have to use the dirty gasoline, but it is much better 
            to use a hybrid vehicle for your second car than a gasoline car.")
    } else if(secondCarChoice()=="BatteryElectric100Car"){
      print("The model predicted Battery Electric Car with 100-mile range! Wow, you actually are in a good position
            to invest in an electric car at this point. Your driving miles, your love for 
            experimenting new technologies and the availability to charge at home or work, 
            they all have helped. You can invest in something like Nissan Leaf.
            Check for vehicle rebates too when you buy the car!")
    } else if(secondCarChoice()=="BatteryElectric200Car"){
      print("The model predicted Battery Electric Car with 200-mile range! Wow, you actually are in a good position
            to invest in an electric car at this point. You can invest in something like, dare I say, Tesla?!
            Check for vehicle rebates too when you buy the car!")
    } else if(secondCarChoice()=="Plugin10Car") {
      print("The model predicted Plugin Electric Vehicle 10 mile range! Something like a Toyota Plug-in Prius. 
            This car can use both electric and gasoline whenever needed. You get to experience the advantage of 
            electric vehicles and at the same time you don't have to worry about 'running out of fuel'. An ideal second car!")
    } else if(secondCarChoice()=="Plugin20Car") {
      print("The model predicted Plugin Electric Vehicle with 20 mile range on electric drive! 
            Something like a Ford C-Max Energi. This car can use both electric
            and gasoline whenever needed. This uses more electricity than the 10-mile range plugins. 
            You get to experience the advantage of electric vehicles
            and at the same time you don't have to worry about 'running out of fuel'.")
    }else if(secondCarChoice()=="Plugin40Car") {
      print("The model predicted Plugin Electric Vehicle with 40 mile range on electric drive! 
            Something like a Chevrolet Volt. This car can use both electric
            and gasoline whenever needed. This uses more electricity than the 10-mile and 20-mile range plugins.
            You get to experience the advantage of electric vehicles
            and at the same time you don't have to worry about 'running out of fuel'. Hmm, it may 
            be because you drive a lot of miles, am I right? Have you considered driving less? Or...
            you can talk to your workplace about installing more charging stations!")
    }
  })

  output$prediction3 <- renderText({
    if(thirdCarChoice()=="GasolineHybridCar"){
      print("The model predicted Gasoline Hybrid Car. If you could not invest in the 
            first two choices, this is a good one to choose. Something like Toyota Prius, 
            Honda Insight, Chevrolet Spark, or Ford Focus. You might still have to 
            use the dirty gasoline, but it is much better to use a hybrid vehicle than a gasoline car.")
    } else if(thirdCarChoice()=="BatteryElectric100Car"){
      print("The model predicted Battery Electric Car with 100-mile range! This is not your ideal first choice, but...
             if you change your driving miles, or modify recharging infrastructre at home or work, 
            you could easily afford this. You can invest in something like Nissan Leaf.
            Check for vehicle rebates too when you buy the car!")
    } else if(thirdCarChoice()=="BatteryElectric200Car"){
      print("The model predicted Battery Electric Car with 200-mile range! Wow, you actually are in a good position
            to invest in an electric car at this point. You can invest in something like, dare I say, Tesla?!
            Check for vehicle rebates too when you buy the car!")
    }else if(thirdCarChoice()=="Plugin10Car") {
      print("The model predicted Plugin Electric Vehicle 10 mile range! Something like a Toyota Plug-in Prius. 
            This car can use both electric and gasoline whenever needed. You get to experience the advantage of 
            electric vehicles and at the same time you don't have to worry about 'running out of fuel'.")
    } else if(thirdCarChoice()=="Plugin20Car") {
      print("The model predicted Plugin Electric Vehicle with 20 mile range on electric drive! 
            Something like a Ford C-Max Energi. This car can use both electric
            and gasoline whenever needed. This uses more electricity than the 10-mile range plugins. 
            You get to experience the advantage of electric vehicles
            and at the same time you don't have to worry about 'running out of fuel'.")
    }else if(thirdCarChoice()=="Plugin40Car") {
      print("The model predicted Plugin Electric Vehicle with 40 mile range on electric drive! 
            Something like a Chevrolet Volt. This car can use both electric
            and gasoline whenever needed. This uses more electricity than the 10-mile and 20-mile range plugins.
            You get to experience the advantage of electric vehicles
            and at the same time you don't have to worry about 'running out of fuel'. ")
    }
  })

   output$firstCarImage <- renderUI({
     filename <- paste0(firstCarChoice(),".jpg")
     tags$img(src=filename, height=300, width=400)
     })
  output$secondCarImage <- renderUI({
    filename <- paste0(secondCarChoice(),".jpg")
    tags$img(src=filename, height=300, width=400)
    })
  output$thirdCarImage <- renderUI({
    filename <- paste0(thirdCarChoice(),".jpg")
    tags$img(src=filename, height=300, width=400)
  })
#   output$mytable <- renderDataTable({
#     refTable()
#   })
 
firstCarChoiceI <- reactive({
  if(input$tech == "I will try anything new!"){
    tt <- "EarlyAdopter"
  }else if(input$tech == "Technology Schmechnology"){
    tt <- "LateMajority"
  }else {
    tt <- "EarlyMajority"
  }
  if(input$driver == "I drive a lot of miles everyday, and I have no public transit access"){
    dd <- "FrequentDriver"
  }else if(input$driver == "I take the car to and from work mostly"){
    dd <- "AverageDriver"
  }else {
    dd <- "ModestDriver"
  }
  if(input$home == "Yes"){
    hh <- "Home L1"
  } else {
    hh <- "Neither"
  }
  if(input$work == "Yes"){
    ww <- "WorkRecharg"
  }else {
    ww <- "NoWorkRecharge"
  }
  
  a <- subset(ii, Risk == tt & Driver == dd & HomeCharging == hh & WorkCharging == ww)
  aa <- a[3:20,]
  
  techNum <- order(aa[,as.numeric(input$year)-2007],decreasing=TRUE)[1:3]
  aa[techNum[1],7]
})

secondCarChoiceI <- reactive({
  if(input$tech == "I will try anything new!"){
    tt <- "EarlyAdopter"
  }else if(input$tech == "Technology Schmechnology"){
    tt <- "LateMajority"
  }else {
    tt <- "EarlyMajority"
  }
  if(input$driver == "I drive a lot of miles everyday, and I have no public transit access"){
    dd <- "FrequentDriver"
  }else if(input$driver == "I take the car to and from work mostly"){
    dd <- "AverageDriver"
  }else {
    dd <- "ModestDriver"
  }
  if(input$home == "Yes"){
    hh <- "Home L1"
  } else {
    hh <- "Neither"
  }
  if(input$work == "Yes"){
    ww <- "WorkRecharg"
  }else {
    ww <- "NoWorkRecharge"
  }
  
  a <- subset(ii, Risk == tt & Driver == dd & HomeCharging == hh & WorkCharging == ww)
  aa <- a[3:20,]
  
  techNum <- order(aa[,as.numeric(input$year)-2007],decreasing=TRUE)[1:3]
  aa[techNum[2],7]
})

thirdCarChoiceI <- reactive({
  if(input$tech == "I will try anything new!"){
    tt <- "EarlyAdopter"
  }else if(input$tech == "Technology Schmechnology"){
    tt <- "LateMajority"
  }else {
    tt <- "EarlyMajority"
  }
  if(input$driver == "I drive a lot of miles everyday, and I have no public transit access"){
    dd <- "FrequentDriver"
  }else if(input$driver == "I take the car to and from work mostly"){
    dd <- "AverageDriver"
  }else {
    dd <- "ModestDriver"
  }
  if(input$home == "Yes"){
    hh <- "Home L1"
  } else {
    hh <- "Neither"
  }
  if(input$work == "Yes"){
    ww <- "WorkRecharg"
  }else {
    ww <- "NoWorkRecharge"
  }
  
  a <- subset(ii, Risk == tt & Driver == dd & HomeCharging == hh & WorkCharging == ww)
  aa <- a[3:20,]
  
  techNum <- order(aa[,as.numeric(input$year)-2007],decreasing=TRUE)[1:3]
  aa[techNum[3],7]
})

output$predictionI1 <- renderText({
  if(firstCarChoiceI()=="GasolineHybridCar"){
    print("The model predicted Gasoline Hybrid Car to be the first feasible choice. 
          Something like Toyota Prius, Honda Insight, Chevrolet Spark, or Ford Focus. 
          You might still have to use the dirty gasoline, but it is much better 
          to use a hybrid vehicle than a gasoline car.")
  } else if(firstCarChoiceI()=="BatteryElectric100Car"){
    print("The model predicted Battery Electric Car with 100-mile range! Wow, you actually are in a good position
          to invest in an electric car at this point. Your driving miles, your love for 
          experimenting new technologies and the availability to charge at home or work, 
          they all have helped. You can invest in something like Nissan Leaf, or dare I say, Tesla?!
          Check for vehicle rebates too when you buy the car!")
  } else if(firstCarChoiceI()=="BatteryElectric200Car"){
    print("The model predicted Battery Electric Car with 200-mile range! Wow, you actually are in a good position
            to invest in an electric car at this point. You can invest in something like, dare I say, Tesla?!
          Check for vehicle rebates too when you buy the car!")
  }else if(firstCarChoiceI()=="Plugin10Car") {
    print("The model predicted Plugin Electric Vehicle with 10 mile range on electric drive! 
          Something like a Toyota Plug-in Prius. This car can use both electric
          and gasoline whenever needed. You get to experience the advantage of electric vehicles
          and at the same time you don't have to worry about 'running out of fuel'. Hmm, it may 
          be because you drive a lot of miles, am I right? Have you considered driving less? Or...
          you can talk to your workplace about installing more charging stations!")
    } else if(firstCarChoiceI()=="Plugin20Car") {
      print("The model predicted Plugin Electric Vehicle with 20 mile range on electric drive! 
            Something like a Ford C-Max Energi. This car can use both electric
            and gasoline whenever needed. This uses more electricity than the 10-mile range plugins. 
            You get to experience the advantage of electric vehicles
            and at the same time you don't have to worry about 'running out of fuel'. Hmm, it may 
            be because you drive a lot of miles, am I right? Have you considered driving less? Or...
            you can talk to your workplace about installing more charging stations!")
    } else if(firstCarChoiceI()=="Plugin40Car") {
      print("The model predicted Plugin Electric Vehicle with 40 mile range on electric drive! 
            Something like a Chevrolet Volt. This car can use both electric
            and gasoline whenever needed. This uses more electricity than the 10-mile and 20-mile range plugins.
            You get to experience the advantage of electric vehicles
            and at the same time you don't have to worry about 'running out of fuel'. Hmm, it may 
            be because you drive a lot of miles, am I right? Have you considered driving less? Or...
            you can talk to your workplace about installing more charging stations!")
    }
  })

output$predictionI2 <- renderText({
  if(secondCarChoiceI()=="GasolineHybridCar"){
    print("The model predicted Gasoline Hybrid Car to be the second feasible choice. 
          Something like Toyota Prius, Honda Insight, Chevrolet Spark, or Ford Focus. 
          You might still have to use the dirty gasoline, but it is much better 
          to use a hybrid vehicle for your second car than a gasoline car.")
  } else if(secondCarChoiceI()=="BatteryElectric100Car"){
    print("The model predicted Battery Electric Car with 100-mile range! Wow, you actually are in a good position
          to invest in an electric car at this point. Your driving miles, your love for 
          experimenting new technologies and the availability to charge at home or work, 
          they all have helped. You can invest in something like Nissan Leaf, or dare I say, Tesla?!
          Check for vehicle rebates too when you buy the car!")
  } else if(secondCarChoiceI()=="BatteryElectric200Car"){
    print("The model predicted Battery Electric Car with 200-mile range! Wow, you actually are in a good position
            to invest in an electric car at this point. You can invest in something like, dare I say, Tesla?!
          Check for vehicle rebates too when you buy the car!")
  } else if(secondCarChoiceI()=="Plugin10Car") {
    print("The model predicted Plugin Electric Vehicle 10 mile range! Something like a Toyota Plug-in Prius. 
           This car can use both electric and gasoline whenever needed. You get to experience the advantage of 
           electric vehicles and at the same time you don't have to worry about 'running out of fuel'. An ideal second car!")
  } else if(secondCarChoiceI()=="Plugin20Car") {
    print("The model predicted Plugin Electric Vehicle with 20 mile range on electric drive! 
            Something like a Ford C-Max Energi. This car can use both electric
            and gasoline whenever needed. This uses more electricity than the 10-mile range plugins. 
            You get to experience the advantage of electric vehicles
            and at the same time you don't have to worry about 'running out of fuel'.")
  }else if(secondCarChoiceI()=="Plugin40Car") {
    print("The model predicted Plugin Electric Vehicle with 40 mile range on electric drive! 
            Something like a Chevrolet Volt. This car can use both electric
            and gasoline whenever needed. This uses more electricity than the 10-mile and 20-mile range plugins.
            You get to experience the advantage of electric vehicles
            and at the same time you don't have to worry about 'running out of fuel'. Hmm, it may 
            be because you drive a lot of miles, am I right? Have you considered driving less? Or...
            you can talk to your workplace about installing more charging stations!")
  }
  })

output$predictionI3 <- renderText({
  if(thirdCarChoiceI()=="GasolineHybridCar"){
    print("The model predicted Gasoline Hybrid Car. If you could not invest in the 
          first two choices, this is a good one to choose. Something like Toyota Prius, 
          Honda Insight, Chevrolet Spark, or Ford Focus. You might still have to 
          use the dirty gasoline, but it is much better to use a hybrid vehicle than a gasoline car.")
  } else if(thirdCarChoiceI()=="BatteryElectric100Car"){
    print("The model predicted Battery Electric Car with 100-mile range! This is not your ideal first choice, but...
          if you change your driving miles, or modify recharging infrastructre at home or work, 
          you could easily afford this. You can invest in something like Nissan Leaf, or dare I say, Tesla?!
          Check for vehicle rebates too when you buy the car!")
  } else if(thirdCarChoiceI()=="BatteryElectric200Car"){
    print("The model predicted Battery Electric Car with 200-mile range! Wow, you actually are in a good position
            to invest in an electric car at this point.You can invest in something like, dare I say, Tesla?!
          Check for vehicle rebates too when you buy the car!")
    }else if(thirdCarChoiceI()=="Plugin10Car") {
      print("The model predicted Plugin Electric Vehicle 10 mile range! Something like a Toyota Plug-in Prius. 
            This car can use both electric and gasoline whenever needed. You get to experience the advantage of 
            electric vehicles and at the same time you don't have to worry about 'running out of fuel'.")
    } else if(thirdCarChoiceI()=="Plugin20Car") {
      print("The model predicted Plugin Electric Vehicle with 20 mile range on electric drive! 
            Something like a Ford C-Max Energi. This car can use both electric
            and gasoline whenever needed. This uses more electricity than the 10-mile range plugins. 
            You get to experience the advantage of electric vehicles
            and at the same time you don't have to worry about 'running out of fuel'.")
    }else if(thirdCarChoiceI()=="Plugin40Car") {
      print("The model predicted Plugin Electric Vehicle with 40 mile range on electric drive! 
            Something like a Chevrolet Volt. This car can use both electric
            and gasoline whenever needed. This uses more electricity than the 10-mile and 20-mile range plugins.
            You get to experience the advantage of electric vehicles
            and at the same time you don't have to worry about 'running out of fuel'. ")
    }
  })

   output$firstCarImageI1 <- renderUI({
     filename <- paste0(firstCarChoiceI(),".jpg")
     tags$img(src=filename, height=300, width=400)
     })
  output$secondCarImageI2 <- renderUI({
    filename <- paste0(secondCarChoiceI(),".jpg")
    tags$img(src=filename, height=300, width=400)
    })
  output$thirdCarImageI3 <- renderUI({
    filename <- paste0(thirdCarChoiceI(),".jpg")
    tags$img(src=filename, height=300, width=400)
  })
 
  }
)
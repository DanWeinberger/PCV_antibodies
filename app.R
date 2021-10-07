library(shiny)
library(readxl)
library(reshape2)
library(shiny)
library(ggplot2)

pcv7sts <- c('4','6B','9V','14','18C','19F','23F')
d1a <- read_excel("./Data/IgGGMCs2.xlsx")
d1 <- d1a[d1a$Trial!='US2',]
names(d1)[1] <- 'vax'

d1.m <- melt(d1, id.vars=c('vax','Dose','Trial','timepoint_dose_weeks'))
d1.m$variable <- gsub('serotype', '', d1.m$variable)
d1.m <- d1.m[d1.m$variable %in% pcv7sts ,]

d2 <- dcast(d1.m, vax+Dose+Trial+timepoint_dose_weeks+variable ~.)
names(d2) <- c('vax','Dose','Trial','time_dose', 'st','gmc')


shinyApp(
  
  ui = fluidPage(
    sidebarPanel(
      selectInput("vax", "Vaccine:",
                  unique(d2$vax), multiple=T, selected=unique(d2$vax)),
      checkboxGroupInput("country", "Country:",
                         unique(d2$Trial), selected=unique(d2$Trial)),
      checkboxGroupInput("doses", "Doses:",
                         unique(d2$Dose), selected=c("3rddoseP" ,"Booster")),
      checkboxGroupInput("st", "Serotypes:",
                         unique(d2$st),  selected=unique(d2$st)),
      selectInput('plot_type', 'Plot type:',
                  c('Raw Concentration','Ratio'), selected= 'Raw Concentration') 
    ),
    mainPanel(
      plotOutput("periodPlot")
    )
  ), 
  
  server = function(input, output) {
    output$periodPlot = renderPlot({
      
      if(input$plot_type=='Raw Concentration'){
        
        plot.ds <- d2[(d2$vax %in% input$vax & 
                         d2$Dose %in% input$doses & 
                         d2$st %in% input$st &
                         d2$Trial %in% input$country ) ,]
        
        p1 <- ggplot(plot.ds, aes(x=vax, y=log(gmc), group=vax, col=vax) ) +
          geom_point() +
          geom_line(aes(group = Trial),color="grey") +
          theme_classic()+
          ylab('log(GMC)') +
          facet_grid(Dose~st ) +
          theme(panel.spacing = unit(1, "lines"))
        p1
        
        
        
      }else{
        plot.ds <- d2[(d2$vax %in% input$vax & 
                         d2$Dose %in% input$doses & 
                         d2$st %in% input$st &
                         d2$Trial %in% input$country ) ,]
        plot.ds.c <- reshape2::dcast(plot.ds, Dose+Trial+st ~vax, value.var='gmc')
        plot.ds.c <- plot.ds.c[!is.na(plot.ds.c$PCV13) & !is.na(plot.ds.c$PCV7),]
        p2 <- ggplot(plot.ds.c, aes(x=st, y=log(PCV13/PCV7), group=Trial, col=Trial) ) +
          geom_point() +
          theme_classic()+
          ylab('log(GMC)') +
          geom_line(aes(group = Trial),color="grey") +
          geom_hline(yintercept=0) +
          facet_grid(Dose~. ) +
          theme(panel.spacing = unit(1, "lines"))
        p2
        
      }
      
      
    },width = "auto", height = "auto")
  }
)
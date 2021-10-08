library(shiny)
library(readxl)
library(reshape2)
library(shiny)
library(ggplot2)
library(plotly)

pcv7sts <- c('4','6B','9V','14','18C','19F','23F')
d1a <- read_excel("./Data/IgGGMCs2.xlsx")

#d1 <- d1a[d1a$Trial!='US2',]
d1 <- d1a
names(d1)[1] <- 'vax'

d1.m <- melt(d1, id.vars=c('vax','Dose','Trial','timepoint_dose_weeks'))
d1.m$variable <- gsub('serotype', '', d1.m$variable)
d1.m <- d1.m[d1.m$variable %in% pcv7sts ,]

d2 <- dcast(d1.m, vax+Dose+Trial+timepoint_dose_weeks+variable ~.)
names(d2) <- c('vax','Dose','Trial','time_dose', 'st','gmc')


shinyApp(
  
  ui = fluidPage(
    
    titlePanel("Comparison of Immmunogenicity of PCVs"),
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
      
    sidebarPanel(
      selectInput("vax", "Vaccine:",
                  unique(d2$vax), multiple=T, selected=unique(d2$vax)),
      checkboxGroupInput("country", "Country:",
                         unique(d2$Trial), selected=unique(d2$Trial)),
      checkboxGroupInput("doses", "Doses:",
                         unique(d2$Dose), selected=c("3rddoseP" ,"Booster")),
      checkboxGroupInput("st", "Serotypes:",
                         unique(d2$st),  selected=unique(d2$st)),
      selectInput("ref_vax", "Vaccine to compare against:",
                  unique(d2$vax), multiple=F, selected=unique(d2$vax)[1]),
     ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Concentration", plotlyOutput("plot_conc")),
                  tabPanel("Ratio", plotlyOutput("plot_ratio"))
      )
      )
  )
  ), 
  
  server = function(input, output) {
    output$plot_conc = renderPlotly({
      
        plot.ds <- d2[(d2$vax %in% input$vax & 
                         d2$Dose %in% input$doses & 
                         d2$st %in% input$st &
                         d2$Trial %in% input$country ) ,]
        
        p1 <-   ggplotly(
          ggplot(plot.ds, aes(x=vax, y=log(gmc), group=vax, col=vax) ) +
          geom_point() +
          ggtitle("Antibody concentration by product") +
          geom_line(aes(group = Trial),color="grey") +
          theme_classic()+
          ylab('log(GMC)') +
          facet_grid(Dose~st ) +
          theme(axis.text.x=element_text(angle=90, hjust=1)) +
          theme(panel.spacing = unit(1.5, "lines"))
        )

    })
        
  
    output$plot_ratio = renderPlotly({
      plot.ds <- d2[(d2$vax %in% input$vax & 
                         d2$Dose %in% input$doses & 
                         d2$st %in% input$st &
                         d2$Trial %in% input$country ) ,]
        plot.ds.c <- reshape2::dcast(plot.ds, Dose+Trial+st ~vax, value.var='gmc')
        
        vax.dat <- plot.ds.c[,input$vax, drop=F]
        vax.dat.ratio <- as.data.frame(apply(vax.dat,2, function(x) x/vax.dat[,input$ref_vax]))
        vax.dat.ratio <- vax.dat.ratio[, -grep(input$ref_vax, names(vax.dat.ratio) ), drop=F]
        names(vax.dat.ratio) <- paste0('Numerator ', names(vax.dat.ratio))
        
        plot.ds.c2 <- cbind.data.frame(plot.ds.c[c('Dose','Trial','st')],vax.dat.ratio)
        plot.ds.c2.m <- reshape2::melt(plot.ds.c2, id.vars=c('Dose','Trial','st'))

        p2 <- ggplotly(
          ggplot(plot.ds.c2.m, aes(x=st, y=log(value), group=Trial, col=Trial) ) +
          geom_point() +
          theme_classic()+
          ggtitle(paste0("Ratio of PCVx to ", input$ref_vax )) +
          ylab('log(Ratio)') +
          geom_line(aes(group = Trial),color="grey") +
          geom_hline(yintercept=0) +
          facet_grid(Dose~variable ) +
          theme(panel.spacing = unit(1, "lines"))
        )
        
      })
  }
)
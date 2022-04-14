library(shiny)
library(readxl)
library(reshape2)
library(shiny)
library(ggplot2)
library(plotly)
library(grid)
library(ggsci)
library(shinydashboard)



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

d2$vax <- factor(d2$vax, levels=c('PCV7','PCV13','PCV20'))
d2 <- d2[order(d2$vax),]

shinyApp(
  
  ui = dashboardPage(
    dashboardHeader(title = "Comparison of Immmunogenicity of PCVs",titleWidth=500),
    dashboardSidebar( selectInput("vax", "Vaccine:",
                                  unique(d2$vax), multiple=T, selected=unique(d2$vax)),
                      selectInput("country", "Country:",
                                         unique(d2$Trial), selected=unique(d2$Trial), multiple=T),
                      selectInput("doses", "Doses:",
                                         unique(d2$Dose), selected=c("3rddoseP")),
                      checkboxGroupInput("st", "Serotypes:",
                                         unique(d2$st),  selected=unique(d2$st)),
                      selectInput("ref_vax", "Reference vaccine:",
                                  unique(d2$vax), multiple=F, selected=unique(d2$vax)[1]),
                      selectInput("comp_vax", "Comparator vaccine",
                                  unique(d2$vax), multiple=F, selected=unique(d2$vax)[2])
                      ),
    dashboardBody(
    
      fluidRow(
        box(
          tabPanel("Concentration", plotlyOutput("plot_conc", height='800px'))
        ),
        box(
          tabPanel("Ratio", plotlyOutput("plot_ratio", height='800px'))
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
        
        vax.dat <- plot.ds.c[,names(plot.ds.c) %in% as.character(unique(d2$vax)), drop=F]
        vax.dat.ratio <- as.data.frame(apply(vax.dat,2, function(x) x/vax.dat[,input$ref_vax]))
        vax.dat.ratio <- vax.dat.ratio[, -grep(input$ref_vax, names(vax.dat.ratio) ), drop=F]
       # names(vax.dat.ratio) <- paste0('Numerator ', names(vax.dat.ratio))
        
        plot.ds.c2 <- cbind.data.frame(plot.ds.c[c('Dose','Trial','st')],vax.dat.ratio)
        plot.ds.c2.m <- reshape2::melt(plot.ds.c2, id.vars=c('Dose','Trial','st'))

        dat_text <- data.frame(
          label = c(rep('', length( unique(plot.ds.c2.m$st))-1) ,   paste0("Higher immunogenicity for ",  input$comp_vax)),
          st   = unique(plot.ds.c2.m$st)
        )
        
        dat_text2 <- data.frame(
          label = c(rep('', length( unique(plot.ds.c2.m$st))-1) ,   paste0("Higher immunogenicity for ", input$ref_vax)),
          st   = unique(plot.ds.c2.m$st)
        )
        
        
        p2 <- ggplotly(
          ggplot(plot.ds.c2.m[plot.ds.c2.m$variable==input$comp_vax,], aes(y=Trial, x=(value), col=st ) ) +
            geom_point() +
            theme_classic()+
            ggtitle(paste0("Comparison of ", input$ref_vax, ' to ', input$comp_vax)) +
            ylab('Study') +
            xlab('Ratio of Immunogenicity')+
            geom_vline(xintercept=1, lty=2, col='gray') +
            xlim(0.2, 1.7) +
            # ylim(0,NA) +
            scale_color_lancet() +
            facet_grid( rows = vars(st)) +
            theme(panel.spacing = unit(0.5, "lines"))+
            theme(legend.position="none") +
            geom_text(
              data    = dat_text,
              mapping = aes(x = 1.4, y = 2, label = label),
              hjust   = 0,
              vjust   = 0.5,
              col='gray'
            ) +
            geom_text(
              data    = dat_text2,
              mapping = aes(x = 0.5, y = 2, label = label),
              hjust   = 1,
              vjust   = 0.5,
              col='gray'
            )
          )
      })
  }
)
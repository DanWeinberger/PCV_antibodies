library(shiny)
library(readxl)
library(reshape2)
library(shiny)
library(ggplot2)
library(plotly)
library(grid)
library(ggsci)
library(shinydashboard)
library(dplyr)



pcv7sts <- c('4','6B','9V','14','18C','19F','23F')
#d1a <- read_excel("./Data/IgGGMCs2.xlsx")
d1 <- read.csv("./Data/wisspar_export.csv")

names(d1) <- gsub('outcome_overview_','',names(d1))
names(d1) <- gsub('study_eligibility_','',names(d1))
names(d1) <- gsub('clinical_trial_','',names(d1))

d1$Trial <- as.numeric(as.factor(d1$study_name))

keep.vars <- c('vaccine','dose_number','Trial','location_continent',
               'time_frame','standard_age_list','phase','assay','serotype')

d2 <- d1 %>% 
 select(all_of(c(keep.vars,'value')))

d2$vax <- factor(d2$vaccine, levels=c('PCV7',"PCV10 (Synflorix)",
                                      "PCV10 (Pneumosil)",
                                      "PCV13",
                                      'PCV15',
                                      'PCV20'))
d2 <- d2 %>%
  group_by(Trial) %>%
  mutate(total_doses=max(dose_number) ,Dose=paste0(dose_number,'/',total_doses) )


#check for duplicates
# 
# dups <- d1%>%
#   group_by(Trial, dose_number, vaccine, standard_age_list, assay, serotype) %>%
#   mutate(n_obs=n())


#audit table
# d1.m <- melt(d1[,c('vaccine','dose_number','study_name','serotype', 'assay')], id.vars=c('vaccine','dose_number','study_name','serotype','assay'))
# d1.c <- dcast(d1.m, study_name ~ dose_number+vaccine + assay  ,fun.aggregate = length)
# write.csv(d1.c,'./Data/audit.csv')

shinyApp(
  
  ui = dashboardPage(
    dashboardHeader(title = "Comparison of Immmunogenicity of PCVs",titleWidth=500),
    dashboardSidebar( selectInput("vax", "Vaccine:",

                                  unique(d2$vaccine), multiple=T, selected=unique(d2$vax)),
                      selectInput("st", "Serotypes:",multiple=T,
                                  unique(d2$serotype),  selected=c('4','14','19F','23F')),
                      selectInput("doses", "Doses:",
                                         unique(d2$Dose), selected=c("2/3"), multiple=T),
                      selectInput("age", "Age group:",
                                  unique(d2$standard_age_list), selected=c("[\"Child\"]")),
                      selectInput("phase", "Trial Phase:",
                                  unique(d2$phase), selected=c("Phase 3")),
                      selectInput("ref_vax", "Reference vaccine:",
                                  unique(d2$vax), multiple=F, selected='PCV10 (Synflorix)'),
                      selectInput("comp_vax", "Comparator vaccine",

                                  unique(d2$vax), multiple=F, selected='PCV10 (Pneumosil)'),
                      selectInput("Trial", "Trial:",
                                  unique(d2$Trial), selected=unique(d2$Trial), multiple=T)
                      
                      ),
    dashboardBody(
    
      fluidRow(
        tabBox(
          title = "",
          id = "tabset1", height = "auto", width=12,
          tabPanel("Concentration (GMC)", plotlyOutput("plot_gmc") ),
          tabPanel("Activity (OPA)", plotlyOutput("plot_opa"))
        )),
        fluidRow(
          
        box(
          tabPanel("Ratio", plotlyOutput("plot_ratio",height='600px' , inline=F)),height='600px', width=12
        ), 

        infoBox("Important information", "Data on immunogenicity alone cannot be used to infer differences in effectiveness between vaccines. These data need to be combined with information on the protective concentration of antibodies required to protect against each serotype in different populations for meaningful comparisons", icon = icon("glyphicon glyphicon-exclamation-sign",lib ='glyphicon'), width=12),
        
        ),

  )
  )
  ,
  
  
  server = function(input, output) {
    output$plot_gmc = renderPlotly({
      
        plot.ds <- d2[(d2$vaccine %in% input$vax & 
                         d2$Dose %in% input$doses & 
                         d2$serotype %in% input$st &
                         d2$Trial %in% input$Trial  &
                        d2$standard_age_list %in% input$age  &
                        d2$phase %in% input$phase)
                        ,]
        
        p1 <-   ggplotly(
          ggplot(plot.ds[plot.ds$assay=='IgG',], aes(x=vax, y=log(value), group=vax, col=vax) ) +
          geom_point() +
          ggtitle("Antibody concentration (GMC) by product") +
          geom_line(aes(group = Trial),color="grey") +
          theme_classic()+
          ylab('log(GMC)') +
          facet_grid(Dose~serotype ) +
          theme(axis.text.x=element_text(angle=90, hjust=1)) +
          theme(panel.spacing = unit(1.5, "lines"))
        )

    })
    
    #OPA

    output$plot_opa = renderPlotly({
      
      plot.ds <- d2[(d2$vaccine %in% input$vax & 
                       d2$Dose %in% input$doses & 
                       d2$serotype %in% input$st &
                       d2$Trial %in% input$Trial  &
                       d2$standard_age_list %in% input$age  &
                       d2$phase %in% input$phase) 
                      
                    ,]
      
      p2 <-   ggplotly(
        ggplot(plot.ds[plot.ds$assay=='OPA',], aes(x=vax, y=log(value), group=vax, col=vax) ) +
          geom_point() +
          ggtitle("Functional antibody (OPA) by product") +
          geom_line(aes(group = Trial),color="grey") +
          theme_classic()+
          ylab('log(GMC)') +
          facet_grid( ~serotype ) +
          theme(axis.text.x=element_text(angle=90, hjust=1)) +
          theme(panel.spacing = unit(1.5, "lines"))
      )
      
    })
    
  
    output$plot_ratio = renderPlotly({
      plot.ds <- d2[(d2$vaccine %in% input$vax & 
                       d2$Dose %in% input$doses & 
                       d2$serotype %in% input$st &
                       d2$Trial %in% input$Trial  &
                       d2$standard_age_list %in% input$age  &
                       d2$phase %in% input$phase)
                    ,]
        plot.ds.c <- reshape2::dcast(plot.ds, Dose+Trial+serotype +assay~vaccine, value.var='value')
        
        vax.dat <- plot.ds.c[,names(plot.ds.c) %in% as.character(unique(d2$vaccine)), drop=F]
        
        vax.dat.ratio <- as.data.frame(apply(vax.dat,2, function(x) x/vax.dat[,input$ref_vax]))
        
        vax.dat.ratio <- vax.dat.ratio[, -which(ref_vax==names(vax.dat.ratio) ), drop=F]
        
        # names(vax.dat.ratio) <- paste0('Numerator ', names(vax.dat.ratio))
        
        plot.ds.c2 <- cbind.data.frame(plot.ds.c[c('Dose','Trial','serotype','assay')],vax.dat.ratio)
        plot.ds.c2.m <- reshape2::melt(plot.ds.c2, id.vars=c('Dose','Trial','serotype','assay'))
        
        plot.df <- plot.ds.c2.m[plot.ds.c2.m$variable==input$comp_vax & plot.ds.c2.m$assay=='IgG',]
        
        plot.df$Trial <- as.numeric( as.factor(plot.df$Trial))
        
        plot.df <- plot.df[!is.na(plot.df$value),]
        
        dat_text <- data.frame(
          label = c(rep('', length( unique(plot.df$serotype))-1) ,   paste0("Higher immunogenicity for ",  input$comp_vax)),
          serotype   = unique(plot.df$serotype)
        )
        
        dat_text2 <- data.frame(
          label = c(rep('', length( unique(plot.df$serotype))-1) ,   paste0("Higher immunogenicity for ", input$ref_vax)),
          serotype   = unique(plot.df$serotype)
        )
        
                p2 <- ggplotly(
          ggplot(plot.df, aes(y=Trial, x=(value), col=serotype ) ) +
            geom_point() +
            theme_classic()+
            ggtitle(paste0("Comparison of ", input$ref_vax, ' to ', input$comp_vax)) +
            ylab('Study') +
            xlab('Ratio of Immunogenicity')+
            geom_vline(xintercept=1, lty=2, col='gray') +
            xlim(0.2, 1.7) +
            # ylim(0,NA) +
            scale_color_lancet() +
            facet_grid( rows = vars(serotype)) +
            theme(panel.spacing = unit(0.5, "lines"))+
            theme(legend.position="none") +
            geom_text(
              data    = dat_text,
              mapping = aes(x = 1.4, y = 1.5, label = label),
              hjust   = 0,
              vjust   = 0.5,
              col='gray'
            ) +
            geom_text(
              data    = dat_text2,
              mapping = aes(x = 0.5, y = 1.5, label = label),
              hjust   = 1,
              vjust   = 0.5,
              col='gray'

            )+
            theme(axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
        )
      })
  }
)
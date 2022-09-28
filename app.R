#TODO: STOP temporary error message

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
library(scales)

scaleFUN <- function(x) sprintf("%.2f", x)

#https://stackoverflow.com/questions/34929206/selectinput-that-is-dependent-on-another-selectinput


pcv7sts <- c('4','6B','9V','14','18C','19F','23F')
#d1a <- read_excel("./Data/IgGGMCs2.xlsx")
d1 <- read.csv("./Data/wisspar_export_CIs.csv")


#d1 <- read.csv("https://wisspar.com/export-options/data-export/?use_case=pcv_antibodies&default=true&clinical_trial_sponsor=true")
#write.csv(d1,"./Data/wisspar_export_CIs.csv")

names(d1) <- gsub('outcome_overview_','',names(d1))
names(d1) <- gsub('study_eligibility_','',names(d1))
names(d1) <- gsub('clinical_trial_','',names(d1))

d1$sponsor[d1$sponsor=="Wyeth is now a wholly owned subsidiary of Pfizer"] <- "Pfizer"
d1$sponsor <- as.factor(d1$sponsor)

d1$study_age <- paste0(d1$study_id, d1$standard_age_list)
keep.vars <- c('vaccine','dose_number','study_id','location_continent',
               'time_frame','study_age','standard_age_list','phase','sponsor','assay','serotype','time_frame_weeks', 'dose_description','schedule','upper_limit','lower_limit')


d2 <- d1 %>% 
 select(all_of(c(keep.vars,'value')))

d2$vax <- factor(d2$vaccine, levels=c('PCV7', "PCV13",
                                      'PCV15',"PCV10 (Synflorix)",
                                      "PCV10 (Pneumosil)",
                                     'PCV20'))

#d2$serotype <- as.factor(d2$serotype)

d2$assay[d2$assay=='IgG'] <- 'GMC'

d2$dose_description[d2$dose_description=='1m post primary series child'] <- '1m post primary child' 
d2$LogResponse= round(log(d2$value),2)
d2$Response= round((d2$value),2)

d2$dose_descr_sponsor <- paste(d2$dose_description, ', Sponsor:',d2$sponsor,', ', d2$study_id)

pediatric.schedules <- unique(d2$schedule)[grep('child', unique(d2$schedule))]
adult.schedules <- unique(d2$schedule)[grep('adult', unique(d2$schedule))]
schedule.list <- list(adult.schedules,pediatric.schedules)

default.dose.options <- c('1m post primary child' ,'1m post dose 1 adult')

# table(d2$dose_descr)
# table(d2$time_frame[is.na(d2$dose_descr)])

#check for duplicates
# 
# dups <- d1%>%

#   group_by(study_id, dose_number, time_frame_weeks,vaccine, standard_age_list, assay, serotype) %>%
#   mutate(n_obs=n())


#audit table
 d1.m <- melt(d1[,c('vaccine','dose_description','time_frame_weeks','study_id','serotype', 'assay')], id.vars=c('vaccine','dose_description','study_id','serotype','assay'))
 d1.c <- dcast(d1.m, study_id ~ dose_description+vaccine + assay  ,fun.aggregate = length)
 write.csv(d1.c,'./Data/audit.csv')

shinyApp(
  
  ui = dashboardPage(
    dashboardHeader(title = "Comparison of Immmunogenicity of PCVs",titleWidth=500),
    dashboardSidebar( selectInput("vax", "Vaccine:",
                                  unique(d2$vaccine), multiple=T, selected=c('PCV7',"PCV10 (Pneumosil)","PCV10 (Synflorix)", 'PCV13','PCV15','PCV20')),
                      selectInput("st", "Serotypes:",multiple=T,
                                  unique(d2$serotype),  selected=c('4','6A','14','19F')),
                    
                      selectizeInput("age", "Child or adults:",
                                c('Child','Adult'), selected=c("Child")),
                      uiOutput('fine_age'),
                      uiOutput("schedule"),
                      uiOutput("dose_description"),
                      
                      selectInput("phase", "Trial Phase:",
                                  unique(d2$phase), selected=unique(d2$phase), multiple=T),
                      uiOutput("ref_vax"),
                      uiOutput("comp_vax"),
                      uiOutput("sponsor_name"),
                      uiOutput("study_id")
                      
                      ),
    dashboardBody(
    
      fluidRow(
        tabBox(
          title = "",
          id = "tabset1", height = "auto", width=12,
          tabPanel(title="Concentration (GMC)",
                   tabBox( title="", id='tabset1a',height='auto',width=12,
                     tabPanel(title='ELISA',
                       plotlyOutput("plot_gmc_elisa" )),
                     tabPanel(title='ECL',
                              plotlyOutput("plot_gmc_ecl")   ))
                   ),
          tabPanel("Activity (OPA)", plotlyOutput("plot_opa")),
          tabPanel("GMC Ratio", plotlyOutput("plot_ratio", inline=F)),
          tabPanel("OPA Ratio", plotlyOutput("plot_ratio_opa", inline=F))
          
        )),
       
        infoBox("Important information", "Data on immunogenicity alone cannot be used to infer 
        differences in effectiveness between vaccines. 
        These data need to be combined with information on the protective concentration of antibodies required to protect against each serotype in 
        different populations for meaningful comparisons. Caution should be used when comparing
                data from trials conducted by different sponsors, which might use differents assays", icon = icon("glyphicon glyphicon-exclamation-sign",lib ='glyphicon'), width=12)
        
        

  )
  )
  ,
  
  server = function(input, output,session) {
    

    output$schedule <- renderUI({
      
      if(any(grep('Child', input$fine_age))){
        sched.options<-unique(d2$schedule)[grep('child',unique(d2$schedule))]
      }else{
        sched.options<- unique(d2$schedule)[grep('adult',unique(d2$schedule))]
      }
       selectizeInput(
        inputId = 'schedule',
        label = 'Schedule:',
        choices = sched.options,
        multiple = TRUE,
        selected = sched.options[1])
    })
    
    output$fine_age <- renderUI({
      if( input$age=='Adult'){
        age.options<- unique(d2$standard_age_list)[grep('Adult',unique(d2$standard_age_list))]
      }else{
        age.options<- unique(d2$standard_age_list)[grep('Child',unique(d2$standard_age_list))]
       }
    selectizeInput(
      inputId = 'fine_age',
      label = 'Finer age categories:',
      choices = age.options,
      multiple = TRUE,
      selected = age.options[1])
  })
   
    
    output$dose_description <- renderUI({
      if(any(grep('Child', input$fine_age))){
       dose.options <- unique(d2$dose_description)[grep('child',unique(d2$dose_description))]
       dose.default.select <- default.dose.options[grep('child',default.dose.options)]
      }else{
        dose.options <- unique(d2$dose_description)[grep('adult',unique(d2$dose_description))]
        dose.default.select <- default.dose.options[grep('adult',default.dose.options)]

      }
      selectizeInput(
        inputId = 'dose_description',
        label = 'Doses received and timing:',
        choices = dose.options,
        multiple = TRUE,
        selected = dose.default.select) #'1m post primary child' ,'1m post dose 1 adult'
      })
    

    
    output$ref_vax <-renderUI({
      plot.ds <- d2[(d2$vaccine %in% input$vax & 
                       d2$dose_description %in% input$dose_description & 
                       d2$standard_age_list %in% input$fine_age  &
                       d2$phase %in% input$phase)   ,]
        selectInput("ref_vax", "Reference vaccine:",
                  input$vax, multiple=F, selected='PCV7')    
      })
    
    output$comp_vax <-renderUI({
      plot.ds <- d2[(d2$vaccine %in% input$vax & 
                       d2$dose_description %in% input$dose_description & 
                       d2$standard_age_list %in% input$fine_age  &
                       d2$phase %in% input$phase)   ,]
      selectInput("comp_vax", "Comparator vaccine",
                  input$vax, multiple=F, selected='PCV13')   
    })

    output$sponsor_name <-renderUI({
      plot.ds <- d2[(d2$vaccine %in% input$vax & 
                       d2$dose_description %in% input$dose_description & 
                       d2$standard_age_list %in% input$fine_age  &
                       d2$phase %in% input$phase)   ,]
      selectInput("sponsor", "Sponsor:",
                  unique(plot.ds$sponsor), selected=unique(plot.ds$sponsor), multiple=T)
    })
    
    output$study_id <-renderUI({
      plot.ds <- d2[(d2$vaccine %in% input$vax & 
                       d2$dose_description %in% input$dose_description & 
                       d2$standard_age_list %in% input$fine_age  &
                       d2$phase %in% input$phase)   ,]
      selectInput("study_id", "Trial:",
                  unique(plot.ds$study_id), selected=unique(plot.ds$study_id), multiple=T)
    })
      
    #add to UI: uiOutput("secondSelection")
    
    plot.ds.gmc_elisa <- reactive({
   
       d2 %>% filter(vaccine %in% input$vax & 
            dose_description %in% input$dose_description & 
            standard_age_list %in% input$fine_age  &
            phase %in% input$phase   &
           serotype %in% input$st &
           assay=='GMC' & 
          sponsor %in% input$sponsor & sponsor != "Merck Sharp & Dohme LLC")
      })
    
    plot.ds.gmc <- reactive({
      
      d2 %>% filter(vaccine %in% input$vax & 
                      dose_description %in% input$dose_description & 
                      standard_age_list %in% input$fine_age  &
                      phase %in% input$phase   &
                      serotype %in% input$st &
                      assay=='GMC' & 
                      sponsor %in% input$sponsor )
    })
    
    plot.ds.gmc_ecl <- reactive({
      
      d2 %>% filter(vaccine %in% input$vax & 
                      dose_description %in% input$dose_description & 
                      standard_age_list %in% input$fine_age  &
                      phase %in% input$phase   &
                      serotype %in% input$st &
                      assay=='GMC' & 
                      sponsor %in% input$sponsor & sponsor == "Merck Sharp & Dohme LLC")
    })
    
    output$plot_gmc_elisa = renderPlotly({
      validate(
        need(nrow(plot.ds.gmc_elisa()) > 0, message = FALSE)
      )
      if(is.null(input$dose_description)){
        
        p1 <- ggplot()
        p1
        #ggplotly(p1)
        
      }else{
        p1 <- plot.ds.gmc_elisa() %>% 
          filter(sponsor != "Merck Sharp & Dohme LLC") %>%
          ggplot( aes(x=vax, y=Response,  
                                          text=dose_descr_sponsor,
                                          #shape=sponsor,
                                          col=vax))  +
          geom_point() +
           scale_y_continuous(
             trans = "log",labels=scaleFUN) +
           geom_errorbar(data=plot.ds.gmc_elisa(), aes(ymin=(lower_limit), ymax=(upper_limit), color=vax, width=0)) +
           ggtitle("Antibody concentration (GMC) by product") +
           geom_line(aes(group = study_id),color="grey") +
           theme_classic()+
           ylab('GMC') +
           geom_hline(yintercept=(0.35), lty=2, col='gray')+
         # # ylim(0,NA) +
           facet_grid(dose_description~serotype ) +
           theme(axis.text.x=element_text(angle=90, hjust=1)) +
           theme(panel.spacing = unit(1.5, "lines"))
        
        ggplotly(p1)
        p1
      }
    })
    
    #ECL
    output$plot_gmc_ecl= renderPlotly({
      validate(
        need(nrow(plot.ds.gmc_ecl()) > 0, message = FALSE)
      )
      if(is.null(input$dose_description)){
        
        p1 <- ggplot()
        p1
        #ggplotly(p1)
        
      }else{
        p1 <- plot.ds.gmc_ecl() %>% 
          filter(sponsor== "Merck Sharp & Dohme LLC") %>%
          ggplot( aes(x=vax, y=Response,  
                      text=dose_descr_sponsor,
                      #shape=sponsor,
                      col=vax))  +
          geom_point() +
          scale_y_continuous(
            trans = "log",labels=scaleFUN) +
          geom_errorbar(data=plot.ds.gmc_ecl(), aes(ymin=(lower_limit), ymax=(upper_limit), color=vax, width=0)) +
          ggtitle("Antibody concentration (GMC) by product") +
          geom_line(aes(group = study_id),color="grey") +
          theme_classic()+
          ylab('GMC') +
          geom_hline(yintercept=(0.35), lty=2, col='gray')+
          # # ylim(0,NA) +
          facet_grid(dose_description~serotype ) +
          theme(axis.text.x=element_text(angle=90, hjust=1)) +
          theme(panel.spacing = unit(1.5, "lines"))
        
        ggplotly(p1)
        p1
      }
    })
    
    #OPA

    output$plot_opa = renderPlotly({
      validate(
        need(nrow(plot.ds.gmc()) > 0, message = FALSE)
      )
      if(is.null(input$dose_description)){
        p2 <- ggplot()
        p2
        #ggplotly(p2)
      }else{
      plot.ds <- d2[(d2$vaccine %in% input$vax & 
                       d2$dose_description %in% input$dose_description & 
                       d2$serotype %in% input$st &
                       d2$study_id %in% input$study_id  &
                       d2$standard_age_list %in% input$fine_age  &
                       d2$phase %in% input$phase & 
                       d2$sponsor %in% input$sponsor) 
                      
                    ,]
      
      plot.ds$study_id <- factor(plot.ds$study_id)
      
      p2 <-   ggplot(plot.ds[plot.ds$assay=='OPA',], aes(x=vax, 
                                                         y=Response, 
                                                         group=study_age, 
                                                         text=dose_descr_sponsor,
                                                        # shape=sponsor,
                                                         col=vax) ) +
          geom_point() +
          ggtitle("Functional antibody (OPA) by product") +
          geom_line(aes(group = study_age),color="grey") +
          theme_classic()+
          ylab('OPA GMT') +
            scale_y_continuous(
              trans = "log",labels=scaleFUN) +
            facet_grid( dose_description~serotype ) +
          theme(axis.text.x=element_text(angle=90, hjust=1)) +
          theme(panel.spacing = unit(1.5, "lines"))
     ggplotly(p2)
      p2
      } 
      
    })
    
  
    output$plot_ratio = renderPlotly({
      validate(
        need(nrow(plot.ds.gmc()) > 0, message = FALSE)
      )
      if(is.null(input$dose_description)){
        p1 <- ggplot()
        ggplotly(p1)
        
      }else{
      plot.ds <- d2[(d2$vaccine %in% input$vax & 
                       d2$dose_description %in% input$dose_description & 
                       d2$serotype %in% input$st &
                       d2$study_id %in% input$study_id  &
                       d2$standard_age_list %in% input$fine_age  &
                       d2$phase %in% input$phase & 
                       d2$sponsor %in% input$sponsor)
                    ,]
        plot.ds$study_id <- factor(plot.ds$study_id)
      
        plot.ds.c <- reshape2::dcast(plot.ds, dose_description+schedule+study_id+serotype +assay~vaccine, value.var='value', fun.aggregate = mean)
        
        vax.dat <- plot.ds.c[,names(plot.ds.c) %in% as.character(unique(d2$vaccine)), drop=F]
        
        vax.dat.ratio <- as.data.frame(apply(vax.dat,2, function(x) x/vax.dat[,input$ref_vax]))
        
        vax.dat.ratio <- vax.dat.ratio[, -which(input$ref_vax==names(vax.dat.ratio) ), drop=F]
        
        # names(vax.dat.ratio) <- paste0('Numerator ', names(vax.dat.ratio))
        
        plot.ds.c2 <- cbind.data.frame(plot.ds.c[c('dose_description','study_id','serotype','assay')],vax.dat.ratio)
        plot.ds.c2.m <- reshape2::melt(plot.ds.c2, id.vars=c('dose_description','study_id','serotype','assay'))
        
        plot.df <- plot.ds.c2.m[plot.ds.c2.m$variable==input$comp_vax & plot.ds.c2.m$assay=='GMC',]
        
        plot.df$study_id <-  as.factor(plot.df$study_id)
        
        plot.df <- plot.df[!is.na(plot.df$value),]
        plot.df$Ratio <- round(plot.df$value,2)
        
        dat_text <- data.frame(
          label = c(rep('', length( unique(plot.df$serotype))-1) ,   paste0("Higher immunogenicity for ",  input$comp_vax)),
          serotype   = unique(plot.df$serotype)
        )
        
        dat_text2 <- data.frame(
          label = c(rep('', length( unique(plot.df$serotype))-1) ,   paste0("Higher immunogenicity for ", input$ref_vax)),
          serotype   = unique(plot.df$serotype)
        )
        
                p2 <- ggplotly(
          ggplot(plot.df, aes(y=study_id, x=Ratio, col=serotype ) ) +
            geom_point(aes(shape=dose_description)) +
            theme_classic()+
          #  scale_x_continuous(
             # trans = "log",labels=scaleFUN) +
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
      }
      })

  output$plot_ratio_opa = renderPlotly({
    validate(
      need(nrow(plot.ds.gmc()) > 0, message = FALSE)
    )
    if(is.null(input$dose_description)){
      p1 <- ggplot()
      ggplotly(p1)
      
    }else{
      plot.ds <- d2[(d2$vaccine %in% input$vax & 
                       d2$dose_description %in% input$dose_description & 
                       d2$serotype %in% input$st &
                       d2$study_id %in% input$study_id  &
                       d2$standard_age_list %in% input$fine_age  &
                       d2$phase %in% input$phase & 
                       d2$sponsor %in% input$sponsor)
                    ,]
      plot.ds$study_id <- factor(plot.ds$study_id)
      
      plot.ds.c <- reshape2::dcast(plot.ds, dose_description+schedule+study_id+serotype +assay~vaccine, value.var='value', fun.aggregate = mean)
      
      vax.dat <- plot.ds.c[,names(plot.ds.c) %in% as.character(unique(d2$vaccine)), drop=F]
      
      vax.dat.ratio <- as.data.frame(apply(vax.dat,2, function(x) x/vax.dat[,input$ref_vax]))
      
      vax.dat.ratio <- vax.dat.ratio[, -which(input$ref_vax==names(vax.dat.ratio) ), drop=F]
      
      # names(vax.dat.ratio) <- paste0('Numerator ', names(vax.dat.ratio))
      
      plot.ds.c2 <- cbind.data.frame(plot.ds.c[c('dose_description','study_id','serotype','assay')],vax.dat.ratio)
      plot.ds.c2.m <- reshape2::melt(plot.ds.c2, id.vars=c('dose_description','study_id','serotype','assay'))
      
      plot.df <- plot.ds.c2.m[plot.ds.c2.m$variable==input$comp_vax & plot.ds.c2.m$assay=='OPA',]
      
      plot.df$study_id <-  as.factor(plot.df$study_id)
      
      plot.df <- plot.df[!is.na(plot.df$value),]
      plot.df$Ratio <- round(plot.df$value,2)
      
      dat_text <- data.frame(
        label = c(rep('', length( unique(plot.df$serotype))-1) ,   paste0("Higher immunogenicity for ",  input$comp_vax)),
        serotype   = unique(plot.df$serotype)
      )
      
      dat_text2 <- data.frame(
        label = c(rep('', length( unique(plot.df$serotype))-1) ,   paste0("Higher immunogenicity for ", input$ref_vax)),
        serotype   = unique(plot.df$serotype)
      )
      
      p2 <- ggplotly(
        ggplot(plot.df, aes(y=study_id, x=Ratio, col=serotype ) ) +
          geom_point(aes(shape=dose_description)) +
          theme_classic()+
          #  scale_x_continuous(
          # trans = "log",labels=scaleFUN) +
          ggtitle(paste0("Comparison of ", input$ref_vax, ' to ', input$comp_vax)) +
          ylab('Study') +
          xlab('Ratio of OPA GMT')+
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
    }
  })
  }

)

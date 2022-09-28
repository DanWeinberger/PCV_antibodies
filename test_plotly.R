plot.ds <- d2[(d2$vaccine %in% unique(d2$vaccine) & 
                 d2$dose_description %in% c("1m post primary child") & 
                 d2$serotype %in% c('4','14','19F','23F') &
                 d2$study_id %in% unique(d2$study_id)  &
                 d2$standard_age_list %in% "[\"Child\"]"  &
                 d2$phase %in% "Phase 3") &
                d2$assay=='GMC'
              ,] 

p1 <-   ggplot(plot.ds, aes(x=vax, y=Response  
                                  ))  +
  geom_point(aes(col=vax)) +
  aes(shape=sponsor) +
  geom_line(color='gray') +
  aes(group = study_id)+
  scale_y_continuous(
    trans = "log",labels=scaleFUN) +
  #geom_errorbar(data=plot.ds.gmc(), aes(ymin=(lower_limit), ymax=(upper_limit), color=vax, width=0)) +
  ggtitle("Antibody concentration (GMC) by product") +
  theme_classic()+
  ylab('GMC') +
  geom_hline(yintercept=(0.35), lty=2, col='gray')+
  # # ylim(0,NA) +
  facet_grid(dose_description~serotype ) +
  theme(axis.text.x=element_text(angle=90, hjust=1)) +
  theme(panel.spacing = unit(1.5, "lines")) +
  guides(shape = "none")

p1
p2 <- ggplotly(p1)

p3 <- p2 %>% style(p2, showlegend = FALSE, traces = 1:6)

# 
# plot.ds <- d2[(d2$vaccine %in% unique(d2$vaccine) & 
#                  d2$Dose %in% c("2/3") & 
#                  d2$serotype %in% c('4','14','19F','23F') &
#                  d2$study_id %in% unique(d2$study_id)  &
#                  d2$standard_age_list %in% "[\"Child\"]"  &
#                  d2$phase %in% "Phase 3")
#               ,] 


plot.ds <- d2[(d2$vaccine %in% unique(d2$vaccine) &
               #  d2$Dose %in% c("2/3") &
                 d2$serotype %in% c('4','14','19F','23F') &
                 d2$study_id %in% unique(d2$study_id)  &
                 d2$standard_age_list %in% "[\"Older Adult\"]"  &
                 d2$phase %in% "Phase 3")
              ,]

ref_vax='PCV13'
comp_vax='PCV20'


plot.ds$study_id <- factor(plot.ds$study_id)
p1 <-   ggplotly(
  ggplot(plot.ds[plot.ds$assay=='GMC',], aes(x=vax, y=log(value), group=vax, col=vax) ) +
    geom_point() +
    ggtitle("Antibody concentration (GMC) by product") +
    # geom_line(aes(group = study_id),color="grey") +
    theme_classic()+
    ylab('log(GMC)') +
    #facet_grid(Dose~serotype ) +
    theme(axis.text.x=element_text(angle=90, hjust=1)) +
    theme(panel.spacing = unit(1.5, "lines"))
)
p2 <-   ggplotly(
  ggplot(plot.ds[plot.ds$assay=='OPA',], aes(x=vax, y=log(value), group=vax, col=vax) ) +
    geom_point() +
    ggtitle("Functional antibody (OPA) by product") +
     geom_line(aes(group = study_id),color="grey") +
    theme_classic()+
    ylab('log(GMC)') +
     facet_grid( ~serotype ) +
    theme(axis.text.x=element_text(angle=90, hjust=1)) +
    theme(panel.spacing = unit(1.5, "lines"))
)


p1 <-   ggplotly(
  ggplot(plot.ds[plot.ds$assay=='IgG',], aes(x=vax, y=log(value), group=vax, col=vax) ) +
    geom_point() +
    ggtitle("Antibody concentration (GMC) by product") +
    geom_line(aes(group = study_id),color="grey") +
    theme_classic()+
    ylab('log(GMC)') +
    facet_grid(Dose~serotype ) +
    theme(axis.text.x=element_text(angle=90, hjust=1)) +
    theme(panel.spacing = unit(1.5, "lines"))
)

p2 <-   ggplotly(
  ggplot(plot.ds[plot.ds$assay=='OPA',], aes(x=vax, y=log(value), group=vax, col=vax) ) +
    geom_point() +
    ggtitle("Functional antibody (OPA) by product") +
    geom_line(aes(group = study_id),color="grey") +
    theme_classic()+
    ylab('log(GMC)') +
    facet_grid( ~serotype ) +
    theme(axis.text.x=element_text(angle=90, hjust=1)) +
    theme(panel.spacing = unit(1.5, "lines"))
)


plot.ds.c <- reshape2::dcast(plot.ds, Dose+study_id+serotype +assay~vaccine, value.var='value')

vax.dat <- plot.ds.c[,names(plot.ds.c) %in% as.character(unique(d2$vaccine)), drop=F]
vax.dat.ratio <- as.data.frame(apply(vax.dat,2, function(x) x/vax.dat[,ref_vax]))
vax.dat.ratio <- vax.dat.ratio[, -which(ref_vax==names(vax.dat.ratio) ), drop=F]

# names(vax.dat.ratio) <- paste0('Numerator ', names(vax.dat.ratio))

plot.ds.c2 <- cbind.data.frame(plot.ds.c[c('Dose','study_id','serotype','assay')],vax.dat.ratio)
plot.ds.c2.m <- reshape2::melt(plot.ds.c2, id.vars=c('Dose','study_id','serotype','assay'))

plot.df <- plot.ds.c2.m[plot.ds.c2.m$variable==comp_vax & plot.ds.c2.m$assay=='IgG',]

plot.df$study_id <- as.numeric( as.factor(plot.df$study_id))

plot.df <- plot.df[!is.na(plot.df$value),]

dat_text <- data.frame(
  label = c(rep('', length( unique(plot.df$serotype))-1) ,   paste0("Higher immunogenicity for ",  comp_vax)),
  serotype   = unique(plot.df$serotype)
)

dat_text2 <- data.frame(
  label = c(rep('', length( unique(plot.df$serotype))-1) ,   paste0("Higher immunogenicity for ", ref_vax)),
  serotype   = unique(plot.df$serotype)
)

p2 <- ggplotly(
  ggplot(plot.df, aes(y=study_id, x=(value), col=serotype ) ) +
    geom_point() +
    theme_classic()+
    ggtitle(paste0("Comparison of ", ref_vax, ' to ', comp_vax)) +
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

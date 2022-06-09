plot.ds <- d2[(d2$vax %in%  unique(d2$vax) & 
                 d2$Dose %in% c("3/4" ) & 
                 d2$serotype %in%  unique(d2$serotype) &
                 d2$Trial %in% unique(d2$Trial) )&
                d2$phase =='Phase 3',]

ref_vax='PCV10 (Pneumosil)'
comp_vax='PCV10 (Synflorix)'


plot.ds.c <- reshape2::dcast(plot.ds, Dose+Trial+serotype +assay~vaccine, value.var='value')
plot.ds.c <- reshape2::dcast(plot.ds, Dose+Trial+serotype +assay~vaccine, value.var='value')

vax.dat <- plot.ds.c[,names(plot.ds.c) %in% as.character(unique(d2$vaccine)), drop=F]
vax.dat.ratio <- as.data.frame(apply(vax.dat,2, function(x) x/vax.dat[,ref_vax]))
vax.dat.ratio <- vax.dat.ratio[, -which(ref_vax==names(vax.dat.ratio) ), drop=F]

# names(vax.dat.ratio) <- paste0('Numerator ', names(vax.dat.ratio))

plot.ds.c2 <- cbind.data.frame(plot.ds.c[c('Dose','Trial','serotype','assay')],vax.dat.ratio)
plot.ds.c2.m <- reshape2::melt(plot.ds.c2, id.vars=c('Dose','Trial','serotype','assay'))

plot.df <- plot.ds.c2.m[plot.ds.c2.m$variable==comp_vax & plot.ds.c2.m$assay=='IgG',]

plot.df$Trial <- as.numeric( as.factor(plot.df$Trial))

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
  ggplot(plot.df, aes(y=Trial, x=(value), col=serotype ) ) +
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
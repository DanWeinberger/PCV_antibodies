plot.ds <- d2[(d2$vax %in%  unique(d2$vax) & 
                 d2$Dose %in% c("3rddoseP" ) & 
                 d2$st %in%  unique(d2$st) &
                 d2$Trial %in% unique(d2$Trial) ) ,]

ref.vax='PCV7'

plot.ds.c <- reshape2::dcast(plot.ds, Dose+Trial+st ~vax, value.var='gmc')
vax.dat <- plot.ds.c[,unique(d2$vax), drop=F]
vax.dat.ratio <- as.data.frame(apply(vax.dat,2, function(x) x/vax.dat[,ref.vax]))
vax.dat.ratio <- vax.dat.ratio[, -grep(ref.vax, names(vax.dat.ratio) ), drop=F]
#names(vax.dat.ratio) <- paste0('Comparator ', names(vax.dat.ratio))



plot.ds.c2 <- cbind.data.frame(plot.ds.c[c('Dose','Trial','st')],vax.dat.ratio)
plot.ds.c2.m <- reshape2::melt(plot.ds.c2, id.vars=c('Dose','Trial','st'))



dat_text <- data.frame(
  label = c(rep('', length( unique(plot.ds.c2.m$st))-1) ,   paste0("Higher immunogenicity for ", 'PCV13')),
  st   = unique(plot.ds.c2.m$st)
)

dat_text2 <- data.frame(
  label = c(rep('', length( unique(plot.ds.c2.m$st))-1) ,   paste0("Higher immunogenicity for ", ref.vax)),
  st   = unique(plot.ds.c2.m$st)
)


p2 <- ggplotly(
  ggplot(plot.ds.c2.m[plot.ds.c2.m$variable=='PCV13',], aes(y=Trial, x=(value), col=st ) ) +
    geom_point() +
    theme_classic()+
    ggtitle(paste0("Comparison of ", ref.vax, ' to PCV13')) +
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

p2

# +
#   annotate(geom="text", x=0.2, y=0, label=lab1,
#            color="red")

  # +geom_text(data=annotations, aes(x=X,y=Y,hjust=x_adjust,vjust=y_adjust,label=lab), col='black') 

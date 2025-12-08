## Graph it
IRI_Wave1_totals<-IRI_Wave1_outdf %>%
  select(tagid,IRI_PT_total,IRI_EC_total,IRI_PD_total) %>%
  mutate(Perspective_Taking=IRI_PT_total,
         Empathic_Concern=IRI_EC_total,
         Personal_Distress=IRI_PD_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("IRI_",item))
IRI_Wave1_totals_graph<-ggplot(IRI_Wave1_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("IRI W1 total subscores for ",length(unique(IRI_Wave1_outdf$tagid[!is.na(IRI_Wave1_outdf$tagid)]))," participants"))+ geom_vline(aes(xintercept = mean(IRI_Wave1_outdf$IRI_PT_total, na.rm=T), colour="Perspective_Taking")) + scale_x_continuous(breaks = c(4,8,12,16,20,24,28)) + ylim(0,0.15)

IRI_Wave2_totals<-IRI_Wave2_outdf %>%
  select(tagid,IRI_PT_total,IRI_EC_total,IRI_PD_total) %>%
  mutate(Perspective_Taking=IRI_PT_total,
         Empathic_Concern=IRI_EC_total,
         Personal_Distress=IRI_PD_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("IRI_",item))
IRI_Wave2_totals_graph<-ggplot(IRI_Wave2_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("IRI W2 total subscores for ",length(unique(IRI_Wave2_outdf$tagid[!is.na(IRI_Wave2_outdf$tagid)]))," participants"))+ geom_vline(aes(xintercept = mean(IRI_Wave2_outdf$IRI_PT_total, na.rm=T), colour="Perspective_Taking")) + scale_x_continuous(breaks = c(4,8,12,16,20,24,28)) + ylim(0,0.15)

IRI_Wave3_totals<-IRI_Wave3_outdf %>%
  select(tagid,IRI_PT_total,IRI_EC_total,IRI_PD_total) %>%
  mutate(Perspective_Taking=IRI_PT_total,
         Empathic_Concern=IRI_EC_total,
         Personal_Distress=IRI_PD_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("IRI_",item))
IRI_Wave3_totals_graph<-ggplot(IRI_Wave3_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("IRI W3 total subscores for ",length(unique(IRI_Wave3_outdf$tagid[!is.na(IRI_Wave3_outdf$tagid)]))," participants"))+ geom_vline(aes(xintercept = mean(IRI_Wave3_outdf$IRI_PT_total, na.rm=T), colour="Perspective_Taking")) + scale_x_continuous(breaks = c(4,8,12,16,20,24,28)) + ylim(0,0.15)

IRI_Wave4_totals<-IRI_Wave4_outdf %>%
  select(tagid,IRI_PT_total,IRI_EC_total,IRI_PD_total) %>%
  mutate(Perspective_Taking=IRI_PT_total,
         Empathic_Concern=IRI_EC_total,
         Personal_Distress=IRI_PD_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("IRI_",item))
IRI_Wave4_totals_graph<-ggplot(IRI_Wave4_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("IRI W4 total subscores for ",length(unique(IRI_Wave4_outdf$tagid[!is.na(IRI_Wave4_outdf$tagid)]))," participants"))+ geom_vline(aes(xintercept = mean(IRI_Wave4_outdf$IRI_PT_total, na.rm=T), colour="Perspective_Taking")) + scale_x_continuous(breaks = c(4,8,12,16,20,24,28)) + ylim(0,0.15)

IRI_Wave5_totals<-IRI_Wave5_outdf %>%
  select(tagid,IRI_PT_total,IRI_EC_total,IRI_PD_total) %>%
  mutate(Perspective_Taking=IRI_PT_total,
         Empathic_Concern=IRI_EC_total,
         Personal_Distress=IRI_PD_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("IRI_",item))
IRI_Wave5_totals_graph<-ggplot(IRI_Wave5_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("IRI W5 total subscores for ",length(unique(IRI_Wave5_outdf$tagid[!is.na(IRI_Wave5_outdf$tagid)]))," participants"))+ geom_vline(aes(xintercept = mean(IRI_Wave5_outdf$IRI_PT_total, na.rm=T), colour="Perspective_Taking")) + scale_x_continuous(breaks = c(4,8,12,16,20,24,28)) + ylim(0,0.15)

grid.arrange(IRI_Wave1_totals_graph, IRI_Wave2_totals_graph, IRI_Wave3_totals_graph, IRI_Wave4_totals_graph, IRI_Wave5_totals_graph)

EC1 <- IRI_Wave1_totals %>% filter(grepl("Empathic_Concern", item))
mean(EC1$value, na.rm=T)
EC2 <- IRI_Wave2_totals %>% filter(grepl("Empathic_Concern", item))
mean(EC2$value, na.rm=T)
EC3 <- IRI_Wave3_totals %>% filter(grepl("Empathic_Concern", item))
mean(EC3$value, na.rm=T)
EC4 <- IRI_Wave4_totals %>% filter(grepl("Empathic_Concern", item))
mean(EC4$value, na.rm=T)
EC5 <- IRI_Wave5_totals %>% filter(grepl("Empathic_Concern", item))
mean(EC5$value, na.rm=T)

PD1 <- IRI_Wave1_totals %>% filter(grepl("Personal_Distress", item))
mean(PD1$value, na.rm=T)
PD2 <- IRI_Wave2_totals %>% filter(grepl("Personal_Distress", item))
mean(PD2$value, na.rm=T)
PD3 <- IRI_Wave3_totals %>% filter(grepl("Personal_Distress", item))
mean(PD3$value, na.rm=T)
PD4 <- IRI_Wave4_totals %>% filter(grepl("Personal_Distress", item))
mean(PD4$value, na.rm=T)
PD5 <- IRI_Wave5_totals %>% filter(grepl("Personal_Distress", item))
mean(PD5$value, na.rm=T)

PT1 <- IRI_Wave1_totals %>% filter(grepl("Perspective_Taking", item))
mean(PT1$value, na.rm=T)
PT2 <- IRI_Wave2_totals %>% filter(grepl("Perspective_Taking", item))
mean(PT2$value, na.rm=T)
PT3 <- IRI_Wave3_totals %>% filter(grepl("Perspective_Taking", item))
mean(PT3$value, na.rm=T)
PT4 <- IRI_Wave4_totals %>% filter(grepl("Perspective_Taking", item))
mean(PT4$value, na.rm=T)
PT5 <- IRI_Wave5_totals %>% filter(grepl("Perspective_Taking", item))
mean(PT5$value, na.rm=T)

IRI_Wave1_means<-IRI_Wave1_outdf %>%
  select(tagid,IRI_PT_mean,IRI_EC_mean,IRI_PD_mean) %>%
  mutate(Perspective_Taking=IRI_PT_mean,
         Empathic_Concern=IRI_EC_mean,
         Personal_Distress=IRI_PD_mean) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("IRI_",item))
IRI_Wave1_means_graph<-ggplot(IRI_Wave1_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("IRI mean subscores for ",length(unique(IRI_Wave1_outdf$tagid[!is.na(IRI_Wave1_outdf$tagid)]))," participants"))

long_iri_age <- merge(agelong, IRI_alldata, by=c('tagid', 'wave'))

tspag <- ggplot(long_iri_age, aes(x=age, y=IRI_PT_total)) + 
  geom_line() + guides(colour=F) + xlab("Age") + ylab("Tendency to Spontaneously 
                                                      \nTake Others' Perspectives")
spag <- tspag + aes(colour = factor(tagid))

sspag_iript <- spag + geom_smooth(se=F, colour='black', size=2) + xlim(10,18)

tspag <- ggplot(long_iri_age, aes(x=age, y=IRI_EC_total)) + 
  geom_line() + guides(colour=F) + xlab("Age") + ylab("Empathic Concern")
spag <- tspag + aes(colour = factor(tagid))

sspag_iriec <- spag + geom_smooth(se=F, colour='black', size=2) + xlim(10,18)

tspag <- ggplot(long_iri_age, aes(x=age, y=IRI_PD_total)) + 
  geom_line() + guides(colour=F) + xlab("Age") + ylab("Personal Distress")
spag <- tspag + aes(colour = factor(tagid))

sspag_iripd <- spag + geom_smooth(se=F, colour='black', size=2) + xlim(10,18)

library(grid)
grid.arrange(sspag_iriec, sspag_iripd, sspag_iript, ncol=3, 
             top=textGrob("Interpersonal Reactivity Index Over Time"))

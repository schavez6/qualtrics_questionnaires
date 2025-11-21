## Graph it
CAMM_Wave1_total_graph<-ggplot(CAMM_Wave1_outdf, aes(x=CAMM_total, colour="deeppink")) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Child and Adolescent Mindfulness Wave1 totals for ",length(CAMM_Wave1_outdf$CAMM_total[!is.na(CAMM_Wave1_outdf$CAMM_total)])," participants"))

CAMM_Wave2_total_graph<-ggplot(CAMM_Wave2_outdf, aes(x=CAMM_total, colour="deeppink")) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Child and Adolescent Mindfulness Wave2 totals for ",length(CAMM_Wave2_outdf$CAMM_total[!is.na(CAMM_Wave2_outdf$CAMM_total)])," participants"))

CAMM_Wave3_total_graph<-ggplot(CAMM_Wave3_outdf, aes(x=CAMM_total, colour="deeppink")) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Child and Adolescent Mindfulness Wave3 totals for ",length(CAMM_Wave3_outdf$CAMM_total[!is.na(CAMM_Wave3_outdf$CAMM_total)])," participants"))

CAMM_Wave4_total_graph<-ggplot(CAMM_Wave4_outdf, aes(x=CAMM_total, colour="deeppink")) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Child and Adolescent Mindfulness Wave4 totals for ",length(CAMM_Wave4_outdf$CAMM_total[!is.na(CAMM_Wave4_outdf$CAMM_total)])," participants"))

CAMM_Wave5_total_graph<-ggplot(CAMM_Wave5_outdf, aes(x=CAMM_total, colour="deeppink")) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Child and Adolescent Mindfulness Wave5 totals for ",length(CAMM_Wave5_outdf$CAMM_total[!is.na(CAMM_Wave5_outdf$CAMM_total)])," participants"))

CAMM_Wave1_mean_graph<-ggplot(CAMM_Wave1, aes(x=CAMM_mean, colour="deeppink")) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Child and Adolescent Mindfulness Wave1 means for ",length(CAMM_Wave1$CAMM_mean[!is.na(CAMM_Wave1$CAMM_mean)])," participants"))

require(gridExtra)
grid.arrange(CAMM_Wave1_total_graph, CAMM_Wave2_total_graph, 
             CAMM_Wave3_total_graph, CAMM_Wave4_total_graph, 
             CAMM_Wave5_total_graph, ncol=2)

mean(CAMM_Wave1_outdf$CAMM_total, na.rm=T)
mean(CAMM_Wave2_outdf$CAMM_total, na.rm=T)
mean(CAMM_Wave3_outdf$CAMM_total, na.rm=T)
mean(CAMM_Wave4_outdf$CAMM_total, na.rm=T)
mean(CAMM_Wave5_outdf$CAMM_total, na.rm=T)
mean(CAMM_Wave6_outdf$CAMM_total, na.rm=T)

library(ggplot2)
agelong <- read.csv(file=paste0(workdir,"Demographics/allwaves_age_long_06.13.2025.csv"))

CAMM_Wave1_outdf$wave <- 1
CAMM_Wave1_TAG2_outdf$wave <- 1
CAMM_Wave2_outdf$wave <- 2
CAMM_Wave2_TAG2_outdf$wave <- 2
CAMM_Wave3_outdf$wave <- 3
CAMM_Wave3_TAG2_outdf$wave <- 3
CAMM_Wave4_outdf$wave <- 4
CAMM_Wave5_outdf$wave <- 5
CAMM_Wave6_outdf$wave <- 6

#need to add tag2w3 when available to this rbind command
CAMM_alldata <- rbind(CAMM_Wave1_outdf,CAMM_Wave2_outdf,CAMM_Wave3_outdf,CAMM_Wave4_outdf,CAMM_Wave5_outdf,CAMM_Wave6_outdf,CAMM_Wave1_TAG2_outdf,CAMM_Wave2_TAG2_outdf,CAMM_Wave3_TAG2_outdf)

CAMM_alldata <- CAMM_alldata[with(CAMM_alldata, order(tagid, survey_date)),]
CAMM_alldata <- CAMM_alldata %>% relocate(tagid, wave, survey_date)
write.csv(CAMM_alldata, file = paste0(workdir,"Qualtrics_Output/Long/CAMM_AllObs_Chronological.csv"))

tspag <- ggplot(CAMM_Wave1, aes(x=as.Date(survey_date), y=CAMM_total)) + 
  geom_line() + guides(colour=F) + xlab("Date") + ylab("Mindfulness Total")
spag <- tspag + aes(colour = factor(tagid))

sspag <- spag + geom_smooth(se=F, colour='black', size=2)

sppa_camm_age <- merge(agelong, CAMM_alldata, by=c('tagid', 'wave'))

tspag_age <- ggplot(sppa_camm_age, aes(x=age, y=CAMM_total)) + 
  geom_line() + guides(colour=F) + xlab("Age") + ylab("Mindfulness Total")
spag_age <- tspag_age + aes(colour = factor(tagid))

sspag_age_gsw <- spag_age + geom_smooth(se=F, colour='black', size=2)

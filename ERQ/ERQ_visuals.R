## Graph it
ERQ_Wave1_totals<-ERQ_Wave1_outdf %>%
  select(tagid,ERQ_reappraisal_total,ERQ_suppression_total) %>%
  mutate(Emotion_Reappraisal=ERQ_reappraisal_total,
         Emotion_Suppression=ERQ_suppression_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("ERQ_",item))
ERQ_Wave1_totals_graph<-ggplot(ERQ_Wave1_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("ERQ W1 total subscores for ",length(unique(ERQ_Wave1_totals$tagid[!is.na(ERQ_Wave1_totals$tagid)]))," participants")) + geom_vline(aes(xintercept = mean(ERQ_Wave1_outdf$ERQ_reappraisal_total, na.rm=T), colour="Emotion_Reappraisal"))  + geom_vline(aes(xintercept = mean(ERQ_Wave1_outdf$ERQ_suppression_total, na.rm=T), colour="Emotion_Suppression"))

ERQ_Wave2_totals<-ERQ_Wave2_outdf %>%
  select(tagid,ERQ_reappraisal_total,ERQ_suppression_total) %>%
  mutate(Emotion_Reappraisal=ERQ_reappraisal_total,
         Emotion_Suppression=ERQ_suppression_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("ERQ_",item))
ERQ_Wave2_totals_graph<-ggplot(ERQ_Wave2_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("ERQ W2 total subscores for ",length(unique(ERQ_Wave2_totals$tagid[!is.na(ERQ_Wave2_totals$tagid)]))," participants")) + geom_vline(aes(xintercept = mean(ERQ_Wave2_outdf$ERQ_reappraisal_total, na.rm=T), colour="Emotion_Reappraisal"))  + geom_vline(aes(xintercept = mean(ERQ_Wave2_outdf$ERQ_suppression_total, na.rm=T), colour="Emotion_Suppression"))

ERQ_Wave3_totals<-ERQ_Wave3_outdf %>%
  select(tagid,ERQ_reappraisal_total,ERQ_suppression_total) %>%
  mutate(Emotion_Reappraisal=ERQ_reappraisal_total,
         Emotion_Suppression=ERQ_suppression_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("ERQ_",item))
ERQ_Wave3_totals_graph<-ggplot(ERQ_Wave3_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("ERQ W3 total subscores for ",length(unique(ERQ_Wave3_totals$tagid[!is.na(ERQ_Wave3_totals$tagid)]))," participants")) + geom_vline(aes(xintercept = mean(ERQ_Wave3_outdf$ERQ_reappraisal_total, na.rm=T), colour="Emotion_Reappraisal"))  + geom_vline(aes(xintercept = mean(ERQ_Wave3_outdf$ERQ_suppression_total, na.rm=T), colour="Emotion_Suppression"))

ERQ_Wave4_totals<-ERQ_Wave4_outdf %>%
  select(tagid,ERQ_reappraisal_total,ERQ_suppression_total) %>%
  mutate(Emotion_Reappraisal=ERQ_reappraisal_total,
         Emotion_Suppression=ERQ_suppression_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("ERQ_",item))
ERQ_Wave4_totals_graph<-ggplot(ERQ_Wave4_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("ERQ W4 total subscores for ",length(unique(ERQ_Wave4_totals$tagid[!is.na(ERQ_Wave4_totals$tagid)]))," participants")) + geom_vline(aes(xintercept = mean(ERQ_Wave4_outdf$ERQ_reappraisal_total, na.rm=T), colour="Emotion_Reappraisal"))  + geom_vline(aes(xintercept = mean(ERQ_Wave4_outdf$ERQ_suppression_total, na.rm=T), colour="Emotion_Suppression"))

ERQ_Wave5_totals<-ERQ_Wave5_outdf %>%
  select(tagid,ERQ_reappraisal_total,ERQ_suppression_total) %>%
  mutate(Emotion_Reappraisal=ERQ_reappraisal_total,
         Emotion_Suppression=ERQ_suppression_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("ERQ_",item))
ERQ_Wave5_totals_graph<-ggplot(ERQ_Wave5_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("ERQ W5 total subscores for ",length(unique(ERQ_Wave5_totals$tagid[!is.na(ERQ_Wave5_totals$tagid)]))," participants")) + geom_vline(aes(xintercept = mean(ERQ_Wave5_outdf$ERQ_reappraisal_total, na.rm=T), colour="Emotion_Reappraisal"))  + geom_vline(aes(xintercept = mean(ERQ_Wave5_outdf$ERQ_suppression_total, na.rm=T), colour="Emotion_Suppression"))

ERQ_Wave6_totals<-ERQ_Wave6_outdf %>%
  select(tagid,ERQ_reappraisal_total,ERQ_suppression_total) %>%
  mutate(Emotion_Reappraisal=ERQ_reappraisal_total,
         Emotion_Suppression=ERQ_suppression_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("ERQ_",item))
ERQ_Wave6_totals_graph<-ggplot(ERQ_Wave6_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("ERQ W6 total subscores for ",length(unique(ERQ_Wave6_totals$tagid[!is.na(ERQ_Wave6_totals$tagid)]))," participants")) + geom_vline(aes(xintercept = mean(ERQ_Wave6_outdf$ERQ_reappraisal_total, na.rm=T), colour="Emotion_Reappraisal"))  + geom_vline(aes(xintercept = mean(ERQ_Wave6_outdf$ERQ_suppression_total, na.rm=T), colour="Emotion_Suppression"))

grid.arrange(ERQ_Wave1_totals_graph, ERQ_Wave2_totals_graph, 
             ERQ_Wave3_totals_graph, ERQ_Wave4_totals_graph, 
             ERQ_Wave5_totals_graph, ERQ_Wave6_totals_graph, ncol=2)

ERQ_Wave1_means<-ERQ_Wave1 %>%
  select(tagid,ERQ_reappraisal_mean,ERQ_suppression_mean) %>%
  mutate(Emotion_Reappraisal=ERQ_reappraisal_mean,
         Emotion_Suppression=ERQ_suppression_mean) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("ERQ_",item))
ERQ_Wave1_means_graph<-ggplot(ERQ_Wave1_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("ERQ mean subscores for ",length(unique(ERQ_Wave1$tagid[!is.na(ERQ_Wave1$tagid)]))," participants"))

long_erq_age <- merge(agelong, ERQ_alldata, by=c('tagid', 'wave'))

tspag <- ggplot(long_erq_age, aes(x=age, y=ERQ_reappraisal_total)) + 
  geom_line() + guides(colour=F) + xlab("Age") + ylab("Tendency to Regulate Emotions 
                                                      \nvia Cognitive Reappraisal")
spag <- tspag + aes(colour = factor(tagid))

sspag_erqcr <- spag + geom_smooth(se=F, colour='black', size=2) + xlim(10,21) + ylim(7,42)

tspag <- ggplot(long_erq_age, aes(x=age, y=ERQ_suppression_total)) + 
  geom_line() + guides(colour=F) + xlab("Age") + ylab("Tendency to Regulate Emotions 
                                                      \nvia Expressive Suppression")
spag <- tspag + aes(colour = factor(tagid))

sspag_erqes <- spag + geom_smooth(se=F, colour='black', size=2) + xlim(10,21) + ylim(7,28)

grid.arrange(sspag_erqcr, sspag_erqes)

## Graph it
SCARED_Wave1_totals<-SCARED_Wave1 %>%
  select(tagid,SCARED_total,SCARED_ptsd_total,SCARED_anxiety_total)%>%
  gather('item','value',2:length(.)) 
SCARED_Wave1_totals_graph<-ggplot(SCARED_Wave1_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("SCARED totals for ",length(unique(SCARED_Wave1$tagid[!is.na(SCARED_Wave1$SCARED_total)]))," participants"))

SCARED_Wave1_means<-SCARED_Wave1 %>%
  select(tagid,SCARED_mean,SCARED_ptsd_mean,SCARED_anxiety_mean)%>%
  gather('item','value',2:length(.)) 
SCARED_Wave1_means_graph<-ggplot(SCARED_Wave1_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("SCARED means for ",length(unique(SCARED_Wave1$tagid[!is.na(SCARED_Wave1$SCARED_mean)]))," participants"))

SCARED_Wave1_totals_graph<-ggplot(SCARED_Wave1_outdf, aes(x=SCARED_total, colour="dodgerblue")) +
  geom_density(alpha=.3, show.legend = FALSE)+
  ggtitle(paste0("SCARED total scores for ",length(SCARED_Wave1_outdf$SCARED_total[!is.na(SCARED_Wave1_outdf$SCARED_total)])," Wave 1 participants")) + xlim(0,22)

SCARED_Wave2_totals_graph<-ggplot(SCARED_Wave2_outdf, aes(x=SCARED_total, colour="dodgerblue")) +
  geom_density(alpha=.3, show.legend = FALSE)+
  ggtitle(paste0("SCARED total scores for ",length(SCARED_Wave2_outdf$SCARED_total[!is.na(SCARED_Wave2_outdf$SCARED_total)])," Wave 2 participants")) + xlim(0,22)

SCARED_Wave3_totals_graph<-ggplot(SCARED_Wave3_outdf, aes(x=SCARED_total, colour="dodgerblue")) +
  geom_density(alpha=.3, show.legend = FALSE)+
  ggtitle(paste0("SCARED total scores for ",length(SCARED_Wave3_outdf$SCARED_total[!is.na(SCARED_Wave3_outdf$SCARED_total)])," Wave 3 participants")) + xlim(0,22)

SCARED_Wave4_totals_graph<-ggplot(SCARED_Wave4_outdf, aes(x=SCARED_total, colour="dodgerblue")) +
  geom_density(alpha=.3, show.legend = FALSE)+
  ggtitle(paste0("SCARED total scores for ",length(SCARED_Wave4_outdf$SCARED_total[!is.na(SCARED_Wave4_outdf$SCARED_total)])," Wave 4 participants")) + xlim(0,22)

SCARED_Wave5_totals_graph<-ggplot(SCARED_Wave5_outdf, aes(x=SCARED_total, colour="dodgerblue")) +
  geom_density(alpha=.3, show.legend = FALSE)+
  ggtitle(paste0("SCARED total scores for ",length(SCARED_Wave5_outdf$SCARED_total[!is.na(SCARED_Wave5_outdf$SCARED_total)])," Wave 5 participants")) + xlim(0,22)

require(gridExtra)
grid.arrange(SCARED_Wave1_totals_graph, SCARED_Wave2_totals_graph, 
             SCARED_Wave3_totals_graph, SCARED_Wave4_totals_graph, 
             SCARED_Wave5_totals_graph, ncol=2)

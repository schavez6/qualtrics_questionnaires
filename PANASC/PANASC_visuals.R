## Graph it
PANAS_Wave1_totals<-PANAS_Wave1_outdf %>%
  select(tagid,PANAS_pos_total,PANAS_neg_total) %>%
  mutate(Positive_Affect=PANAS_pos_total,
         Negative_Affect=PANAS_neg_total) %>%
  select(-contains("PANAS")) %>%
  gather('item','value',2:length(.)) 
PANAS_Wave1_totals_graph<-ggplot(PANAS_Wave1_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("PANAS totals for ",length(unique(PANAS_Wave1_outdf$tagid[!is.na(PANAS_Wave1_outdf$PANAS_pos_total)]))," participants"))

PANAS_Wave2_totals<-PANAS_Wave2_outdf %>%
  select(tagid,PANAS_pos_total,PANAS_neg_total) %>%
  mutate(Positive_Affect=PANAS_pos_total,
         Negative_Affect=PANAS_neg_total) %>%
  select(-contains("PANAS")) %>%
  gather('item','value',2:length(.)) 
PANAS_Wave2_totals_graph<-ggplot(PANAS_Wave2_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("PANAS totals for ",length(unique(PANAS_Wave2_outdf$tagid[!is.na(PANAS_Wave2_outdf$PANAS_pos_total)]))," participants"))

PANAS_Wave1_means<-PANAS_Wave1 %>%
  select(tagid,PANAS_pos_mean,PANAS_neg_mean) %>%
  mutate(Positive_Affect=PANAS_pos_mean,
         Negative_Affect=PANAS_neg_mean) %>%
  select(-contains("PANAS")) %>%
  gather('item','value',2:length(.)) 
PANAS_Wave1_means_graph<-ggplot(PANAS_Wave1_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("PANAS means for ",length(unique(PANAS_Wave1$tagid[!is.na(PANAS_Wave1$PANAS_pos_mean)]))," participants"))

options(scipen=999)
PANAS_Wave1_cor_graph<-ggplot(PANAS_Wave1, aes(x=PANAS_neg_mean,y=PANAS_pos_mean)) +
  ylab("Positive Affect")+
  xlab("Negative Affect")+
  geom_point(show.legend = FALSE)+
  ggtitle(paste0("PANAS positive and negative affect correlations for ",length(unique(PANAS_Wave1$tagid[!is.na(PANAS_Wave1$PANAS_pos_mean)])),
                 " Wave 1 participants. r = ",
                 round(cor.test(x = PANAS_Wave1$PANAS_pos_mean,y=PANAS_Wave1$PANAS_neg_mean,use=na.or.complete)[[4]],3)," p = ",
                 cor.test(x=PANAS_Wave1$PANAS_pos_mean,y=PANAS_Wave1$PANAS_neg_mean,use=na.or.complete)[[3]]))

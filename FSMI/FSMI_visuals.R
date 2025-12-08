## Graph it
FSMI_Wave3_means<-FSMI_Wave3_outdf %>%
  select(tagid,self_protection,disease_avoidance,group_affiliation,
         exclusion_concern_affiliation,independence_affiliation,status,
         mate_seeking,general_mate_retention,breakup_concern_mate_retention) %>% 
  select(-contains("FSMI")) %>%
  gather('item','value',2:length(.)) 
FSMI_Wave3_means_graph<-ggplot(FSMI_Wave3_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Wave 3 FSMI mean subscores for ",length(unique(FSMI_Wave3_outdf$tagid[!is.na(FSMI_Wave3_outdf$self_protection)]))," participants"))

FSMI_Wave4_means<-FSMI_Wave4_outdf %>%
  select(tagid,self_protection,disease_avoidance,group_affiliation,
         exclusion_concern_affiliation,independence_affiliation,status,
         mate_seeking,general_mate_retention,breakup_concern_mate_retention) %>% 
  select(-contains("FSMI")) %>%
  gather('item','value',2:length(.)) 
FSMI_Wave4_means_graph<-ggplot(FSMI_Wave4_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Wave 4 FSMI mean subscores for ",length(unique(FSMI_Wave4_outdf$tagid[!is.na(FSMI_Wave4_outdf$self_protection)]))," participants"))

FSMI_Wave5_means<-FSMI_Wave5_outdf %>%
  select(tagid,self_protection,disease_avoidance,group_affiliation,
         exclusion_concern_affiliation,independence_affiliation,status,
         mate_seeking,general_mate_retention,breakup_concern_mate_retention) %>% 
  select(-contains("FSMI")) %>%
  gather('item','value',2:length(.)) 
FSMI_Wave5_means_graph<-ggplot(FSMI_Wave5_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Wave 5 FSMI mean subscores for ",length(unique(FSMI_Wave5_outdf$tagid[!is.na(FSMI_Wave5_outdf$self_protection)]))," participants"))

FSMI_Wave1_TAG2_means<-FSMI_Wave1_TAG2_outdf %>%
  select(tagid,self_protection,disease_avoidance,group_affiliation,
         exclusion_concern_affiliation,independence_affiliation,status,
         mate_seeking,general_mate_retention,breakup_concern_mate_retention) %>% 
  select(-contains("FSMI")) %>%
  gather('item','value',2:length(.)) 
FSMI_Wave1_TAG2_means_graph<-ggplot(FSMI_Wave1_TAG2_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Wave 1 TAG2 FSMI mean subscores for ",length(unique(FSMI_Wave1_TAG2_outdf$tagid[!is.na(FSMI_Wave1_TAG2_outdf$self_protection)]))," participants"))

grid.arrange(FSMI_Wave3_means_graph, FSMI_Wave4_means_graph, 
             FSMI_Wave5_means_graph, FSMI_Wave1_TAG2_means_graph)

stat3 <- FSMI_Wave3_means %>% filter(grepl("status", item))
mean(stat3$value, na.rm=T)
stat4 <- FSMI_Wave4_means %>% filter(grepl("status", item))
mean(stat4$value, na.rm=T)

group3 <- FSMI_Wave3_means %>% filter(grepl("group_affiliation", item))
mean(group3$value, na.rm=T)
group4 <- FSMI_Wave4_means %>% filter(grepl("group_affiliation", item))
mean(group4$value, na.rm=T)

seek3 <- FSMI_Wave3_means %>% filter(grepl("mate_seeking", item))
mean(seek3$value, na.rm=T)
seek4 <- FSMI_Wave4_means %>% filter(grepl("mate_seeking", item))
mean(seek4$value, na.rm=T)

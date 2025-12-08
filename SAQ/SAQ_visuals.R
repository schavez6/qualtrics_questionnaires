## Graph it
SAQ_Wave1_totals<-SAQ_Wave1_outdf %>%
  select(tagid,SAQ_socdevgoals_total,SAQ_approachgoals_total,SAQ_avoidgoals_total) %>%
  mutate(Social_Development_Goals=SAQ_socdevgoals_total,
         Social_Approach_Goals=SAQ_approachgoals_total,
         Social_Avoidance_Goals=SAQ_avoidgoals_total) %>%
  select(-contains("SAQ")) %>%
  gather('item','value',2:length(.)) 
SAQ_Wave1_totals_graph<-ggplot(SAQ_Wave1_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("SAQ W1 totals for ",length(unique(SAQ_Wave1_outdf$tagid[!is.na(SAQ_Wave1_outdf$SAQ_approachgoals_total)]))," participants"))

SAQ_Wave2_totals<-SAQ_Wave2_outdf %>%
  select(tagid,SAQ_socdevgoals_total,SAQ_approachgoals_total,SAQ_avoidgoals_total) %>%
  mutate(Social_Development_Goals=SAQ_socdevgoals_total,
         Social_Approach_Goals=SAQ_approachgoals_total,
         Social_Avoidance_Goals=SAQ_avoidgoals_total) %>%
  select(-contains("SAQ")) %>%
  gather('item','value',2:length(.)) 
SAQ_Wave2_totals_graph<-ggplot(SAQ_Wave2_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("SAQ W2 totals for ",length(unique(SAQ_Wave2_outdf$tagid[!is.na(SAQ_Wave2_outdf$SAQ_approachgoals_total)]))," participants"))

SAQ_Wave3_totals<-SAQ_Wave3_outdf %>%
  select(tagid,SAQ_socdevgoals_total,SAQ_approachgoals_total,SAQ_avoidgoals_total) %>%
  mutate(Social_Development_Goals=SAQ_socdevgoals_total,
         Social_Approach_Goals=SAQ_approachgoals_total,
         Social_Avoidance_Goals=SAQ_avoidgoals_total) %>%
  select(-contains("SAQ")) %>%
  gather('item','value',2:length(.)) 
SAQ_Wave3_totals_graph<-ggplot(SAQ_Wave3_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("SAQ W3 totals for ",length(unique(SAQ_Wave3_outdf$tagid[!is.na(SAQ_Wave3_outdf$SAQ_approachgoals_total)]))," participants"))

SAQ_Wave4_totals<-SAQ_Wave4_outdf %>%
  select(tagid,SAQ_socdevgoals_total,SAQ_approachgoals_total,SAQ_avoidgoals_total) %>%
  mutate(Social_Development_Goals=SAQ_socdevgoals_total,
         Social_Approach_Goals=SAQ_approachgoals_total,
         Social_Avoidance_Goals=SAQ_avoidgoals_total) %>%
  select(-contains("SAQ")) %>%
  gather('item','value',2:length(.)) 
SAQ_Wave4_totals_graph<-ggplot(SAQ_Wave4_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("SAQ W4 totals for ",length(unique(SAQ_Wave4_outdf$tagid[!is.na(SAQ_Wave4_outdf$SAQ_approachgoals_total)]))," participants"))

SAQ_Wave5_totals<-SAQ_Wave5_outdf %>%
  select(tagid,SAQ_socdevgoals_total,SAQ_approachgoals_total,SAQ_avoidgoals_total) %>%
  mutate(Social_Development_Goals=SAQ_socdevgoals_total,
         Social_Approach_Goals=SAQ_approachgoals_total,
         Social_Avoidance_Goals=SAQ_avoidgoals_total) %>%
  select(-contains("SAQ")) %>%
  gather('item','value',2:length(.)) 
SAQ_Wave5_totals_graph<-ggplot(SAQ_Wave5_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("SAQ W5 totals for ",length(unique(SAQ_Wave5_outdf$tagid[!is.na(SAQ_Wave5_outdf$SAQ_approachgoals_total)]))," participants"))

grid.arrange(SAQ_Wave1_totals_graph, SAQ_Wave2_totals_graph,
             SAQ_Wave3_totals_graph, SAQ_Wave4_totals_graph,
             SAQ_Wave5_totals_graph)

App1 <- SAQ_Wave1_totals %>% filter(grepl("Social_Approach_Goals", item))
median(App1$value, na.rm=T)
App2 <- SAQ_Wave2_totals %>% filter(grepl("Social_Approach_Goals", item))
median(App2$value, na.rm=T)
App3 <- SAQ_Wave3_totals %>% filter(grepl("Social_Approach_Goals", item))
median(App3$value, na.rm=T)
App4 <- SAQ_Wave4_totals %>% filter(grepl("Social_Approach_Goals", item))
median(App4$value, na.rm=T)
App5 <- SAQ_Wave5_totals %>% filter(grepl("Social_Approach_Goals", item))
median(App5$value, na.rm=T)

Avo1 <- SAQ_Wave1_totals %>% filter(grepl("Social_Avoidance_Goals", item))
median(Avo1$value, na.rm=T)
Avo2 <- SAQ_Wave2_totals %>% filter(grepl("Social_Avoidance_Goals", item))
median(Avo2$value, na.rm=T)
Avo3 <- SAQ_Wave3_totals %>% filter(grepl("Social_Avoidance_Goals", item))
median(Avo3$value, na.rm=T)
Avo4 <- SAQ_Wave4_totals %>% filter(grepl("Social_Avoidance_Goals", item))
median(Avo4$value, na.rm=T)
Avo5 <- SAQ_Wave5_totals %>% filter(grepl("Social_Avoidance_Goals", item))
median(Avo5$value, na.rm=T)

Dev1 <- SAQ_Wave1_totals %>% filter(grepl("Social_Development_Goals", item))
mean(Dev1$value, na.rm=T)
Dev2 <- SAQ_Wave2_totals %>% filter(grepl("Social_Development_Goals", item))
mean(Dev2$value, na.rm=T)
Dev3 <- SAQ_Wave3_totals %>% filter(grepl("Social_Development_Goals", item))
mean(Dev3$value, na.rm=T)
Dev4 <- SAQ_Wave4_totals %>% filter(grepl("Social_Development_Goals", item))
mean(Dev4$value, na.rm=T)
Dev5 <- SAQ_Wave5_totals %>% filter(grepl("Social_Development_Goals", item))
mean(Dev5$value, na.rm=T)

SAQ_Wave1_socdev_items<-SAQ_Wave1 %>%
  select(tagid,SAQ_1,SAQ_2,SAQ_3,SAQ_4,SAQ_5,SAQ_6,SAQ_7,SAQ_8) %>%
  gather('item','value',2:length(.)) 
SAQ_Wave1_socdev_items_graph<-ggplot(SAQ_Wave1_socdev_items, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("SAQ Social Development Goals items for ",length(unique(SAQ_Wave1$tagid[!is.na(SAQ_Wave1$tagid)]))," participants"))

SAQ_Wave1_means<-SAQ_Wave1 %>%
  select(tagid,SAQ_socdevgoals_mean,SAQ_approachgoals_mean,SAQ_avoidgoals_mean) %>%
  mutate(Social_Development_Goals=SAQ_socdevgoals_mean,
         Social_Approach_Goals=SAQ_approachgoals_mean,
         Social_Avoidance_Goals=SAQ_avoidgoals_mean) %>%
  select(-contains("SAQ")) %>%
  gather('item','value',2:length(.)) 
SAQ_Wave1_means_graph<-ggplot(SAQ_Wave1_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("SAQ means for ",length(unique(SAQ_Wave1$tagid[!is.na(SAQ_Wave1$SAQ_approachgoals_mean)]))," participants"))

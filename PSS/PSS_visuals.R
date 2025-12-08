## Graph it
PSS_Wave1_totals<-PSS_Wave123456_FirstResponse %>%
  select(tagid,PSS_total) %>%
  mutate(Perceived_Stress=PSS_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("PSS_",item))
PSS_Wave1_totals_graph<-ggplot(PSS_Wave1_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("PSS total score for ",length(unique(PSS_Wave123456_FirstResponse$tagid[!is.na(PSS_Wave123456_FirstResponse$PSS_total)]))," participants"))

PSS_Wave1_means<-PSS_Wave1_outdf %>%
  select(tagid,PSS_mean) %>%
  mutate(Perceived_Stress=PSS_mean) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("PSS_",item))
PSS_Wave1_means_graph<-ggplot(PSS_Wave1_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Wave 1 PSS mean score for ",length(unique(PSS_Wave1_outdf$tagid[!is.na(PSS_Wave1_outdf$tagid)]))," participants"))

PSS_Wave2_means<-PSS_Wave2_outdf %>%
  select(tagid,PSS_mean) %>%
  mutate(Perceived_Stress=PSS_mean) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("PSS_",item))
PSS_Wave2_means_graph<-ggplot(PSS_Wave2_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Wave 2 PSS mean score for ",length(unique(PSS_Wave2_outdf$tagid[!is.na(PSS_Wave2_outdf$tagid)]))," participants"))

PSS_Wave3_means<-PSS_Wave3_Session1_outdf %>%
  select(tagid,PSS_mean) %>%
  mutate(Perceived_Stress=PSS_mean) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("PSS_",item))
PSS_Wave3_means_graph<-ggplot(PSS_Wave3_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Wave 3 PSS mean score for ",length(unique(PSS_Wave3_Session1_outdf$tagid[!is.na(PSS_Wave3_Session1_outdf$tagid)]))," participants"))

PSS_Wave4_means<-PSS_Wave4_Session1_outdf %>%
  select(tagid,PSS_mean) %>%
  mutate(Perceived_Stress=PSS_mean) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("PSS_",item))
PSS_Wave4_means_graph<-ggplot(PSS_Wave4_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Wave 4 PSS mean score for ",length(unique(PSS_Wave4_Session1_outdf$tagid[!is.na(PSS_Wave4_Session1_outdf$tagid)]))," participants"))

require(gridExtra)
grid.arrange(PSS_Wave1_means_graph, PSS_Wave2_means_graph, 
             PSS_Wave3_means_graph, PSS_Wave4_means_graph, ncol=2)

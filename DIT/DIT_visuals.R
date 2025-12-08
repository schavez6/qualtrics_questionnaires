## Graph it
DIT_Wave1_totals<-DIT_Wave1_outdf %>%
  select(tagid,DIT_belonging_total,DIT_intel_total,DIT_moral_total,
         DIT_group_total) %>%
  mutate(Social_Belonging=DIT_belonging_total,
         Intelligence_Malleability=DIT_intel_total,
         Morality_Malleability=DIT_moral_total,
         Group_Malleability=DIT_group_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("DIT_",item))
DIT_Wave1_totals_graph<-ggplot(DIT_Wave1_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("W1 DIT total subscores for ",length(unique(DIT_Wave1_outdf$tagid[!is.na(DIT_Wave1_outdf$tagid)]))," participants"))

DIT_Wave2_totals<-DIT_Wave2_outdf %>%
  select(tagid,DIT_belonging_total,DIT_intel_total,DIT_moral_total,
         DIT_group_total) %>%
  mutate(Social_Belonging=DIT_belonging_total,
         Intelligence_Malleability=DIT_intel_total,
         Morality_Malleability=DIT_moral_total,
         Group_Malleability=DIT_group_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("DIT_",item))
DIT_Wave2_totals_graph<-ggplot(DIT_Wave2_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("W2 DIT total subscores for ",length(unique(DIT_Wave2_outdf$tagid[!is.na(DIT_Wave2_outdf$tagid)]))," participants"))

DIT_Wave3_totals<-DIT_Wave3_outdf %>%
  select(tagid,DIT_belonging_total,DIT_intel_total,DIT_moral_total,
         DIT_group_total) %>%
  mutate(Social_Belonging=DIT_belonging_total,
         Intelligence_Malleability=DIT_intel_total,
         Morality_Malleability=DIT_moral_total,
         Group_Malleability=DIT_group_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("DIT_",item))
DIT_Wave3_totals_graph<-ggplot(DIT_Wave3_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("W3 DIT total subscores for ",length(unique(DIT_Wave3_outdf$tagid[!is.na(DIT_Wave3_outdf$tagid)]))," participants"))

DIT_Wave4_totals<-DIT_Wave4_outdf %>%
  select(tagid,DIT_belonging_total,DIT_intel_total,DIT_moral_total,
         DIT_group_total) %>%
  mutate(Social_Belonging=DIT_belonging_total,
         Intelligence_Malleability=DIT_intel_total,
         Morality_Malleability=DIT_moral_total,
         Group_Malleability=DIT_group_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("DIT_",item))
DIT_Wave4_totals_graph<-ggplot(DIT_Wave4_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("W4 DIT total subscores for ",length(unique(DIT_Wave4_outdf$tagid[!is.na(DIT_Wave4_outdf$tagid)]))," participants"))


DIT_Wave5_totals<-DIT_Wave5_outdf %>%
  select(tagid,DIT_belonging_total,DIT_intel_total,DIT_moral_total,
         DIT_group_total) %>%
  mutate(Social_Belonging=DIT_belonging_total,
         Intelligence_Malleability=DIT_intel_total,
         Morality_Malleability=DIT_moral_total,
         Group_Malleability=DIT_group_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("DIT_",item))
DIT_Wave5_totals_graph<-ggplot(DIT_Wave5_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("W5 DIT total subscores for ",length(unique(DIT_Wave5_outdf$tagid[!is.na(DIT_Wave5_outdf$tagid)]))," participants"))

grid.arrange(DIT_Wave1_totals_graph, DIT_Wave2_totals_graph, 
             DIT_Wave3_totals_graph, DIT_Wave4_totals_graph, 
             DIT_Wave5_totals_graph, ncol=2)

Gr1 <- DIT_Wave1_totals %>% filter(grepl("Group_Malleability", item))
mean(Gr1$value, na.rm=T)
Gr2 <- DIT_Wave2_totals %>% filter(grepl("Group_Malleability", item))
mean(Gr2$value, na.rm=T)
Gr3 <- DIT_Wave3_totals %>% filter(grepl("Group_Malleability", item))
mean(Gr3$value, na.rm=T)
Gr4 <- DIT_Wave4_totals %>% filter(grepl("Group_Malleability", item))
mean(Gr4$value, na.rm=T)
Gr5 <- DIT_Wave5_totals %>% filter(grepl("Group_Malleability", item))
mean(Gr5$value, na.rm=T)

In1 <- DIT_Wave1_totals %>% filter(grepl("Intelligence_Malleability", item))
mean(In1$value, na.rm=T)
In2 <- DIT_Wave2_totals %>% filter(grepl("Intelligence_Malleability", item))
mean(In2$value, na.rm=T)
In3 <- DIT_Wave3_totals %>% filter(grepl("Intelligence_Malleability", item))
mean(In3$value, na.rm=T)
In4 <- DIT_Wave4_totals %>% filter(grepl("Intelligence_Malleability", item))
mean(In4$value, na.rm=T)
In5 <- DIT_Wave5_totals %>% filter(grepl("Intelligence_Malleability", item))
mean(In5$value, na.rm=T)


DIT_Wave1_means<-DIT_Wave1 %>%
  select(tagid,DIT_belonging_mean,DIT_intel_mean,DIT_moral_mean,
         DIT_group_mean) %>%
  mutate(Social_Belonging=DIT_belonging_mean,
         Intelligence_Malleability=DIT_intel_mean,
         Morality_Malleability=DIT_moral_mean,
         Group_Malleability=DIT_group_mean) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("DIT_",item))
DIT_Wave1_means_graph<-ggplot(DIT_Wave1_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("DIT mean subscores for ",length(unique(DIT_Wave1$tagid[!is.na(DIT_Wave1$tagid)]))," participants"))

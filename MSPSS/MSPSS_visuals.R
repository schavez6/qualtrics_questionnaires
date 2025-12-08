## Graph it
MSPSS_Wave1_totals<-MSPSS_Wave1_outdf %>%
  select(tagid,MSPSS_fam_total,MSPSS_fri_total,MSPSS_so_total) %>%
  mutate(Family_Support=MSPSS_fam_total,
         Friend_Support=MSPSS_fri_total,
         SigOther_Support=MSPSS_so_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("MSPSS_",item))
MSPSS_Wave1_totals_graph<-ggplot(MSPSS_Wave1_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("MSPSS W1 total subscores for ",length(unique(MSPSS_Wave1_outdf$tagid[!is.na(MSPSS_Wave1_outdf$tagid)]))," participants"))

MSPSS_Wave2_totals<-MSPSS_Wave2_outdf %>%
  select(tagid,MSPSS_fam_total,MSPSS_fri_total,MSPSS_so_total) %>%
  mutate(Family_Support=MSPSS_fam_total,
         Friend_Support=MSPSS_fri_total,
         SigOther_Support=MSPSS_so_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("MSPSS_",item))
MSPSS_Wave2_totals_graph<-ggplot(MSPSS_Wave2_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("MSPSS W2 total subscores for ",length(unique(MSPSS_Wave2_outdf$tagid[!is.na(MSPSS_Wave2_outdf$tagid)]))," participants"))

MSPSS_Wave3_totals<-MSPSS_Wave3_outdf %>%
  select(tagid,MSPSS_fam_total,MSPSS_fri_total,MSPSS_so_total) %>%
  mutate(Family_Support=MSPSS_fam_total,
         Friend_Support=MSPSS_fri_total,
         SigOther_Support=MSPSS_so_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("MSPSS_",item))
MSPSS_Wave3_totals_graph<-ggplot(MSPSS_Wave3_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("MSPSS W3 total subscores for ",length(unique(MSPSS_Wave3_outdf$tagid[!is.na(MSPSS_Wave3_outdf$tagid)]))," participants"))

MSPSS_Wave4_totals<-MSPSS_Wave4_outdf %>%
  select(tagid,MSPSS_fam_total,MSPSS_fri_total,MSPSS_so_total) %>%
  mutate(Family_Support=MSPSS_fam_total,
         Friend_Support=MSPSS_fri_total,
         SigOther_Support=MSPSS_so_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("MSPSS_",item))
MSPSS_Wave4_totals_graph<-ggplot(MSPSS_Wave4_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("MSPSS W4 total subscores for ",length(unique(MSPSS_Wave4_outdf$tagid[!is.na(MSPSS_Wave4_outdf$tagid)]))," participants"))

MSPSS_Wave5_totals<-MSPSS_Wave5_outdf %>%
  select(tagid,MSPSS_fam_total,MSPSS_fri_total,MSPSS_so_total) %>%
  mutate(Family_Support=MSPSS_fam_total,
         Friend_Support=MSPSS_fri_total,
         SigOther_Support=MSPSS_so_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("MSPSS_",item))
MSPSS_Wave5_totals_graph<-ggplot(MSPSS_Wave5_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("MSPSS W5 total subscores for ",length(unique(MSPSS_Wave5_outdf$tagid[!is.na(MSPSS_Wave5_outdf$tagid)]))," participants"))

grid.arrange(MSPSS_Wave1_totals_graph, MSPSS_Wave2_totals_graph, 
             MSPSS_Wave3_totals_graph, MSPSS_Wave4_totals_graph, 
             MSPSS_Wave5_totals_graph)

Fam1 <- MSPSS_Wave1_totals %>% filter(grepl("Family_Support", item))
mean(Fam1$value, na.rm=T)
Fam2 <- MSPSS_Wave2_totals %>% filter(grepl("Family_Support", item))
mean(Fam2$value, na.rm=T)
Fam3 <- MSPSS_Wave3_totals %>% filter(grepl("Family_Support", item))
mean(Fam3$value, na.rm=T)
Fam4 <- MSPSS_Wave4_totals %>% filter(grepl("Family_Support", item))
mean(Fam4$value, na.rm=T)
Fam5 <- MSPSS_Wave5_totals %>% filter(grepl("Family_Support", item))
mean(Fam5$value, na.rm=T)

Fri1 <- MSPSS_Wave1_totals %>% filter(grepl("Friend_Support", item))
mean(Fri1$value, na.rm=T)
Fri2 <- MSPSS_Wave2_totals %>% filter(grepl("Friend_Support", item))
mean(Fri2$value, na.rm=T)
Fri3 <- MSPSS_Wave3_totals %>% filter(grepl("Friend_Support", item))
mean(Fri3$value, na.rm=T)
Fri4 <- MSPSS_Wave4_totals %>% filter(grepl("Friend_Support", item))
mean(Fri4$value, na.rm=T)
Fri5 <- MSPSS_Wave5_totals %>% filter(grepl("Friend_Support", item))
mean(Fri5$value, na.rm=T)

Sig1 <- MSPSS_Wave1_totals %>% filter(grepl("SigOther_Support", item))
mean(Sig1$value, na.rm=T)
Sig2 <- MSPSS_Wave2_totals %>% filter(grepl("SigOther_Support", item))
mean(Sig2$value, na.rm=T)
Sig3 <- MSPSS_Wave3_totals %>% filter(grepl("SigOther_Support", item))
mean(Sig3$value, na.rm=T)
Sig4 <- MSPSS_Wave4_totals %>% filter(grepl("SigOther_Support", item))
mean(Sig4$value, na.rm=T)
Sig5 <- MSPSS_Wave5_totals %>% filter(grepl("SigOther_Support", item))
mean(Sig5$value, na.rm=T)

MSPSS_Wave1_means<-MSPSS_Wave1 %>%
  select(tagid,MSPSS_fam_mean,MSPSS_fri_mean,MSPSS_so_mean) %>%
  mutate(Family_Support=MSPSS_fam_mean,
         Friend_Support=MSPSS_fri_mean,
         SigOther_Support=MSPSS_so_mean) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("MSPSS_",item))
MSPSS_Wave1_means_graph<-ggplot(MSPSS_Wave1_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("MSPSS mean subscores for ",length(unique(MSPSS_Wave1$tagid[!is.na(MSPSS_Wave1$tagid)]))," participants"))

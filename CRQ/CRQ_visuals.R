## Graph it
CRQ_Wave1_totals<-CRQ_Wave1_outdf %>%
  select(tagid,CRQ_rehashing_total,CRQ_mulling_total,CRQ_problemtalk_total) %>%
  mutate(Rehashing=CRQ_rehashing_total,
         Mulling=CRQ_mulling_total,
         Problem_Talk=CRQ_problemtalk_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("CRQ_",item))
CRQ_Wave1_totals_graph<-ggplot(CRQ_Wave1_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("CRQ total subscores for ",length(CRQ_Wave1_outdf$CRQ_total[!is.na(CRQ_Wave1_outdf$CRQ_total)])," participants"))

CRQ_Wave2_totals<-CRQ_Wave2_outdf %>%
  select(tagid,CRQ_rehashing_total,CRQ_mulling_total,CRQ_problemtalk_total) %>%
  mutate(Rehashing=CRQ_rehashing_total,
         Mulling=CRQ_mulling_total,
         Problem_Talk=CRQ_problemtalk_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("CRQ_",item))
CRQ_Wave2_totals_graph<-ggplot(CRQ_Wave2_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("CRQ total subscores for ",length(CRQ_Wave2_outdf$CRQ_total[!is.na(CRQ_Wave2_outdf$CRQ_total)])," participants"))

CRQ_Wave3_totals<-CRQ_Wave3_outdf %>%
  select(tagid,CRQ_rehashing_total,CRQ_mulling_total,CRQ_problemtalk_total) %>%
  mutate(Rehashing=CRQ_rehashing_total,
         Mulling=CRQ_mulling_total,
         Problem_Talk=CRQ_problemtalk_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("CRQ_",item))
CRQ_Wave3_totals_graph<-ggplot(CRQ_Wave3_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("CRQ total subscores for ",length(CRQ_Wave3_outdf$CRQ_total[!is.na(CRQ_Wave3_outdf$CRQ_total)])," participants"))

CRQ_Wave4_totals<-CRQ_Wave4_outdf %>%
  select(tagid,CRQ_rehashing_total,CRQ_mulling_total,CRQ_problemtalk_total) %>%
  mutate(Rehashing=CRQ_rehashing_total,
         Mulling=CRQ_mulling_total,
         Problem_Talk=CRQ_problemtalk_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("CRQ_",item))
CRQ_Wave4_totals_graph<-ggplot(CRQ_Wave4_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("CRQ total subscores for ",length(CRQ_Wave4_outdf$CRQ_total[!is.na(CRQ_Wave4_outdf$CRQ_total)])," participants"))

CRQ_Wave5_totals<-CRQ_Wave5_outdf %>%
  select(tagid,CRQ_rehashing_total,CRQ_mulling_total,CRQ_problemtalk_total) %>%
  mutate(Rehashing=CRQ_rehashing_total,
         Mulling=CRQ_mulling_total,
         Problem_Talk=CRQ_problemtalk_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("CRQ_",item))
CRQ_Wave5_totals_graph<-ggplot(CRQ_Wave5_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("CRQ total subscores for ",length(CRQ_Wave5_outdf$CRQ_total[!is.na(CRQ_Wave5_outdf$CRQ_total)])," participants"))

require(gridExtra)
grid.arrange(CRQ_Wave1_totals_graph, CRQ_Wave2_totals_graph, 
             CRQ_Wave3_totals_graph, CRQ_Wave4_totals_graph, 
             CRQ_Wave5_totals_graph, ncol=2)

M1 <- CRQ_Wave1_totals %>% filter(grepl("Mulling", item))
mean(M1$value, na.rm=T)
M2 <- CRQ_Wave2_totals %>% filter(grepl("Mulling", item))
mean(M2$value, na.rm=T)
M3 <- CRQ_Wave3_totals %>% filter(grepl("Mulling", item))
mean(M3$value, na.rm=T)
M4 <- CRQ_Wave4_totals %>% filter(grepl("Mulling", item))
mean(M4$value, na.rm=T)
M5 <- CRQ_Wave5_totals %>% filter(grepl("Mulling", item))
mean(M5$value, na.rm=T)

P1 <- CRQ_Wave1_totals %>% filter(grepl("Problem_Talk", item))
mean(P1$value, na.rm=T)
P2 <- CRQ_Wave2_totals %>% filter(grepl("Problem_Talk", item))
mean(P2$value, na.rm=T)
P3 <- CRQ_Wave3_totals %>% filter(grepl("Problem_Talk", item))
mean(P3$value, na.rm=T)
P4 <- CRQ_Wave4_totals %>% filter(grepl("Problem_Talk", item))
mean(P4$value, na.rm=T)
P5 <- CRQ_Wave5_totals %>% filter(grepl("Problem_Talk", item))
mean(P5$value, na.rm=T)

R1 <- CRQ_Wave1_totals %>% filter(grepl("Rehashing", item))
mean(R1$value, na.rm=T)
R2 <- CRQ_Wave2_totals %>% filter(grepl("Rehashing", item))
mean(R2$value, na.rm=T)
R3 <- CRQ_Wave3_totals %>% filter(grepl("Rehashing", item))
mean(R3$value, na.rm=T)
R4 <- CRQ_Wave4_totals %>% filter(grepl("Rehashing", item))
mean(R4$value, na.rm=T)
R5 <- CRQ_Wave5_totals %>% filter(grepl("Rehashing", item))
mean(R5$value, na.rm=T)

CRQ_Wave1_means<-CRQ_Wave1 %>%
  select(tagid,CRQ_rehashing_mean,CRQ_mulling_mean,CRQ_problemtalk_mean) %>%
  mutate(Rehashing=CRQ_rehashing_mean,
         Mulling=CRQ_mulling_mean,
         Problem_Talk=CRQ_problemtalk_mean) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("CRQ_",item))
CRQ_Wave1_means_graph<-ggplot(CRQ_Wave1_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("CRQ mean subscores for ",length(CRQ_Wave1$CRQ_mean[!is.na(CRQ_Wave1$CRQ_mean)])," participants"))

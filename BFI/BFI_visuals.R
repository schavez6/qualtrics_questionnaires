## Graph it
BFI10_Wave1_totals<-BFI10_Wave1_outdf %>%
  select(tagid,BFI10_neuro_total,BFI10_open_total,BFI10_consc_total,BFI10_agree_total,BFI10_extra_total) %>%
  mutate(Neuroticism=BFI10_neuro_total,
         Openness=BFI10_open_total,
         Conscientiousness=BFI10_consc_total,
         Agreeableness=BFI10_agree_total,
         Extraversion=BFI10_extra_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("BFI10",item))

BFI10_Wave1_totals_graph<-ggplot(BFI10_Wave1_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("BFI Wave1 total for ",length(BFI10_Wave1_outdf$tagid[!is.na(BFI10_Wave1_outdf$tagid)])," participants")) + ylim(0,0.4)

BFI10_Wave2_totals<-BFI10_Wave2_outdf %>%
  select(tagid,BFI10_neuro_total,BFI10_open_total,BFI10_consc_total,BFI10_agree_total,BFI10_extra_total) %>%
  mutate(Neuroticism=BFI10_neuro_total,
         Openness=BFI10_open_total,
         Conscientiousness=BFI10_consc_total,
         Agreeableness=BFI10_agree_total,
         Extraversion=BFI10_extra_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("BFI10",item))

BFI10_Wave2_totals_graph<-ggplot(BFI10_Wave2_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("BFI Wave2 total for ",length(BFI10_Wave2_outdf$tagid[!is.na(BFI10_Wave2_outdf$tagid)])," participants")) + ylim(0,0.3)

BFI10_Wave3_totals<-BFI10_Wave3_outdf %>%
  select(tagid,BFI10_neuro_total,BFI10_open_total,BFI10_consc_total,BFI10_agree_total,BFI10_extra_total) %>%
  mutate(Neuroticism=BFI10_neuro_total,
         Openness=BFI10_open_total,
         Conscientiousness=BFI10_consc_total,
         Agreeableness=BFI10_agree_total,
         Extraversion=BFI10_extra_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("BFI10",item))

BFI10_Wave3_totals_graph<-ggplot(BFI10_Wave3_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("BFI Wave3 total for ",length(BFI10_Wave3_outdf$tagid[!is.na(BFI10_Wave3_outdf$tagid)])," participants")) + ylim(0,0.4)

BFI10_Wave4_totals<-BFI10_Wave4_outdf %>%
  select(tagid,BFI10_neuro_total,BFI10_open_total,BFI10_consc_total,BFI10_agree_total,BFI10_extra_total) %>%
  mutate(Neuroticism=BFI10_neuro_total,
         Openness=BFI10_open_total,
         Conscientiousness=BFI10_consc_total,
         Agreeableness=BFI10_agree_total,
         Extraversion=BFI10_extra_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("BFI10",item))

BFI10_Wave4_totals_graph<-ggplot(BFI10_Wave4_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("BFI Wave4 total for ",length(BFI10_Wave4_outdf$tagid[!is.na(BFI10_Wave4_outdf$tagid)])," participants")) + ylim(0,0.3)

BFI10_Wave5_totals<-BFI10_Wave5_outdf %>%
  select(tagid,BFI10_neuro_total,BFI10_open_total,BFI10_consc_total,BFI10_agree_total,BFI10_extra_total) %>%
  mutate(Neuroticism=BFI10_neuro_total,
         Openness=BFI10_open_total,
         Conscientiousness=BFI10_consc_total,
         Agreeableness=BFI10_agree_total,
         Extraversion=BFI10_extra_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("BFI10",item))

BFI10_Wave5_totals_graph<-ggplot(BFI10_Wave5_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("BFI Wave5 total for ",length(BFI10_Wave5_outdf$tagid[!is.na(BFI10_Wave5_outdf$tagid)])," participants")) + ylim(0,0.3)

BFI10_Wave6_totals<-BFI10_Wave6_outdf %>%
  select(tagid,BFI10_neuro_total,BFI10_open_total,BFI10_consc_total,BFI10_agree_total,BFI10_extra_total) %>%
  mutate(Neuroticism=BFI10_neuro_total,
         Openness=BFI10_open_total,
         Conscientiousness=BFI10_consc_total,
         Agreeableness=BFI10_agree_total,
         Extraversion=BFI10_extra_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("BFI10",item))

BFI10_Wave6_totals_graph<-ggplot(BFI10_Wave6_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("BFI Wave6 total for ",length(BFI10_Wave6_outdf$tagid[!is.na(BFI10_Wave6_outdf$tagid)])," participants")) + ylim(0,0.3)

BFI10_Wave1_means<-BFI10_Wave1 %>%
  select(tagid,BFI10_neuro_mean,BFI10_open_mean,BFI10_consc_mean,BFI10_agree_mean,BFI10_extra_mean) %>%
  mutate(Neuroticism=BFI10_neuro_mean,
         Opennes=BFI10_open_mean,
         Conscientiousness=BFI10_consc_mean,
         Agreeableness=BFI10_agree_mean,
         Extraversion=BFI10_extra_mean) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("BFI10",item))
BFI10_Wave1_means_graph<-ggplot(BFI10_Wave1_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3) +
  ggtitle(paste0("BFI Wave1 means for ",length(BFI10_Wave1_means$tagid[!is.na(BFI10_Wave1_means$tagid)])," participants"))

require(gridExtra)
grid.arrange(BFI10_Wave1_totals_graph, BFI10_Wave2_totals_graph, 
             BFI10_Wave3_totals_graph, BFI10_Wave4_totals_graph, 
             BFI10_Wave5_totals_graph, BFI10_Wave6_totals_graph, ncol=2)

N1 <- BFI10_Wave1_totals %>% filter(grepl("Neuroticism", item))
mean(N1$value, na.rm=T)
N2 <- BFI10_Wave2_totals %>% filter(grepl("Neuroticism", item))
mean(N2$value, na.rm=T)
N3 <- BFI10_Wave3_totals %>% filter(grepl("Neuroticism", item))
mean(N3$value, na.rm=T)
N4 <- BFI10_Wave4_totals %>% filter(grepl("Neuroticism", item))
mean(N4$value, na.rm=T)
N5 <- BFI10_Wave5_totals %>% filter(grepl("Neuroticism", item))
mean(N5$value, na.rm=T)
N6 <- BFI10_Wave6_totals %>% filter(grepl("Neuroticism", item))
mean(N6$value, na.rm=T)

E1 <- BFI10_Wave1_totals %>% filter(grepl("Extraversion", item))
mean(E1$value, na.rm=T)
E2 <- BFI10_Wave2_totals %>% filter(grepl("Extraversion", item))
mean(E2$value, na.rm=T)
E3 <- BFI10_Wave3_totals %>% filter(grepl("Extraversion", item))
mean(E3$value, na.rm=T)
E4 <- BFI10_Wave4_totals %>% filter(grepl("Extraversion", item))
mean(E4$value, na.rm=T)
E5 <- BFI10_Wave5_totals %>% filter(grepl("Extraversion", item))
mean(E5$value, na.rm=T)
E6 <- BFI10_Wave6_totals %>% filter(grepl("Extraversion", item))
mean(E6$value, na.rm=T)

A1 <- BFI10_Wave1_totals %>% filter(grepl("Agreeableness", item))
mean(A1$value, na.rm=T)
A2 <- BFI10_Wave2_totals %>% filter(grepl("Agreeableness", item))
mean(A2$value, na.rm=T)
A3 <- BFI10_Wave3_totals %>% filter(grepl("Agreeableness", item))
mean(A3$value, na.rm=T)
A4 <- BFI10_Wave4_totals %>% filter(grepl("Agreeableness", item))
mean(A4$value, na.rm=T)
A5 <- BFI10_Wave5_totals %>% filter(grepl("Agreeableness", item))
mean(A5$value, na.rm=T)
A6 <- BFI10_Wave6_totals %>% filter(grepl("Agreeableness", item))
mean(A6$value, na.rm=T)

C1 <- BFI10_Wave1_totals %>% filter(grepl("Conscientiousness", item))
mean(C1$value, na.rm=T)
C2 <- BFI10_Wave2_totals %>% filter(grepl("Conscientiousness", item))
mean(C2$value, na.rm=T)
C3 <- BFI10_Wave3_totals %>% filter(grepl("Conscientiousness", item))
mean(C3$value, na.rm=T)
C4 <- BFI10_Wave4_totals %>% filter(grepl("Conscientiousness", item))
mean(C4$value, na.rm=T)
C5 <- BFI10_Wave5_totals %>% filter(grepl("Conscientiousness", item))
mean(C5$value, na.rm=T)
C6 <- BFI10_Wave6_totals %>% filter(grepl("Conscientiousness", item))
mean(C6$value, na.rm=T)

O1 <- BFI10_Wave1_totals %>% filter(grepl("Openness", item))
mean(O1$value, na.rm=T)
O2 <- BFI10_Wave2_totals %>% filter(grepl("Openness", item))
mean(O2$value, na.rm=T)
O3 <- BFI10_Wave3_totals %>% filter(grepl("Openness", item))
mean(O3$value, na.rm=T)
O4 <- BFI10_Wave4_totals %>% filter(grepl("Openness", item))
mean(O4$value, na.rm=T)
O5 <- BFI10_Wave5_totals %>% filter(grepl("Openness", item))
mean(O5$value, na.rm=T)
O6 <- BFI10_Wave6_totals %>% filter(grepl("Openness", item))
mean(O6$value, na.rm=T)

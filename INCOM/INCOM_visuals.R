## Graph it
INCOM_Wave3_totals<-INCOM_Wave3_outdf %>%
  select(tagid,INCOM_abilities_total,INCOM_opinions_total) %>%
  mutate(Abilities=INCOM_abilities_total,
         Opinions=INCOM_opinions_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("INCOM",item))

INCOM_Wave3_totals_graph<-ggplot(INCOM_Wave3_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("INCOM Wave3 total for ",length(INCOM_Wave3_outdf$tagid[!is.na(INCOM_Wave3_outdf$tagid)])," participants"))

INCOM_Wave4_totals<-INCOM_Wave4_outdf %>%
  select(tagid,INCOM_abilities_total,INCOM_opinions_total) %>%
  mutate(Abilities=INCOM_abilities_total,
         Opinions=INCOM_opinions_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("INCOM",item))

INCOM_Wave4_totals_graph<-ggplot(INCOM_Wave4_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("INCOM Wave4 total for ",length(INCOM_Wave4_outdf$tagid[!is.na(INCOM_Wave4_outdf$tagid)])," participants"))

INCOM_Wave5_totals<-INCOM_Wave5_outdf %>%
  select(tagid,INCOM_abilities_total,INCOM_opinions_total) %>%
  mutate(Abilities=INCOM_abilities_total,
         Opinions=INCOM_opinions_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("INCOM",item))

INCOM_Wave5_totals_graph<-ggplot(INCOM_Wave5_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("INCOM Wave5 total for ",length(INCOM_Wave5_outdf$tagid[!is.na(INCOM_Wave5_outdf$tagid)])," participants"))

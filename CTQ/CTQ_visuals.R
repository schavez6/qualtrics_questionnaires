## Graph it
CTQ_Wave1_totals<-CTQ_Wave1_outdf %>%
  select(tagid,CTQ_emotionalabuse_total,CTQ_emotionalneglect_total,CTQ_physicalabuse_total,
         CTQ_physicalneglect_total,CTQ_sexualabuse_total,CTQ_validity_total) %>%
  mutate(Emotional_Abuse=CTQ_emotionalabuse_total,
         Emotional_Neglect=CTQ_emotionalneglect_total,
         Physical_Abust=CTQ_physicalabuse_total,
         Physical_Neglect=CTQ_physicalneglect_total,
         Sexual_Abuse=CTQ_sexualabuse_total,
         Validity=CTQ_validity_total) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("CTQ_",item))
CTQ_Wave1_totals_graph<-ggplot(CTQ_Wave1_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("CTQ total subscores for ",length(unique(CTQ_Wave1_outdf$tagid[!is.na(CTQ_Wave1_outdf$tagid)]))," participants"))

CTQ_Wave1_means<-CTQ_Wave1 %>%
  select(tagid,CTQ_emotionalabuse_mean,CTQ_emotionalneglect_mean,CTQ_physicalabuse_mean,
         CTQ_physicalneglect_mean,CTQ_sexualabuse_mean,CTQ_validity_mean) %>%
  mutate(Emotional_Abuse=CTQ_emotionalabuse_mean,
         Emotional_Neglect=CTQ_emotionalneglect_mean,
         Physical_Abust=CTQ_physicalabuse_mean,
         Physical_Neglect=CTQ_physicalneglect_mean,
         Sexual_Abuse=CTQ_sexualabuse_mean,
         Validity=CTQ_validity_mean) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("CTQ_",item))
CTQ_Wave1_means_graph<-ggplot(CTQ_Wave1_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("CTQ mean subscores for ",length(unique(CTQ_Wave1$tagid[!is.na(CTQ_Wave1$tagid)]))," participants"))

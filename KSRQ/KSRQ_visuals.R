# File not ready for use yet...
## Graph it
KSRQ_Wave1_means<-KSRQ_Wave1_outdf %>%
  select(tagid, KSRQ_adm_mean,KSRQ_negsoc_mean,KSRQ_pass_mean,KSRQ_prosoc_mean,
         KSRQ_sex_mean,KSRQ_social_mean) %>%
  mutate(Admiration=KSRQ_adm_mean,
         Negative_Social_Potency=KSRQ_negsoc_mean,
         Passivity=KSRQ_pass_mean,
         Prosocial_Interactions=KSRQ_prosoc_mean,
         Sexual_Relationships=KSRQ_sex_mean,
         Sociability=KSRQ_social_mean) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("KSRQ_",item)) 
KSRQ_Wave1_means_graph<-ggplot(KSRQ_Wave1_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("KSRQ W1 mean subscores for ",length(unique(KSRQ_Wave1_outdf$tagid[!is.na(KSRQ_Wave1_outdf$tagid)]))," participants"))

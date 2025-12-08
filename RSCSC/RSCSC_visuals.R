## Graph it
RSCS_Wave1_totals<-RSCS_Wave1_outdf %>% 
  select(tagid,RSCS_publicsc_total,RSCS_privatesc_total,RSCS_socanx_total) %>%
  mutate(Public_Self_Consicousness=RSCS_publicsc_total,
         Private_Self_Consicousness=RSCS_privatesc_total,
         Social_Anxiousness=RSCS_socanx_total) %>%
  gather('item','value',2:length(.)) %>% 
  filter(!grepl("R_SCS_C",item)) %>% 
  filter(!grepl("RSCS_",item))
RSCS_Wave1_totals_graph<-ggplot(RSCS_Wave1_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("RSCS W1 totals for ",length(unique(RSCS_Wave1_outdf$tagid[!is.na(RSCS_Wave1_outdf$tagid)]))," participants"))

RSCS_Wave2_totals<-RSCS_Wave2_outdf %>% 
  select(tagid,RSCS_publicsc_total,RSCS_privatesc_total,RSCS_socanx_total) %>%
  mutate(Public_Self_Consicousness=RSCS_publicsc_total,
         Private_Self_Consicousness=RSCS_privatesc_total,
         Social_Anxiousness=RSCS_socanx_total) %>%
  gather('item','value',2:length(.)) %>% 
  filter(!grepl("R_SCS_C",item)) 
RSCS_Wave2_totals_graph<-ggplot(RSCS_Wave2_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("RSCS W2 totals for ",length(unique(RSCS_Wave2_outdf$tagid[!is.na(RSCS_Wave2_outdf$tagid)]))," participants"))

RSCS_Wave1_totals<-RSCS_Wave1_outdf %>% 
  select(tagid,RSCS_publicsc_total,RSCS_privatesc_total,RSCS_socanx_total) %>%
  mutate(tagid=tagid, 
         Public_Self_Consicousness=RSCS_publicsc_total,
         Private_Self_Consicousness=RSCS_privatesc_total,
         Social_Anxiousness=RSCS_socanx_total) %>%
  gather('item','value',2:length(.)) %>% 
  filter(!grepl("R_SCS_C",item)) 
RSCS_Wave1_totals_graph<-ggplot(RSCS_Wave1_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("RSCS W1 totals for ",length(unique(RSCS_Wave1_outdf$tagid[!is.na(RSCS_Wave1_outdf$tagid)]))," participants"))


RSCS_Wave1_means<-RSCS_Wave1 %>%
  select(tagid,RSCS_publicsc_mean,RSCS_privatesc_mean,RSCS_socanx_mean) %>%
  mutate(Public_Self_Consicousness=RSCS_publicsc_mean,
         Private_Self_Consicousness=RSCS_privatesc_mean,
         Social_Anxiousness=RSCS_socanx_mean) %>%
  select(-contains("RSCS")) %>%
  gather('item','value',2:length(.)) 
RSCS_Wave1_means_graph<-ggplot(RSCS_Wave1_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("RSCS means for ",length(unique(RSCS_Wave1$tagid))," participants"))

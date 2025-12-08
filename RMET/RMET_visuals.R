#File under construction - not yet ready for use
## Graph it
RMET_Wave1_percentcorrect<-RMET_Wave1 %>%
  select(tagid,RMET_Percent_Correct) %>%
  gather('item','value',2:length(.)) 
RMET_Wave1_percentcorrect_graph<-ggplot(RMET_Wave1_percentcorrect, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("RMET Percent Correct for ",length(unique(RMET_Wave1$tagid[!is.na(RMET_Wave1$RMET_Percent_Correct)]))," participants"))

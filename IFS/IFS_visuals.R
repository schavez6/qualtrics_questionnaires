## Graph it
IFS_Wave1_totals<-IFS_cleaned %>%
  select(tagid,IFS_frankness_total,IFS_trust_total,IFS_common_total,
         IFS_impose_total,IFS_sharing_total,IFS_exclusiveness_total,
         IFS_attachment_total,IFS_sensitivity_total) %>%
  mutate(Frankness_Spontaneity=IFS_frankness_total,
         Trust_Loyalty=IFS_trust_total,
         Common_Activities=IFS_common_total,
         Imposition=IFS_impose_total,
         Giving_Sharing=IFS_sharing_total,
         Exclusiveness=IFS_exclusiveness_total,
         Sensitivity_Knowing=IFS_sensitivity_total,
         Attachment=IFS_attachment_total) %>%
  select(-contains("IFS")) %>%
  gather('item','value',2:length(.)) 
IFS_Wave1_totals_graph<-ggplot(IFS_Wave1_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("IFS totals for ",length(unique(IFS_cleaned$tagid[!is.na(IFS_cleaned$IFS_attachment_total)]))," participants"))

IFS_Wave1_means<-IFS_cleaned %>%
  select(tagid,IFS_frankness_mean,IFS_trust_mean,IFS_common_mean,
         IFS_impose_mean,IFS_sharing_mean,IFS_exclusiveness_mean,
         IFS_attachment_mean,IFS_sensitivity_mean) %>%
  mutate(Frankness_Spontaneity=IFS_frankness_mean,
         Trust_Loyalty=IFS_trust_mean,
         Common_Activities=IFS_common_mean,
         Imposition=IFS_impose_mean,
         Giving_Sharing=IFS_sharing_mean,
         Exclusiveness=IFS_exclusiveness_mean,
         Sensitivity_Knowing=IFS_sensitivity_mean,
         Attachment=IFS_attachment_mean) %>%
  select(-contains("IFS")) %>%
  gather('item','value',2:length(.)) 
IFS_Wave1_means_graph<-ggplot(IFS_Wave1_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("IFS means for ",length(unique(IFS_cleaned$tagid[!is.na(IFS_cleaned$IFS_attachment_mean)]))," participants"))

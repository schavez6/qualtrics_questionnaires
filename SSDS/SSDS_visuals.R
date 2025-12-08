## Graph it
SSDS_Wave1_totals<-SSDS_Wave1_outdf %>%
  select(tagid,SSDS_family_disclosure_total,SSDS_friend_disclosure_total,
         SSDS_physdev_disclosure_total) %>%
  mutate(Family_Disclosure=SSDS_family_disclosure_total,
         Friend_Disclosure=SSDS_friend_disclosure_total,
         Physical_Development_Disclosure=SSDS_physdev_disclosure_total) %>%
  select(-contains("SSDS")) %>%
  gather('item','value',2:length(.)) 
SSDS_Wave1_totals_graph<-ggplot(SSDS_Wave1_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Friendship disclosure subscale totals for ",length(unique(SSDS_Wave1_outdf$tagid[!is.na(SSDS_Wave1_outdf$SSDS_friend_disclosure_total)]))," participants"))

SSDS_Wave1_means<-SSDS_Wave1 %>%
  select(tagid,SSDS_1,SSDS_family_disclosure_mean,SSDS_friend_disclosure_mean,
         SSDS_physdev_disclosure_mean) %>%
  mutate(#Self=SSDS_1, NV: deleted this from coding above, since it's not a valid item.
         Family_Disclosure=SSDS_family_disclosure_mean,
         Friend_Disclosure=SSDS_friend_disclosure_mean,
         Physical_Development_Disclosure=SSDS_physdev_disclosure_mean) %>%
  select(-contains("SSDS")) %>%
  gather('item','value',2:length(.)) 
SSDS_Wave1_means_graph<-ggplot(SSDS_Wave1_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Friendship disclosure subscale means for ",length(unique(SSDS_Wave1$tagid[!is.na(SSDS_Wave1$SSDS_family_disclosure_mean)]))," participants"))

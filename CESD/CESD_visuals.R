## Graph it #note - this script is in progress... (parent-report data only_
CESD_parent_Wave1_totals_graph<-ggplot(CESD_parent_Wave1, aes(x=CES_D_total, colour="deeppink")) +
  geom_density(alpha=.3,show.legend = FALSE)+
  ggtitle(paste0("CESD-Parent total scores for ",length(CESD_parent_Wave1$CES_D_total[!is.na(CESD_parent_Wave1$CES_D_total)])," Wave 1 participants"))

CESD_parent_Wave1_means_graph<-ggplot(CESD_parent_Wave1, aes(x=CES_D_mean, colour="dodgerblue")) +
  geom_density(alpha=.3, show.legend = FALSE)+
  ggtitle(paste0("CESD-Parent mean scores for ",length(CESD_parent_Wave1$CES_D_mean[!is.na(CESD_parent_Wave1$CES_D_mean)])," Wave 1 participants"))

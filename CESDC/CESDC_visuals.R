## Graph it
#graph prep
cesdc_age <- left_join(CESDC_alldata,dob, by="tagid")
cesdc_age$dob <- as.Date(cesdc_age$dob, "%Y-%m-%d")
cesdc_age$ageday <- difftime(cesdc_age$survey_date, cesdc_age$dob, units = "days")
cesdc_age$age <- round(as.numeric(cesdc_age$ageday/365),10)

#long graph
cesdcmean <- ggplot(aes(age, CES_DC_mean, colour=factor(tagid)), data=cesdc_age) + 
  geom_smooth(se=F) + guides(colour=F) + ylim(0,3)

cesdcmean2 <- cesdcmean + geom_smooth(aes(group = 1), method="loess", formula=y~x, color="black")

cesdctotal <- ggplot(aes(age, CES_DC_total, colour=factor(tagid)), data=cesdc_age) + 
  geom_smooth(se=F, linewidth = 0.1) + geom_point(size = 0.5, shape = 17) + guides(colour=F) 

cesdctotal2 <- cesdctotal + geom_smooth(aes(group = 1), method="loess", formula=y~x, color="black", linewidth=1) + scale_y_continuous(breaks = seq(0,60,5), limits = c(0,60)) + 
  geom_hline(color="blue",yintercept = 15, linewidth=1) + 
  scale_x_continuous(breaks = seq(10,21,1)) + ylab("CES-DC Total Score") + xlab("Age") + ggtitle("Depression is Part of Normative Development in Teenage Girls", subtitle = "Blue=Clinical Cutoff Score") + theme(plot.subtitle = element_text(color="blue"))

cesdctotaldate <- ggplot(aes(survey_date, CES_DC_total, colour=factor(tagid)), data=cesdc_age) + 
  geom_smooth(se=F, linewidth = 0.1) + geom_point(size = 0.5, shape = 17) + guides(colour=F) 

cesdctotaldate2 <- cesdctotaldate + geom_smooth(aes(group = 1), method="loess", formula=y~x, color="black", linewidth=1) + scale_y_continuous(breaks = seq(0,60,5), limits = c(0,60)) + 
  geom_hline(color="blue",yintercept = 15, linewidth=1) + 
  geom_vline(color="red",xintercept = as.Date("2020-03-23"), linewidth=1) + ylab("CES-DC Total Score") + xlab("Calendar Date") + ggtitle("No, Covid Doesn't Explain this Phenomenon...", subtitle = "Red=Oregon Governor Issues Statewide Stay-at-Home Order") + theme(plot.subtitle = element_text(color="red"))
#+ 
 # scale_x_continuous(breaks = seq(10,21,1))

grid.arrange(cesdctotal2,cesdctotaldate2)

CESDC_Wave1_totals_graph<-ggplot(CESDC_Wave1_outdf, aes(x=CES_DC_total, colour="deeppink")) +
  geom_density(alpha=.3,show.legend = FALSE)+
  ggtitle(paste0("CESD-C total scores for ",length(CESDC_Wave1_outdf$CES_DC_total[!is.na(CESDC_Wave1_outdf$CES_DC_total)])," Wave 1 participants"))

CESDC_Wave2_totals_graph<-ggplot(CESDC_Wave2_outdf, aes(x=CES_DC_total, colour="deeppink")) +
  geom_density(alpha=.3,show.legend = FALSE)+
  ggtitle(paste0("CESD-C total scores for ",length(CESDC_Wave2_outdf$CES_DC_total[!is.na(CESDC_Wave2_outdf$CES_DC_total)])," Wave 2 participants"))

CESDC_Wave3_totals_graph<-ggplot(CESDC_Wave3_outdf, aes(x=CES_DC_total, colour="deeppink")) +
  geom_density(alpha=.3,show.legend = FALSE)+
  ggtitle(paste0("CESD-C total scores for ",length(CESDC_Wave3_outdf$CES_DC_total[!is.na(CESDC_Wave3_outdf$CES_DC_total)])," Wave 3 participants"))

CESDC_Wave4_totals_graph<-ggplot(CESDC_Wave4_outdf, aes(x=CES_DC_total, colour="deeppink")) +
  geom_density(alpha=.3,show.legend = FALSE)+
  ggtitle(paste0("CESD-C total scores for ",length(CESDC_Wave4_outdf$CES_DC_total[!is.na(CESDC_Wave4_outdf$CES_DC_total)])," Wave 4 participants"))

CESDC_Wave5_totals_graph<-ggplot(CESDC_Wave5_outdf, aes(x=CES_DC_total, colour="deeppink")) +
  geom_density(alpha=.3,show.legend = FALSE)+
  ggtitle(paste0("CESD-C total scores for ",length(CESDC_Wave5_outdf$CES_DC_total[!is.na(CESDC_Wave5_outdf$CES_DC_total)])," Wave 5 participants"))

require(gridExtra)
grid.arrange(CESDC_Wave1_totals_graph, CESDC_Wave2_totals_graph, 
             CESDC_Wave3_totals_graph, CESDC_Wave4_totals_graph, 
             CESDC_Wave5_totals_graph, ncol=2)

cesdc_change_12 <- merge(CESDC_Wave1_outdf, CESDC_Wave2_outdf, by = "tagid", all = T) %>% 
  dplyr::select(tagid,CES_DC_total.x,CES_DC_total.y)
cesdc_change_13 <- merge(CESDC_Wave1_outdf, CESDC_Wave3_outdf, by = "tagid", all = T) %>% 
  dplyr::select(tagid,CES_DC_total.x,CES_DC_total.y)
cesdc_change_14 <- merge(CESDC_Wave1_outdf, CESDC_Wave4_outdf, by = "tagid", all = T) %>% 
  dplyr::select(tagid,CES_DC_total.x,CES_DC_total.y)

cor.test(cesdc_change_12$CES_DC_total.x, cesdc_change_12$CES_DC_total.y, use = "pairwise.complete.obs", method = "pearson")
cor.test(cesdc_change_12$CES_DC_total.x, cesdc_change_12$CES_DC_total.y, use = "pairwise.complete.obs", method = "spearman")

cor.test(cesdc_change_13$CES_DC_total.x, cesdc_change_13$CES_DC_total.y, use = "pairwise.complete.obs", method = "pearson")
cor.test(cesdc_change_13$CES_DC_total.x, cesdc_change_13$CES_DC_total.y, use = "pairwise.complete.obs", method = "spearman")

cor.test(cesdc_change_14$CES_DC_total.x, cesdc_change_14$CES_DC_total.y, use = "pairwise.complete.obs", method = "pearson")
cor.test(cesdc_change_14$CES_DC_total.x, cesdc_change_14$CES_DC_total.y, use = "pairwise.complete.obs", method = "spearman")

CESDC_Wave1_means_graph<-ggplot(CESDC_Wave1_outdf, aes(x=CES_DC_mean, colour="dodgerblue")) +
  geom_density(alpha=.3, show.legend = FALSE)+
  ggtitle(paste0("CESD-C mean scores for ",length(CESDC_Wave1_outdf$CES_DC_mean[!is.na(CESDC_Wave1_outdf$CES_DC_mean)])," Wave 1 participants"))

CESDC_Wave2_means_graph<-ggplot(CESDC_Wave2_outdf, aes(x=CES_DC_mean, colour="dodgerblue")) +
  geom_density(alpha=.3, show.legend = FALSE)+
  ggtitle(paste0("CESD-C mean scores for ",length(CESDC_Wave2_outdf$CES_DC_mean[!is.na(CESDC_Wave2_outdf$CES_DC_mean)])," Wave 2 participants"))

CESDC_Wave3_means_graph<-ggplot(CESDC_Wave3_outdf, aes(x=CES_DC_mean, colour="dodgerblue")) +
  geom_density(alpha=.3, show.legend = FALSE)+
  ggtitle(paste0("CESD-C mean scores for ",length(CESDC_Wave3_outdf$CES_DC_mean[!is.na(CESDC_Wave3_outdf$CES_DC_mean)])," Wave 3 participants"))

CESDC_Wave4_means_graph<-ggplot(CESDC_Wave4_outdf, aes(x=CES_DC_mean, colour="dodgerblue")) +
  geom_density(alpha=.3, show.legend = FALSE)+
  ggtitle(paste0("CESD-C mean scores for ",length(CESDC_Wave4_outdf$CES_DC_mean[!is.na(CESDC_Wave4_outdf$CES_DC_mean)])," Wave 4 participants"))

require(gridExtra)
grid.arrange(CESDC_Wave1_means_graph, CESDC_Wave2_means_graph, 
             CESDC_Wave3_means_graph, CESDC_Wave4_means_graph, ncol=2)

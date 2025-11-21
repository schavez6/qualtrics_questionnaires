## Graph it
#graph prep
pbip_age <- left_join(PBIP_alldata,dob, by="tagid")
pbip_age$dob <- as.Date(pbip_age$dob, "%Y-%m-%d")
pbip_age$ageday <- difftime(pbip_age$survey_date, pbip_age$dob, units = "days")
pbip_age$age <- round(as.numeric(pbip_age$ageday/365),10)

#long graph
stage <- ggplot(aes(age, as.numeric(stage), colour=factor(tagid)), data=pbip_age) + 
  geom_smooth(se=F, linewidth=0.1, fullrange=T, method = 'glm', family = "binomial") + guides(colour=F)

stage2 <- stage + geom_smooth(aes(group = 1), color="black") + 
  scale_y_continuous(breaks = seq(1,5,.5), limits = c(1,6)) + scale_x_continuous(breaks = seq(10,21,1)) + 
  ylab("PBIP Stage") + xlab("Age") + ggtitle("Pubertal Development in TAG Sample (PBIP)")


PBIP_Wave1_pbip_graph<-ggplot(PBIP_Wave1_outdf, aes(x=stage, colour="deeppink")) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Tanner stage for ",length(PBIP_Wave1_outdf$stage[!is.na(PBIP_Wave1_outdf$stage)])," Wave 1 participants"))

PBIP_Wave2_pbip_graph<-ggplot(PBIP_Wave2_outdf, aes(x=stage, colour="deeppink")) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Tanner stage for ",length(PBIP_Wave2_outdf$stage[!is.na(PBIP_Wave2_outdf$stage)])," Wave 2 participants"))

PBIP_Wave3_pbip_graph<-ggplot(PBIP_Wave3_outdf, aes(x=stage, colour="deeppink")) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Tanner stage for ",length(PBIP_Wave3_outdf$stage[!is.na(PBIP_Wave3_outdf$stage)])," Wave 3 participants"))

PBIP_Wave4_pbip_graph<-ggplot(PBIP_Wave4_outdf, aes(x=stage, colour="deeppink")) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Tanner stage for ",length(PBIP_Wave4_outdf$stage[!is.na(PBIP_Wave4_outdf$stage)])," Wave 4 participants"))

PBIP_Wave5_pbip_graph<-ggplot(PBIP_Wave5_outdf, aes(x=stage, colour="deeppink")) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Tanner stage for ",length(PBIP_Wave5_outdf$stage[!is.na(PBIP_Wave5_outdf$stage)])," Wave 5 participants"))

grid.arrange(PBIP_Wave1_pbip_graph, PBIP_Wave2_pbip_graph, 
             PBIP_Wave3_pbip_graph, PBIP_Wave4_pbip_graph, 
             PBIP_Wave5_pbip_graph, ncol=2)

options(scipen=999)
PBIP_PDS_Wave1<-left_join(PBIP_Wave1_outdf,read.csv(paste0(workdir,"Qualtrics_Output/Wave1/PDS_Wave1.csv"),header=TRUE,stringsAsFactors = FALSE))
PBIP_Wave1_pbip_by_pds_graph<-ggplot(PBIP_PDS_Wave1, aes(x=pdss,y=stage)) +
  geom_point(show.legend = FALSE)+
  ggtitle(paste0("PDSS by age for ",length(PBIP_PDS_Wave1$stage[!is.na(PBIP_PDS_Wave1$stage)])," Wave 1 participants. r = ",
                 round(cor.test(PBIP_PDS_Wave1$stage,PBIP_PDS_Wave1$pdss,use=na.or.complete)[[4]],3)," p = ",
                 cor.test(PBIP_PDS_Wave1$stage,PBIP_PDS_Wave1$pdss,use=na.or.complete)[[3]]))

## Graph it #note - this is self-report data
#graph prep
dob <- redcap_cleaned %>% select(dob,tagid) %>% unique()
pds_age <- left_join(PDS_alldata,dob, by="tagid")
pds_age$dob <- as.Date(pds_age$dob, "%Y-%m-%d")
pds_age$ageday <- difftime(pds_age$survey_date, pds_age$dob, units = "days")
pds_age$age <- round(as.numeric(pds_age$ageday/365),10)

#long graph
adren <- ggplot(aes(age, adrenf, colour=factor(tagid)), data=pds_age) + 
  geom_smooth(formula = y~ log(x),se=F) + guides(colour=F) + ylim(1,5)

a2 <- adren + geom_smooth(aes(group = 1), method="loess", formula=y~log(x), color="black")

gonad <- ggplot(aes(age, gonadf, colour=factor(tagid)), data=pds_age) + 
  geom_smooth(formula = y~ log(x),se=F) + guides(colour=F) + ylim(1,5)

g2 <- gonad + geom_smooth(aes(group = 1), method="loess", formula=y~log(x), color="black")

pdss <- ggplot(aes(age, pdss, colour=factor(tagid)), data=pds_age) + 
  geom_smooth(formula = y~ log(x),se=F) + guides(colour=F) + ylim(1,5)

pdss2 <- pdss + geom_smooth(aes(group = 1), method="loess", formula=y~log(x), color="black")

library(gridExtra)
grid.arrange(a2,g2)

pdsf1 <- ggplot(aes(age, PDS_F1, colour=factor(tagid)), data=pds_age) + 
  geom_smooth(formula = y~ log(x),se=F) + guides(colour=F) + ylim(1,4)

height <- pdsf1 + geom_smooth(aes(group = 1), method="loess", formula=y~log(x), color="black")

pdsf2 <- ggplot(aes(age, PDS_F2, colour=factor(tagid)), data=pds_age) + 
  geom_smooth(formula = y~ log(x),se=F) + guides(colour=F) + ylim(1,4)

bodyhair <- pdsf2 + geom_smooth(aes(group = 1), method="loess", formula=y~log(x), color="black")

pdsf3 <- ggplot(aes(age, PDS_F3, colour=factor(tagid)), data=pds_age) + 
  geom_smooth(formula = y~ log(x),se=F) + guides(colour=F) + ylim(1,4)

skinchange <- pdsf3 + geom_smooth(aes(group = 1), method="loess", formula=y~log(x), color="black")

pdsf4 <- ggplot(aes(age, PDS_F4, colour=factor(tagid)), data=pds_age) + 
  geom_smooth(formula = y~ log(x),se=F) + guides(colour=F) + ylim(1,4)

breast <- pdsf4 + geom_smooth(aes(group = 1), method="loess", formula=y~log(x), color="black")

pdsf5 <- ggplot(aes(age, PDS_F5, colour=factor(tagid)), data=pds_age) + 
  #geom_smooth(formula = y~ log(x),se=F) 
geom_smooth(se=F) + guides(colour=F) + ylim(1,4)

peercomp <- pdsf5 + geom_smooth(aes(group = 1), method="loess", color="black")

PDS_Wave1_pdss_graph<-ggplot(PDS_Wave1_outdf, aes(x=pdss, colour="deeppink")) +
  geom_density(alpha=.3)+
  ggtitle(paste0("PDSS scores for ",length(PDS_Wave1_outdf$pdss[!is.na(PDS_Wave1_outdf$pdss)])," Wave 1 participants"))

PDS_Wave2_pdss_graph<-ggplot(PDS_Wave2_outdf, aes(x=pdss, colour="deeppink")) +
  geom_density(alpha=.3)+
  ggtitle(paste0("PDSS scores for ",length(PDS_Wave2_outdf$pdss[!is.na(PDS_Wave2_outdf$pdss)])," Wave 2 participants"))

PDS_Wave3_pdss_graph<-ggplot(PDS_Wave3_outdf, aes(x=pdss, colour="deeppink")) +
  geom_density(alpha=.3)+
  ggtitle(paste0("PDSS scores for ",length(PDS_Wave3_outdf$pdss[!is.na(PDS_Wave3_outdf$pdss)])," Wave 3 participants"))

PDS_Wave4_pdss_graph<-ggplot(PDS_Wave4_outdf, aes(x=pdss, colour="deeppink")) +
  geom_density(alpha=.3)+
  ggtitle(paste0("PDSS scores for ",length(PDS_Wave4_outdf$pdss[!is.na(PDS_Wave4_outdf$pdss)])," Wave 4 participants"))

PDS_Wave5_pdss_graph<-ggplot(PDS_Wave5_outdf, aes(x=pdss, colour="deeppink")) +
  geom_density(alpha=.3)+
  ggtitle(paste0("PDSS scores for ",length(PDS_Wave5_outdf$pdss[!is.na(PDS_Wave5_outdf$pdss)])," Wave 5 participants"))

options(scipen=999)
PDS_withage<-(left_join(PDS_Wave1,redcap_cleaned,by="tagid") %>%
                filter(!is.na(survey_date),!survey_date=="")%>%
                mutate(age=round((interval(start =dob, end = survey_date) / duration(num = 1, units = "years")),2)))
PDS_Wave1_pdss_by_age_graph<-ggplot(PDS_withage, aes(x=age,y=pdss)) +
  geom_point(show.legend = FALSE)+
  ggtitle(paste0("PDSS by age for ",length(PDS_withage$pdss[!is.na(PDS_withage$pdss)])," Wave 1 participants. r = ",
                 round(cor.test(PDS_withage$age,PDS_withage$pdss,use=na.or.complete)[[4]],3)," p = ",
                 cor.test(PDS_withage$age,PDS_withage$pdss,use=na.or.complete)[[3]]))

require(gridExtra)
grid.arrange(PDS_Wave1_pdss_graph, PDS_Wave2_pdss_graph, 
             PDS_Wave3_pdss_graph, PDS_Wave4_pdss_graph, 
             PDS_Wave5_pdss_graph, ncol=2)

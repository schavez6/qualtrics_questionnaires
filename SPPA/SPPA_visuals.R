#note: some of this is dependent on other "selfie" questionnaires and demographics data that must be aggregated first before this script will work... not ready for prime time
## Graph it
SPPA_Wave1_totals<-SPPA_Wave1_outdf %>%
  select(tagid,SPPA_scholasticcomp_total,SPPA_socialcomp_total,SPPA_athleticcomp_total,
         SPPA_physicalappear_total,SPPA_behavconduct_total,SPPA_closefriend_total,
         SPPA_globalselfworth_total) %>%
  mutate(Scholastic_Competence=SPPA_scholasticcomp_total,
         Social_Competence=SPPA_socialcomp_total,
         Athletic_Competence=SPPA_athleticcomp_total,
         Physical_Appearance=SPPA_physicalappear_total,
         Behavioral_Conduct=SPPA_behavconduct_total,
         Close_Friendship=SPPA_closefriend_total,
         Global_Self_Worth=SPPA_globalselfworth_total) %>%
  select(-contains("SPPA")) %>%
  gather('item','value',2:length(.)) 
SPPA_Wave1_totals_graph<-ggplot(SPPA_Wave1_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Wave 1 SPPA total subscores for ",length(unique(SPPA_Wave1_outdf$tagid[!is.na(SPPA_Wave1_outdf$SPPA_scholasticcomp_total)]))," participants")) + geom_vline(aes(xintercept = median(SPPA_Wave1_outdf$SPPA_globalselfworth_total, na.rm=T), colour="Global_Self_Worth"))

SPPA_Wave2_totals<-SPPA_Wave2_outdf %>%
  select(tagid,SPPA_scholasticcomp_total,SPPA_socialcomp_total,SPPA_athleticcomp_total,
         SPPA_physicalappear_total,SPPA_behavconduct_total,SPPA_closefriend_total,
         SPPA_globalselfworth_total) %>%
  mutate(Scholastic_Competence=SPPA_scholasticcomp_total,
         Social_Competence=SPPA_socialcomp_total,
         Athletic_Competence=SPPA_athleticcomp_total,
         Physical_Appearance=SPPA_physicalappear_total,
         Behavioral_Conduct=SPPA_behavconduct_total,
         Close_Friendship=SPPA_closefriend_total,
         Global_Self_Worth=SPPA_globalselfworth_total) %>%
  select(-contains("SPPA")) %>%
  gather('item','value',2:length(.)) 
SPPA_Wave2_totals_graph<-ggplot(SPPA_Wave2_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+ 
  ggtitle(paste0("Wave 2 SPPA total subscores for ",length(unique(SPPA_Wave2_outdf$tagid[!is.na(SPPA_Wave2_outdf$SPPA_scholasticcomp_total)]))," participants")) + geom_vline(aes(xintercept = median(SPPA_Wave2_outdf$SPPA_globalselfworth_total, na.rm=T), colour="Global_Self_Worth"))

SPPA_Wave3_totals<-SPPA_Wave3_outdf %>%
  select(tagid,SPPA_scholasticcomp_total,SPPA_socialcomp_total,SPPA_athleticcomp_total,
         SPPA_physicalappear_total,SPPA_behavconduct_total,SPPA_closefriend_total,
         SPPA_globalselfworth_total,SPPA_romanticappeal_total) %>%
  mutate(Scholastic_Competence=SPPA_scholasticcomp_total,
         Social_Competence=SPPA_socialcomp_total,
         Athletic_Competence=SPPA_athleticcomp_total,
         Physical_Appearance=SPPA_physicalappear_total,
         Behavioral_Conduct=SPPA_behavconduct_total,
         Close_Friendship=SPPA_closefriend_total,
         Global_Self_Worth=SPPA_globalselfworth_total,
         Romantic_Appeal=SPPA_romanticappeal_total) %>%
  select(-contains("SPPA")) %>%
  gather('item','value',2:length(.)) 
SPPA_Wave3_totals_graph<-ggplot(SPPA_Wave3_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+ 
  ggtitle(paste0("Wave 3 SPPA total subscores for ",length(unique(SPPA_Wave3_outdf$tagid[!is.na(SPPA_Wave3_outdf$SPPA_scholasticcomp_total)]))," participants")) + geom_vline(aes(xintercept = median(SPPA_Wave3_outdf$SPPA_globalselfworth_total, na.rm=T), colour="Global_Self_Worth"))
#+ geom_vline(aes(xintercept = median(SPPA_Wave3_outdf$SPPA_romanticappeal_total, na.rm=T), colour="Romantic_Appeal"))

SPPA_Wave4_totals<-SPPA_Wave4_outdf %>%
  select(tagid,SPPA_scholasticcomp_total,SPPA_socialcomp_total,SPPA_athleticcomp_total,
         SPPA_physicalappear_total,SPPA_behavconduct_total,SPPA_closefriend_total,
         SPPA_globalselfworth_total,SPPA_romanticappeal_total) %>%
  mutate(Scholastic_Competence=SPPA_scholasticcomp_total,
         Social_Competence=SPPA_socialcomp_total,
         Athletic_Competence=SPPA_athleticcomp_total,
         Physical_Appearance=SPPA_physicalappear_total,
         Behavioral_Conduct=SPPA_behavconduct_total,
         Close_Friendship=SPPA_closefriend_total,
         Global_Self_Worth=SPPA_globalselfworth_total,
         Romantic_Appeal=SPPA_romanticappeal_total) %>%
  select(-contains("SPPA")) %>%
  gather('item','value',2:length(.)) 
SPPA_Wave4_totals_graph<-ggplot(SPPA_Wave4_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+ 
  ggtitle(paste0("Wave 4 SPPA total subscores for ",length(unique(SPPA_Wave4_outdf$tagid[!is.na(SPPA_Wave4_outdf$SPPA_scholasticcomp_total)]))," participants")) + geom_vline(aes(xintercept = median(SPPA_Wave4_outdf$SPPA_globalselfworth_total, na.rm=T), colour="Global_Self_Worth"))
#+ geom_vline(aes(xintercept = median(SPPA_Wave4_outdf$SPPA_romanticappeal_total, na.rm=T), colour="Romantic_Appeal"))

SPPA_Wave5_totals<-SPPA_Wave5_outdf %>%
  select(tagid,SPPA_scholasticcomp_total,SPPA_socialcomp_total,SPPA_athleticcomp_total,
         SPPA_physicalappear_total,SPPA_behavconduct_total,SPPA_closefriend_total,
         SPPA_globalselfworth_total,SPPA_romanticappeal_total) %>%
  mutate(Scholastic_Competence=SPPA_scholasticcomp_total,
         Social_Competence=SPPA_socialcomp_total,
         Athletic_Competence=SPPA_athleticcomp_total,
         Physical_Appearance=SPPA_physicalappear_total,
         Behavioral_Conduct=SPPA_behavconduct_total,
         Close_Friendship=SPPA_closefriend_total,
         Global_Self_Worth=SPPA_globalselfworth_total,
         Romantic_Appeal=SPPA_romanticappeal_total) %>%
  select(-contains("SPPA")) %>%
  gather('item','value',2:length(.)) 
SPPA_Wave5_totals_graph<-ggplot(SPPA_Wave5_totals, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+ 
  ggtitle(paste0("Wave 5 SPPA total subscores for ",length(unique(SPPA_Wave5_outdf$tagid[!is.na(SPPA_Wave5_outdf$SPPA_scholasticcomp_total)]))," participants")) + geom_vline(aes(xintercept = median(SPPA_Wave5_outdf$SPPA_globalselfworth_total, na.rm=T), colour="Global_Self_Worth"))
#+ geom_vline(aes(xintercept = median(SPPA_Wave4_outdf$SPPA_romanticappeal_total, na.rm=T), colour="Romantic_Appeal"))

require(gridExtra)
grid.arrange(SPPA_Wave1_totals_graph, SPPA_Wave2_totals_graph, 
             SPPA_Wave3_totals_graph, SPPA_Wave4_totals_graph, 
             SPPA_Wave5_totals_graph, ncol=2)

library(ggplot2)
tspag <- ggplot(SPPA_Wave1, aes(x=as.Date(survey_date), y=SPPA_globalselfworth_total)) + 
  geom_line() + guides(colour=F) + xlab("Date") + ylab("Global Self Worth")
spag <- tspag + aes(colour = factor(tagid))

sspag <- spag + geom_smooth(se=F, colour='black', size=2)

#write.csv(SPPA_Wave1, file = paste0(workdir,"Demographics/sppa_all_v3.csv"))
sppa_age <- read.csv(file = paste0(workdir,"Demographics/sppa_all.csv"))

tspag_age <- ggplot(sppa_age, aes(x=age, y=SPPA_globalselfworth_total)) + 
  geom_line() + guides(colour=F) + xlab("Age") + ylab("Global Self Worth")
spag_age <- tspag_age + aes(colour = factor(tagid))

sspag_age_gsw <- spag_age + geom_smooth(se=F, colour='black', size=2)

tspag_age <- ggplot(sppa_age, aes(x=age, y=SPPA_physicalappear_total)) + 
  geom_line() + guides(colour=F) + xlab("Age") + ylab("Physical Appearance Self-Concept")
spag_age <- tspag_age + aes(colour = factor(tagid))

sspag_age_phys <- spag_age + geom_smooth(se=F, colour='black', size=2)

tspag_age <- ggplot(sppa_age, aes(x=age, y=SPPA_socialcomp_total)) + 
  geom_line() + guides(colour=F) + xlab("Age") + ylab("Social Competence")
spag_age <- tspag_age + aes(colour = factor(tagid))

sspag_age_soccomp <- spag_age + geom_smooth(se=F, colour='black', size=2)

tspag_age <- ggplot(sppa_age, aes(x=age, y=SPPA_scholasticcomp_total)) + 
  geom_line() + guides(colour=F) + xlab("Age") + ylab("Scholastic Competence")
spag_age <- tspag_age + aes(colour = factor(tagid))

sspag_age_scholcomp <- spag_age + geom_smooth(se=F, colour='black', size=2)

tspag_age <- ggplot(sppa_age, aes(x=age, y=SPPA_romanticappeal_total)) + 
  geom_line() + guides(colour=F) + xlab("Age") + ylab("Romantic Appeal")
spag_age <- tspag_age + aes(colour = factor(tagid))

sspag_age_romapp <- spag_age + geom_smooth(se=F, colour='black', size=2)

tspag_age <- ggplot(sppa_age, aes(x=age, y=SPPA_behavconduct_total)) + 
  geom_line() + guides(colour=F) + xlab("Age") + ylab("Behavioral Conduct")
spag_age <- tspag_age + aes(colour = factor(tagid))

sspag_age_bcon <- spag_age + geom_smooth(se=F, colour='black', size=2)

tspag_age <- ggplot(sppa_age, aes(x=age, y=SPPA_closefriend_total)) + 
  geom_line() + guides(colour=F) + xlab("Age") + ylab("Close Friendship Self-Concept")
spag_age <- tspag_age + aes(colour = factor(tagid))

sspag_age_closefriend <- spag_age + geom_smooth(se=F, colour='black', size=2)

tspag_age <- ggplot(sppa_age, aes(x=age, y=SPPA_athleticcomp_total)) + 
  geom_line() + guides(colour=F) + xlab("Age") + ylab("Athletic Competence")
spag_age <- tspag_age + aes(colour = factor(tagid))

sspag_age_athcomp <- spag_age + geom_smooth(se=F, colour='black', size=2)

grid.arrange(sspag_age_athcomp,sspag_age_bcon,sspag_age_closefriend,sspag_age_phys,
             sspag_age_romapp,sspag_age_scholcomp,sspag_age_soccomp,sspag_age_gsw)

ath1 <- SPPA_Wave1_totals %>% filter(grepl("Athletic_Competence", item))
mean(ath1$value, na.rm=T)
ath2 <- SPPA_Wave2_totals %>% filter(grepl("Athletic_Competence", item))
mean(ath2$value, na.rm=T)
ath3 <- SPPA_Wave3_totals %>% filter(grepl("Athletic_Competence", item))
mean(ath3$value, na.rm=T)
ath4 <- SPPA_Wave4_totals %>% filter(grepl("Athletic_Competence", item))
mean(ath4$value, na.rm=T)

friend1 <- SPPA_Wave1_totals %>% filter(grepl("Close_Friendship", item))
mean(friend1$value, na.rm=T)
friend2 <- SPPA_Wave2_totals %>% filter(grepl("Close_Friendship", item))
mean(friend2$value, na.rm=T)
friend3 <- SPPA_Wave3_totals %>% filter(grepl("Close_Friendship", item))
mean(friend3$value, na.rm=T)
friend4 <- SPPA_Wave4_totals %>% filter(grepl("Close_Friendship", item))
mean(friend4$value, na.rm=T)

glob1 <- SPPA_Wave1_totals %>% filter(grepl("Global_Self_Worth", item))
mean(glob1$value, na.rm=T)
glob2 <- SPPA_Wave2_totals %>% filter(grepl("Global_Self_Worth", item))
mean(glob2$value, na.rm=T)
glob3 <- SPPA_Wave3_totals %>% filter(grepl("Global_Self_Worth", item))
mean(glob3$value, na.rm=T)
glob4 <- SPPA_Wave4_totals %>% filter(grepl("Global_Self_Worth", item))
mean(glob4$value, na.rm=T)
glob5 <- SPPA_Wave5_totals %>% filter(grepl("Global_Self_Worth", item))
mean(glob5$value, na.rm=T)

phys1 <- SPPA_Wave1_totals %>% filter(grepl("Physical_Appearance", item))
mean(phys1$value, na.rm=T)
phys2 <- SPPA_Wave2_totals %>% filter(grepl("Physical_Appearance", item))
mean(phys2$value, na.rm=T)
phys3 <- SPPA_Wave3_totals %>% filter(grepl("Physical_Appearance", item))
mean(phys3$value, na.rm=T)
phys4 <- SPPA_Wave4_totals %>% filter(grepl("Physical_Appearance", item))
mean(phys4$value, na.rm=T)

sch1 <- SPPA_Wave1_totals %>% filter(grepl("Scholastic_Competence", item))
mean(sch1$value, na.rm=T)
sch2 <- SPPA_Wave2_totals %>% filter(grepl("Scholastic_Competence", item))
mean(sch2$value, na.rm=T)
sch3 <- SPPA_Wave3_totals %>% filter(grepl("Scholastic_Competence", item))
mean(sch3$value, na.rm=T)
sch4 <- SPPA_Wave4_totals %>% filter(grepl("Scholastic_Competence", item))
mean(sch4$value, na.rm=T)

soc1 <- SPPA_Wave1_totals %>% filter(grepl("Social_Competence", item))
mean(soc1$value, na.rm=T)
soc2 <- SPPA_Wave2_totals %>% filter(grepl("Social_Competence", item))
mean(soc2$value, na.rm=T)
soc3 <- SPPA_Wave3_totals %>% filter(grepl("Social_Competence", item))
mean(soc3$value, na.rm=T)
soc4 <- SPPA_Wave4_totals %>% filter(grepl("Social_Competence", item))
mean(soc4$value, na.rm=T)

grid.arrange(SPPA_Wave1_totals_graph, SPPA_Wave2_totals_graph, 
             SPPA_Wave3_totals_graph, SPPA_Wave4_totals_graph, 
             SPPA_Wave5_totals_graph, ncol=2)

sppa_pds_wave1 <- merge(SPPA_Wave1_outdf, PDS_Wave1_outdf, by = "tagid", all = T)
sppa_pds_wave1$pdsgroup <- ifelse(sppa_pds_wave1$pdss<=2, 1, ifelse(sppa_pds_wave1$pdss>=4, 3, ifelse(is.na(sppa_pds_wave1$pdss), NA, 2)))
sppa_pds_wave2 <- merge(SPPA_Wave2_outdf, PDS_Wave2_outdf, by = "tagid", all = T)
sppa_pds_wave2$pdsgroup <- ifelse(sppa_pds_wave2$pdss<=2, 1, ifelse(sppa_pds_wave2$pdss>=4, 3, ifelse(is.na(sppa_pds_wave2$pdss), NA, 2)))

sppa_change <- merge(SPPA_Wave1_outdf, SPPA_Wave2_outdf, by = "tagid", all = T)
sppa_change <- sppa_change[c("tagid","SPPA_globalselfworth_mean.x","SPPA_globalselfworth_mean.y")]
cor.test(sppa_change$SPPA_globalselfworth_total.x, sppa_change$SPPA_globalselfworth_total.y, use = "pairwise.complete.obs", method = "pearson")
cor.test(sppa_change$SPPA_globalselfworth_total.x, sppa_change$SPPA_globalselfworth_total.y, use = "pairwise.complete.obs", method = "spearman")

sppa_long <- gather(sppa_change, wave, sppa_global_selfworth_total, SPPA_globalselfworth_total.x:SPPA_globalselfworth_total.y, factor_key=TRUE)
sppa_long$wave <- ifelse(sppa_long$wave=="SPPA_globalselfworth_total.x",1,2)

tspag_sppa = ggplot(sppa_long, aes(x=wave, y=sppa_global_selfworth_total)) + 
  geom_line() + guides(colour=FALSE) + xlab("Wave") +
  ylab("SPPA-Global_Self-Worth-Total Score")
spag_sppa = tspag_sppa + aes(colour = factor(tagid)) + scale_x_continuous(breaks=c(1,2)) + ylim(4,22)
spag2_sppa <- spag_sppa + geom_smooth(se=FALSE, colour="black", size=2)

SPPA_Wave1_means<-SPPA_Wave1 %>%
  select(tagid,SPPA_scholasticcomp_mean,SPPA_socialcomp_mean,SPPA_athleticcomp_mean,
         SPPA_physicalappear_mean,SPPA_behavconduct_mean,SPPA_closefriend_mean,
         SPPA_globalselfworth_mean) %>%
  mutate(Scholastic_Competence=SPPA_scholasticcomp_mean,
         Social_Competence=SPPA_socialcomp_mean,
         Athletic_Competence=SPPA_athleticcomp_mean,
         Physical_Appearance=SPPA_physicalappear_mean,
         Behavioral_Conduct=SPPA_behavconduct_mean,
         Close_Friendship=SPPA_closefriend_mean,
         Global_Self_Worht=SPPA_globalselfworth_mean) %>%
  select(-contains("SPPA")) %>%
  gather('item','value',2:length(.)) 
SPPA_Wave1_means_graph<-ggplot(SPPA_Wave1_means, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Self-Perception Profile for Adolescents mean subscores for ",length(unique(SPPA_Wave1$tagid[!is.na(SPPA_Wave1$SPPA_scholasticcomp_mean)]))," participants"))

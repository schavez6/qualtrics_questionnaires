#note: some of this is dependent on other "self" questionnaires having been processed already... script not ready for primetime
## Graph it
SCC_Wave1_total<-SCC_Wave1_outdf %>%
  select(tagid,SCC_total) %>%
  gather('item','value',2:length(.)) 
SCC_Wave1_total_graph<-ggplot(SCC_Wave1_total, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Self-Concept Clarity total score for ",length(unique(SCC_Wave1_outdf$tagid[!is.na(SCC_Wave1_outdf$SCC_total)]))," participants")) +
  ylim(0,0.06) + xlim(0,60) + geom_vline(aes(xintercept = median(SCC_Wave1_outdf$SCC_total, na.rm=T), 
                                colour="SCC_total"))

SCC_Wave2_total<-SCC_Wave2_outdf %>%
  select(tagid,SCC_total) %>%
  gather('item','value',2:length(.)) 
SCC_Wave2_total_graph<-ggplot(SCC_Wave2_total, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Self-Concept Clarity total score for ",length(unique(SCC_Wave2_outdf$tagid[!is.na(SCC_Wave2_outdf$SCC_total)]))," participants")) +
  ylim(0,0.06) + xlim(0,60) + geom_vline(aes(xintercept = median(SCC_Wave2_outdf$SCC_total, na.rm=T), 
                                colour="SCC_total"))

SCC_Wave3_total<-SCC_Wave3_outdf %>%
  select(tagid,SCC_total) %>%
  gather('item','value',2:length(.)) 
SCC_Wave3_total_graph<-ggplot(SCC_Wave3_total, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Self-Concept Clarity total score for ",length(unique(SCC_Wave3_outdf$tagid[!is.na(SCC_Wave3_outdf$SCC_total)]))," participants")) +
  ylim(0,0.06) + xlim(0,60) + geom_vline(aes(xintercept = median(SCC_Wave3_outdf$SCC_total, na.rm=T), 
                                colour="SCC_total"))

SCC_Wave4_total<-SCC_Wave4_outdf %>%
  select(tagid,SCC_total) %>%
  gather('item','value',2:length(.)) 
SCC_Wave4_total_graph<-ggplot(SCC_Wave4_total, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Self-Concept Clarity total score for ",length(unique(SCC_Wave4_outdf$tagid[!is.na(SCC_Wave4_outdf$SCC_total)]))," participants")) +
  ylim(0,0.06) + xlim(0,60) + geom_vline(aes(xintercept = median(SCC_Wave4_outdf$SCC_total, na.rm=T), 
                                colour="SCC_total"))

SCC_Wave5_total<-SCC_Wave5_outdf %>%
  select(tagid,SCC_total) %>%
  gather('item','value',2:length(.)) 
SCC_Wave5_total_graph<-ggplot(SCC_Wave5_total, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Self-Concept Clarity total score for ",length(unique(SCC_Wave5_outdf$tagid[!is.na(SCC_Wave5_outdf$SCC_total)]))," participants")) +
  ylim(0,0.06) + xlim(0,60) + geom_vline(aes(xintercept = median(SCC_Wave5_outdf$SCC_total, na.rm=T), 
                                colour="SCC_total"))

require(gridExtra)
grid.arrange(SCC_Wave1_total_graph, SCC_Wave2_total_graph, 
             SCC_Wave3_total_graph, SCC_Wave4_total_graph, 
             SCC_Wave5_total_graph, ncol=2)

scc1 <- SCC_Wave1_total %>% filter(grepl("SCC_total", item))
median(scc1$value, na.rm=T)
scc2 <- SCC_Wave2_total %>% filter(grepl("SCC_total", item))
median(scc2$value, na.rm=T)
scc3 <- SCC_Wave3_total %>% filter(grepl("SCC_total", item))
median(scc3$value, na.rm=T)
scc4 <- SCC_Wave4_total %>% filter(grepl("SCC_total", item))
median(scc4$value, na.rm=T)
scc5 <- SCC_Wave5_total %>% filter(grepl("SCC_total", item))
median(scc5$value, na.rm=T)

tspag <- ggplot(SCC_Wave1, aes(x=as.Date(survey_date), y=SCC_total)) + 
  geom_line() + guides(colour=F) + xlab("Date") + ylab("Self-Concept Clarity")
spag <- tspag + aes(colour = factor(tagid))

sspag <- spag + geom_smooth(se=F, colour='black', size=2)

index_sppa_age <- arrange(sppa_age, tagid, mdy(survey_date)) %>% group_by(tagid) %>% mutate(counter=row_number(tagid))
index_scc <- arrange(SCC_Wave1, tagid, ymd(survey_date)) %>% group_by(tagid) %>% mutate(counter=row_number(tagid))

self_age <- full_join(index_sppa_age, index_scc, by=c('tagid', 'counter'))

tspag <- ggplot(self_age, aes(x=age, y=SCC_total)) + 
  geom_line() + guides(colour=F) + xlab("Age") + ylab("Self-Concept Clarity")
spag <- tspag + aes(colour = factor(tagid))

sspag_scc <- spag + geom_smooth(se=F, colour='black', size=2) + xlim(9,21)

tspag2 <- ggplot(sppa_age, aes(x=age, y=SPPA_globalselfworth_total)) + 
  geom_line() + guides(colour=F) + xlab("Age") + ylab("Global Self-Worth")
spag2 <- tspag2 + aes(colour = factor(tagid))

sspag_scc <- spag + geom_smooth(se=F, colour='black', size=2) + xlim(9,21) + ylim(12,60)
sspag_sppa <- spag2 + geom_smooth(se=F, colour='black', size=2) + xlim(9,21) + ylim(5,20)


sspag_gsw <- sspag_age_gsw + xlim(9,21)
grid.arrange(sspag_sppa, sspag_scc, ncol=2)

self_age$agesquare <- self_age$age*self_age$age
self_age$agecube <- self_age$age*self_age$age*self_age$age
self_age$sccsquare <- self_age$SCC_total*self_age$SCC_total
self_age$tagid <- as.factor(self_age$tagid)

df <- self_age %>% as.data.frame() %>% select(tagid,age,agesquare,SPPA_globalselfworth_total,SCC_total)
df <- df %>% group_by(tagid) %>% 
  mutate(sppa_mean2 = mean(SPPA_globalselfworth_total, na.rm=T),
         scc_mean2 = mean(SCC_total, na.rm=T),
         age_mean2 = mean(age, na.rm=T)
  ) %>% ungroup() %>% 
  mutate(sppa_c2 = scale(SPPA_globalselfworth_total, center=T, scale = T),
         scc_c2 = scale(SCC_total, center=T, scale = T),
         age_c2 = scale(age, center=T, scale = F),
         sppa_cw2 = SPPA_globalselfworth_total-sppa_mean2,
         scc_cw2 = SCC_total-scc_mean2,
         age_cw2 = age-age_mean2,
         sppa_cb2 = sppa_c2-sppa_cw2,
         scc_cb2 = scc_c2-scc_cw2,
         age_cb2 = age_c2-age_cw2
         )
df$ageint10yrs <- df$age-10
df$agesqr10yrs <- df$ageint10yrs*df$ageint10yrs
df$agecube10yrs <- df$ageint10yrs*df$ageint10yrs*df$ageint10yrs
df$invage10yrs <- 1/df$ageint10yrs
df$sqinvage10yrs <- 1/(df$ageint10yrs*df$ageint10yrs)
df$cubeinvage10yrs <- 1/(df$ageint10yrs*df$ageint10yrs*df$ageint10yrs)
df$sqrtage10yrs <- sqrt(df$ageint10yrs)
df$logage10yrs <- log(df$ageint10yrs)
df$invsqrt10yrs <- 1/(sqrt(df$ageint10yrs))
cuberoot = function(x){

    if(x < 0)

    { - (-x)^(1/3)}

    else

    {x^(1/3)}

    }
df$invcubert10yrs <- 1/cuberoot(df$ageint10yrs)

test <- lmerTest::lmer(SPPA_globalselfworth_total ~ agesquare + age + SCC_total + (1 |tagid), data = df)
lmerTest::ranova(test)
summary(test, digits=4)
ranef(test)
sjPlot::tab_model(test,p.val = "kr", show.df=T)

testca <- lmerTest::lmer(sppa_c2 ~ sqinvage10yrs + scc_c2 + (1 |tagid), data = df)
testcb <- lmerTest::lmer(sppa_c2 ~ invage10yrs + scc_c2 + (1 |tagid), data = df)
testcc <- lmerTest::lmer(sppa_c2 ~ invsqrt10yrs + invage10yrs + scc_c2 + (1 |tagid), data = df)
testcd <- lmerTest::lmer(sppa_c2 ~ ageint10yrs + scc_c2 + (1 |tagid), data = df)
testce <- lmerTest::lmer(sppa_c2 ~ sqrtage10yrs + scc_c2 + (1 |tagid), data = df)
testcf <- lmerTest::lmer(sppa_c2 ~ agesqr10yrs + ageint10yrs + scc_c2 + (1 |tagid), data = df)
testcg <- lmerTest::lmer(sppa_c2 ~ agecube10yrs + scc_c2 + (1 |tagid), data = df)
testch <- lmerTest::lmer(sppa_c2 ~ agesqr10yrs + ageint10yrs + scc_c2 + (1 |tagid), data = df)
lmerTest::ranova(testch)
summary(testch, digits=4)
ranef(testch)
sjPlot::tab_model(testch,p.val = "kr", show.df=T)

agemod <- lmerTest::lmer(scc_c2 ~ ageint10yrs + sppa_c2 + (1 |tagid), data = df)
lmerTest::ranova(agemod)
summary(agemod, digits=4)
ranef(agemod)
sjPlot::tab_model(agemod,p.val = "kr", show.df=T)

test1 <- lmerTest::lmer(SPPA_globalselfworth_total ~ age + SCC_total + (1 |tagid), data = df)
lmerTest::ranova(test1)
summary(test1, digits=4)
ranef(test1)
sjPlot::tab_model(test1,p.val = "kr", show.df=T)

test2 <- lmerTest::lmer(SCC_total ~ agesquare + age + SPPA_globalselfworth_total + (1 |tagid), data = df)
lmerTest::ranova(test2)
summary(test2, digits=4)
ranef(test2)
sjPlot::tab_model(test2,p.val = "kr", show.df=T)

test3 <- lmerTest::lmer(SCC_total ~ age + SPPA_globalselfworth_total + (1 |tagid), data = df)
lmerTest::ranova(test3)
summary(test3, digits=4)
ranef(test3)
sjPlot::tab_model(test3,p.val = "kr", show.df=T)

library(nlme)

model1 <- lmer(SPPA_globalselfworth_total ~ age + SCC_total + (1+ age | tagid), data=self_age)
summary(model1)
sjPlot:: tab_model(model1)

model2 <- lmer(SCC_total ~ age + SPPA_globalselfworth_total + (1+ age | tagid), data=self_age)
summary(model2)
sjPlot:: tab_model(model2)

#index_sppa_age <- arrange(sppa_age, tagid, mdy(survey_date)) %>% group_by(tagid) %>% mutate(counter=row_number(tagid))
test <- CESDC_Wave1 %>% subset(!survey_type=="ACE")
test2 <- sppa_age %>% subset(!survey_type=="SOS") 
test2 <- arrange(test2, tagid, mdy(survey_date)) %>% group_by(tagid) %>% mutate(counter=row_number(tagid))
test3 <- SCC_Wave1 %>% subset(!survey_type=="SOS")
test3 <- arrange(test3, tagid, ymd(survey_date)) %>% group_by(tagid) %>% mutate(counter=row_number(tagid))
index_cesdc <- arrange(test, tagid, ymd(survey_date)) %>% group_by(tagid) %>% mutate(counter=row_number(tagid))

self_dep_age <- full_join(test2, index_cesdc, by=c('tagid', 'counter')) %>% full_join(test3, by=c('tagid','counter'))

tspag2 <- ggplot(CESDC_Wave1, aes(x=as.Date(survey_date), y=CES_DC_total)) + 
  geom_line() + guides(colour=F) + xlab("Date") + ylab("Depressive Symptoms")
spag2 <- tspag2 + aes(colour = factor(tagid))

sspag_cesdc <- spag2 + geom_smooth(se=F, colour='black', size=2)

tspag3 <- ggplot(self_dep_age, aes(x=age, y=CES_DC_total)) + 
  geom_line() + guides(colour=F) + xlab("Age") + ylab("Depressive Symptoms")
spag3 <- tspag3 + aes(colour = factor(tagid))

sspag_cesdc <- spag3 + geom_smooth(se=F, colour='black', size=2) + xlim(9,21) + ylim(0,60)

dfdep <- self_dep_age %>% as.data.frame() %>% select(tagid,age,SPPA_globalselfworth_total,SCC_total,CES_DC_total)
dfdep <- dfdep %>% group_by(tagid) %>% 
  mutate(sppa_mean2 = mean(SPPA_globalselfworth_total, na.rm=T),
         scc_mean2 = mean(SCC_total, na.rm=T),
         cesdc_mean2 = mean(CES_DC_total, na.rm=T),
         age_mean2 = mean(age, na.rm=T)
  ) %>% ungroup() %>% 
  mutate(sppa_c2 = scale(SPPA_globalselfworth_total, center=T, scale = T),
         scc_c2 = scale(SCC_total, center=T, scale = T),
         cesdc_c2 = scale(CES_DC_total, center=T, scale=T),
         age_c2 = scale(age, center=T, scale = F),
         sppa_cw2 = SPPA_globalselfworth_total-sppa_mean2,
         scc_cw2 = SCC_total-scc_mean2,
         cesdc_cw2 = CES_DC_total-cesdc_mean2,
         age_cw2 = age-age_mean2,
         sppa_cb2 = sppa_c2-sppa_cw2,
         scc_cb2 = scc_c2-scc_cw2,
         cesdc_cb2 = cesdc_c2-cesdc_cw2,
         age_cb2 = age_c2-age_cw2
         )

dfdep$ageint10yrs <- dfdep$age-10
dfdep$agesqr10yrs <- dfdep$ageint10yrs*dfdep$ageint10yrs
dfdep$agecube10yrs <- dfdep$ageint10yrs*dfdep$ageint10yrs*dfdep$ageint10yrs
dfdep$invage10yrs <- 1/dfdep$ageint10yrs
dfdep$sqinvage10yrs <- 1/(dfdep$ageint10yrs*dfdep$ageint10yrs)
dfdep$cubeinvage10yrs <- 1/(dfdep$ageint10yrs*dfdep$ageint10yrs*dfdep$ageint10yrs)
dfdep$sqrtage10yrs <- sqrt(dfdep$ageint10yrs)
dfdep$cubertage10yrs <- cuberoot(dfdep$ageint10yrs)
dfdep$logage10yrs <- log(dfdep$ageint10yrs)
dfdep$invsqrt10yrs <- 1/(sqrt(dfdep$ageint10yrs))
dfdep$invcubert10yrs <- 1/cuberoot(dfdep$ageint10yrs)

modeldepnull <- lmer(cesdc_c2 ~ invcubert10yrs + invsqrt10yrs + invage10yrs + (1 | tagid), data=dfdep)
modeldep <- lmer(cesdc_c2 ~ invcubert10yrs + invsqrt10yrs + invage10yrs + sppa_c2 + scc_c2 + (1|tagid), data=dfdep, REML = T)

modeldepint <- lmer(cesdc_c2 ~ invcubert10yrs + invsqrt10yrs + invage10yrs + sppa_c2 + scc_c2 + (1|tagid), data=dfdep)
lmerTest::ranova(modeldep)
summary(modeldep, digits=4)
ranef(modeldep)
sjPlot::tab_model(modeldepint, p.val = "kr", show.df=T)

#m15nlme <- lme(cesdc_c2 ~ 1 + sppa_c2 + scc_c2 + invcubert10yrs + invsqrt10yrs + invage10yrs, random = ~1 |tagid, data=dfdep, method="ML",na.action = na.omit) 
#sjPlot::tab_model(m15nlme)
self_dep_age$agecat <- ifelse(self_dep_age$age<13,1,
                       ifelse(13<self_dep_age$age & self_dep_age$age<16,2,3))
tspag4 <- ggplot(self_dep_age, aes(x=SCC_total, y=SPPA_globalselfworth_total, group_by('agecat'))) + 
  geom_line() + guides(colour=F) + xlab("Self-Concept Clarity") + ylab("Global Self-Worth")
spag4 <- tspag4 + aes(colour = factor(tagid))


sspag4 <- spag4 + geom_smooth(se=F, colour='black', size=2)

library(rgl)
#col=palette.colors(palette = "Set 2")
interleave <- function(v1, v2)  as.vector(rbind(v1,v2))

open3d()
rgl::plot3d(self_dep_age$SCC_total, self_dep_age$SPPA_globalselfworth_total, self_dep_age$age, type = "s", size = 0.5, lit = F, col=as.numeric(factor(self_dep_age$tagid)), axes=F, xlab = "", ylab = "", zlab = "")
segments3d(interleave(self_dep_age$SCC_total, self_dep_age$SCC_total),
           interleave(self_dep_age$SPPA_globalselfworth_total,  self_dep_age$SPPA_globalselfworth_total),
           interleave(self_dep_age$age, min(self_dep_age$age)),
           alpha = 0.4, col = as.numeric(factor(self_dep_age$tagid)))

# Draw the box.
rgl.bbox(color = "grey50",          # grey60 surface and black text
         emission = "grey50",       # emission color is grey50
         xlen = 0, ylen = 0, zlen = 0)  # Don't add tick marks

# Set default color of future objects to black
rgl.material(color = "black")

# Add axes to specific sides. Possible values are "x--", "x-+", "x+-", and "x++".
axes3d(edges = c("x--", "y+-", "z--"),
       ntick = 6,                       # Attempt 6 tick marks on each side
       cex = .75)                       # Smaller font

# Add axis labels. 'line' specifies how far to set the label from the axis.
mtext3d("Self-Clarity",       edge = "x--", line = 2)
mtext3d("Self-Worth", edge = "y+-", line = 3)
mtext3d("Age",          edge = "z--", line = 3)

# Show regression plane with z as dependent variable

fit <- lmer(age ~ SCC_total + SPPA_globalselfworth_total + (1|tagid), data=self_dep_age)

#coefs <- coef(fit)
coefs <- fixed.effects(fit)
a <- coefs["x"]
b <- coefs["y"]
c <- -1
d <- coefs["(Intercept)"] #model intercept fixed
abclines3d(a, b, c, d, alpha = 0.5)


#summary(sem('cesdc_c2 ~ sppa_c2 + scc_c2 + ageint10yrs', data = dfdep, cluster = 'tagid', 
#  missing = 'fiml', fixed.x = F))

scc_change <- merge(SCC_Wave1_outdf, SCC_Wave2_outdf, by = "tagid", all = T)
scc_change <- scc_change[c("tagid","SCC_total.x","SCC_total.y")]
cor.test(scc_change$SCC_total.x, scc_change$SCC_total.y, use = "pairwise.complete.obs", method = "pearson")
cor.test(scc_change$SCC_total.x, scc_change$SCC_total.y, use = "pairwise.complete.obs", method = "spearman")

scc_long <- gather(scc_change, wave, scc_total, SCC_total.x:SCC_total.y, factor_key=TRUE)
scc_long$wave <- ifelse(scc_long$wave=="SCC_total.x",1,2)

tspag_scc = ggplot(SCC_Wave1, aes(x=wave, y=scc_total)) + 
  geom_line() + guides(colour=FALSE) + xlab("Wave") +
  ylab("SCC-Total Score")
spag_scc = tspag_scc + aes(colour = factor(tagid)) + scale_x_continuous(breaks=c(1,2))
spag2_scc <- spag_scc + geom_smooth(se=FALSE, colour="black", size=2)

SCC_Wave1_mean<-SCC_Wave1 %>%
  select(tagid,SCC_mean)%>%
  gather('item','value',2:length(.)) 
SCC_Wave1_mean_graph<-ggplot(SCC_Wave1_mean, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("Self-Concept Clarity mean score for ",length(unique(SCC_Wave1$tagid[!is.na(SCC_Wave1$SCC_mean)]))," participants"))

## Graph it
CARE_R_SOC_Wave1_DO<-CARE_R_SOC_Wave1 %>%
  select(tagid,carersoc_gen,carersoc_dograffiti,carersoc_dosubstance,carersoc_doviolence,carersoc_dorecklessdriving) %>%
  mutate(General_Social_Care=carersoc_gen,
         Graffiti=carersoc_dograffiti,
         Substances=carersoc_dosubstance,
         Violence=carersoc_doviolence,
         Reckless_Driving=carersoc_dorecklessdriving) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("carersoc_",item))
CARE_R_SOC_Wave1_DO_graph<-ggplot(CARE_R_SOC_Wave1_DO, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("CARE-R Social Wave1: Doing these things would make people 1: like me a lot less to 5: like me a lot more. N= ",length(CARE_R_SOC_Wave1_DO$tagid[!is.na(CARE_R_SOC_Wave1_DO$tagid)])," participants"))

CARE_R_SOC_Wave1_DONT<-CARE_R_SOC_Wave1 %>%
  select(tagid,carersoc_gen,carersoc_avoidgraffiti,carersoc_avoidsubstance,carersoc_avoidviolence,carersoc_avoidrecklessdriving) %>%
  mutate(General_Social_Care=carersoc_gen,
         Graffiti=carersoc_avoidgraffiti,
         Substances=carersoc_avoidsubstance,
         Violence=carersoc_avoidviolence,
         Reckless_Driving=carersoc_avoidrecklessdriving) %>%
  gather('item','value',2:length(.)) %>%
  filter(!grepl("carersoc_",item))
CARE_R_SOC_Wave1_DONT_graph<-ggplot(CARE_R_SOC_Wave1_DONT, aes(x=value, colour=item)) +
  geom_density(alpha=.3)+
  ggtitle(paste0("CARE-R Social Wave1: NOT doing these things would make people 1: like me a lot less to 5: like me a lot more. N= ",length(CARE_R_SOC_Wave1_DONT$tagid[!is.na(CARE_R_SOC_Wave1_DONT$tagid)])," participants"))

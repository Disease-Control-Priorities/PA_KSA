rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(purrr)

RRs<-read_xlsx("data/prop_active.xlsx", sheet="RR")%>%
  bind_rows(., merge(read_xlsx("data/prop_active.xlsx", sheet="RR_all")%>%select(-age), 
                     data.frame(age=c(15,20,25,30,35,40,45,50,55,60,65,70,75,80,85)))
            )

prop<-read_xlsx("data/prop_active.xlsx", sheet="PA")%>%
  select(age, sex, Low, Moderate, High)%>%
  gather(category, prop, -age, -sex)

targets<-data.frame(scenario = c("Intervention", "Intervention", "Intervention", 
                                 "Ideal", "Ideal", "Ideal"),
                    category = c("Low", "Moderate", "High",
                                 "Low", "Moderate", "High"),
                    prop.target = c(19, 74, 7,
                             5, 65, 30))
#PIFs
df<-left_join(prop, targets)%>%
  mutate(prop.target = ifelse(category=="Low" & prop<prop.target, prop, prop.target),
         prop.target = ifelse(category=="High" & prop>prop.target, prop, prop.target))%>%
  select(-prop)%>%
  spread(category, prop.target)%>%
  mutate(Moderate = 100 - Low - High)%>%
  gather(category, target.prop, -age, -sex, -scenario)%>%
  left_join(., prop%>%rename(base.prop = prop))%>%
  left_join(., RRs)%>%
  mutate(Pi = base.prop*RR/100,
         Pihat = target.prop*RR/100)%>%
  group_by(age, sex, cause, scenario)%>%
  summarise(Pi = sum(Pi),
            Pihat = sum(Pihat))%>%
  mutate(PIF = (Pi - Pihat)/Pi)%>%
  select(age, sex, cause, scenario, PIF)

#add average

write.csv(df, "PIFs.csv", row.names = F)  

#plot PIFS (instead of PAFs?)
ggplot(df%>%filter(scenario!="Baseline")%>%
         filter(!(sex=="Male" & cause == "Breast cancer")), 
       aes(cause, PIF, fill=scenario))+ 
  geom_boxplot()+
  theme_bw()+
  labs(fill="Scenario")+
  xlab("")+
  ylab("Potential impact fraction (PIF)")+
  facet_wrap(~sex)+ 
  theme(axis.text.x = element_text(angle = 45, hjust=0.95, size=11))+
  scale_y_continuous(limits = c(0, 0.16))+
  scale_fill_manual(values=c("#E69F00", "#56B4E9"))

#scale (0 to 0.16 by 0.2)

ggsave("PIF_figure.tiff", height=6, width=11, dpi=300)
ggsave("PIF_figure.eps", height=6, width=11, dpi=300)
ggsave("PIF_figure.pdf", height=6, width=11, dpi=300)
ggsave("PIF_figure.jpg", height=6, width=11, dpi=300)


#average PIF by scenario
df%>%group_by(scenario)%>%
  summarise(PIF = mean(PIF))

0.0645/0.0493

(0.0645-0.0493)/0.0493

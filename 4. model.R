rm(list=ls()) 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
pacman::p_load(data.table, dplyr, tidyr, stringr, ggplot2, tidyverse, broom, readxl)

##########################################################################
#########################################################################
loc<-"Saudi Arabia"

pop0<-read.csv("data/pop0.csv", stringsAsFactors = F) 
pop0<-data.table(pop0)
b_rates<-read.csv("data/tps_adjusted.csv", stringsAsFactors = F)

cse<-unique(b_rates$cause)

#set bg.mx.all
b_rates<-b_rates%>%group_by(age, year, location, sex)%>%
  mutate(ncd.mx = sum(dis.mx),
         bg.mx.all = all.mx - ncd.mx)

#repeat b_rates for 2019-2040
add<-b_rates%>%filter(year==2019)

for (i in 2020:2040){
      b_rates<-bind_rows(b_rates, add%>%mutate(year=i))
}

b_rates<-data.table(b_rates)


## Pull in intervention effects for ages 25-79 ##
int.df<-read.csv("PIFs.csv")%>%filter(age>=25, age<80)
#Add baseline
int.df<-bind_rows(int.df,
                  int.df%>%filter(scenario=="Ideal")%>%
                    mutate(scenario = "Baseline",
                           PIF = 0))

#repeat for single year ages
int.df<-bind_rows(int.df,
                  int.df%>%mutate(age=age+1),
                  int.df%>%mutate(age=age+2),
                  int.df%>%mutate(age=age+3),
                  int.df%>%mutate(age=age+4)
                  )

#Add intervention effects to TPs
int_rates<-bind_rows(b_rates%>%mutate(scenario="Baseline"),
                     b_rates%>%mutate(scenario="Ideal"),
                     b_rates%>%mutate(scenario="Intervention"))%>%
  left_join(., int.df)%>%
  mutate(PIF = ifelse(is.na(PIF),0,PIF),
         IR = ifelse(year>=2023, IR*(1-PIF), IR))

any(is.na(int_rates))

#Start in 2015 for burn in period?

state.transition<-function(b_rates, pop0){ 
      
      base_rates<-merge(b_rates, pop0[year<=2040], by=c("year", "location", "sex", "age"), all=TRUE)%>%
            arrange(year, age)%>%
            filter(year>=2015)

      base_rates[age==0 & year>2015, Nx:=Nx0]
      base_rates[, Nx0:=NULL]
      
      # calculate initial states for the incoming year 2015 and all years for age 0 population
      
      suppressWarnings(base_rates[year==2015 | age==0, sick:=Nx*prev.rate])
      base_rates[year==2015 | age==0, dead:=Nx*dis.mx]
      base_rates[year==2015 | age==0, well:=Nx*(1-(prev.rate+all.mx))]
      base_rates[year==2015 | age==0, newcases:=well*IR]
      
      base_rates[age==0 | year==2015, pop:=Nx]
      base_rates[age==0 | year==2015, all.dead:=Nx*all.mx]
      
      base_rates[CF>0.9, CF:=0.9]
      base_rates[IR>0.9, IR:=0.9]
      
      base_rates<-base_rates%>%arrange(scenario, sex, location, cause, age, year)
      #i<-1
      #STATE TRANSITIONS#
      for(i in 1:26){
            
            b2<-base_rates[year<=2015+i & year>=2015+i-1]
            b2[,age2:=age+1]
            
            #new cases
            b2[, newcases2:=shift(well)*IR, by=.(sex, location, cause, age)]
            #b2[age2>=95, sick2:=sick2+shift(sick2, type="lead"), by=.(sex, location, cause, year)]
            b2[newcases2<0, newcases2:=0] #prevent possible negatives
            
            #sick
            b2[, sick2:=shift(sick)*(1-(CF+bg.mx)) + shift(well)*IR, by=.(sex, location, cause, age)]
            #b2[age2>=95, sick2:=sick2+shift(sick2, type="lead"), by=.(sex, location, cause, year)]
            b2[sick2<0, sick2:=0] #prevent possible negatives
            
            #dead
            b2[, dead2:=shift(sick)*CF, by=.(sex, location, cause, age)]
            #b2[age2>=95, dead2:=dead2+shift(dead2, type="lead"), by=.(sex, location, cause, year)]
            b2[dead2<0, sick2:=0] #prevent possible negatives
            
            #pop
            b2[,pop2:=shift(pop)-shift(all.dead), by=.(sex, location, cause, age)]
            #b2[age2>=95, pop2:=pop2+shift(pop2, type="lead"), by=.(sex, location, cause, year)]
            b2[pop2<0, pop2:=0] #prevent possible negatives
            
            #all dead
            b2[, all.dead2:=sum(dead2), by=.(sex, location, year, age)]
            b2[, all.dead2:=all.dead2+(pop2*bg.mx.all)]
            b2[all.dead2<0, all.dead2:=0]
            
            #well
            b2[, well2:=pop2-all.dead2-sick2]
            b2[well2<0, well2:=0] #prevent possible negatives
            
            #re-combined into original data.table
            b2<-b2[year==2015+i & age2<96, c("age2", "newcases2", "sick2", "dead2", "well2", "pop2", "all.dead2", "scenario", "sex", "location", "cause")]
            setnames(b2, "age2", "age")
            base_rates[year==2015+i & age>0, sick:=b2[,sick2]]
            base_rates[year==2015+i & age>0, newcases:=b2[,newcases2]]
            base_rates[year==2015+i & age>0, dead:=b2[,dead2]]
            base_rates[year==2015+i & age>0, well:=b2[,well2]]
            base_rates[year==2015+i & age>0, pop:=b2[,pop2]]
            base_rates[year==2015+i & age>0, all.dead:=b2[,all.dead2]]
            
      }
      
      base_rates%>%select(year, scenario, location, sex, age, cause, IR, CF, well, newcases, sick, dead, pop, all.dead)
}

#Run it
test<-state.transition(int_rates, pop0)
any(is.na(test))

#Inspect
plot<-test%>%group_by(year, scenario, cause)%>%
  summarise(deaths = sum(dead))

ggplot(plot, aes(x=year, y=deaths, color=scenario))+
  geom_point()+
  facet_wrap(~cause)

DA<-test%>%group_by(scenario)%>%
  summarise(deaths = sum(dead))%>%
  spread(scenario, deaths)%>%
  mutate(averted1 = Baseline - Ideal,
         averted2 = Baseline - Intervention)

write.csv(test, "all_results.csv", row.names = F)


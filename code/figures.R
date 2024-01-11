## figures for paper ###
library(here)
library(modelsummary)
library(tidyverse)
library(multimediate)
library(scico)

load(here("data","trust1020_simulation_results.RData"))

thm1<-scale_fill_scico_d(palette="tokyo",begin=0.3, 
                         end=0.8, direction = -1,
                         aesthetics = c("colour","fill"))
## plot for global results

## dataset-wide results are stored in mult.ma


#modelplot(mult.ma$model.y)
summary(mult.ma) #has the results of the simulation

plot(mult.ma,logit="OR") #why does this plot different the ORs and bring everything 
# close to one even when the estimates have not that value? there are ORs here that are close
# to zero but the plot brings them close to one?

#convert to data frame
t<-data.frame(summary(mult.ma,logit="OR"))

#rename first column
names(t)[1] <- "Estimates"

#remove "OR"

t$Estimates<-sub("OR","",t$Estimates)

# Remove the PM estimates

t<-t%>%
  filter(!grepl('PM',Estimates))

# create new column with type of estimate

t<-t %>%
  mutate(Type=case_when(
    grepl("treat",Estimates) ~ "Treatment",
    grepl("control",Estimates) ~ "Control",
    .default=as.character(Estimates)
    
  ))
  
# remove the type of estimate from the estimate cell

t$Estimates <-gsub(".control.|.control","",t$Estimates)
t$Estimates <-gsub(".treat.|.treat","",t$Estimates)
  
# calculate average values

average<-t%>%
  group_by(Estimates)%>%
  summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE)))

average$Type<-rep("Average",nrow(average))

t<-rbind(t,average)

#remove duplicated entry for total effect

t<- t[-nrow(t),]


a<-set.seed(123)

t %>% 
  group_by(Estimates)%>%
  ggplot(aes(y=Estimates,x=Estimation))+
  geom_vline(xintercept = 1, linetype="dashed",color="red") +
  geom_point(aes(color=Type),size=2,position = position_jitter(seed=123,width=0,height =0.5))+
  geom_linerange(aes(xmin=IC.inf,xmax=IC.sup,color=Type),position=position_jitter(seed=123,width=0,height =0.5))+
  scale_y_discrete(limits=rev)+
  theme_classic()+
  theme(legend.position = "bottom")+
  ggtitle("Estimates and Confidence Intervals in OR scale")+
  #xlim()+
  xlab("")+
  ylab("")+
  thm1

#this plots differently, why?




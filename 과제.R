proact=read.delim("proact_sample.txt", sep="|") # column�� ???????ִ? |
head(proact)
dim(proact)
str(proact)

library(dplyr)

prac=filter(proact, form_name=="ALSFRS")

proact %>%
  filter(form_name == "ALSFRS") -> alsfrs

head(prac)
data=filter(prac, 
            feature_name == "mouth"|
              feature_name == "hands"|
              feature_name == "trunk"|
              feature_name == "leg"|
              feature_name == "respiratory_R"|
              feature_name == "ALSFRS_R_Total")

prac %>%
  filter(feature_name %in% c("mouth", "hands", 
                             "trunk", "leg", 
                             "respiratory_R", 
                             "ALSFRS_R_Total")) -> data

data = droplevels(data)

colnames(data)=c("SubjectID","form_name","feature","value","unit","delta")

temp = duplicated(data)

head(data[temp,])

data = distinct(data)

data %>%
  group_by(SubjectID, feature, delta) %>%
  filter(n() == 1) -> temp

temp = select(temp, -c("form_name", "unit"))
temp = distinct(temp)

library(tidyr)
alsfrs_wide = spread(data = temp, key = feature, value = value)



head(alsfrs_wide)

alsfrs_wide[complete.cases(alsfrs_wide),] -> temp

alsfrs_long=gather(temp, 
                   key="feature", 
                   value="value", 
                   ALSFRS_R_Total:trunk)
head(alsfrs_long)

alsfrs_wide = temp

library(ggplot2)
# sample=dataset%>%
#  filter(feature=="ALSFRS_R_Total")

sample_subj = sample(unique(alsfrs_wide$SubjectID), 
                     10)

temp = alsfrs_wide[alsfrs_wide$SubjectID %in% sample_subj,]

temp$ALSFRS_R_Total = as.numeric(as.character(temp$ALSFRS_R_Total))

alsfrs_wide$ALSFRS_R_Total = as.numeric(as.character(alsfrs_wide$ALSFRS_R_Total))
ggplot(alsfrs_wide, 
       aes(delta, ALSFRS_R_Total, 
                 col=factor(SubjectID))) + 
  geom_point() + geom_line() + 
  theme(legend.position = "none")


i=1:10
for(i in 1:10)
  sample_new=rbind(sample_new,sample %>%
  filter(SubjectID == random[i]))

sample_long=spread(sample_new, delta, value)
delta_subject=sample_new %>%
  group_by(SubjectID, delta) %>%
  summarize(value)
sample_new

p=ggplot(sample_new, aes(x=delta, y=value, group=SubjectID, color=SubjectID))+geom_line()
q=ggplot(dataset, aes(x=delta, y=value, group=SubjectID, color=SubjectID))+geom_line()
max=dataset %>%
  group_by(SubjectID, feature) %>%
  filter(delta==max(delta))
nrow(dataset)
min=dataset %>%
  group_by(SubjectID, feature) %>%
  filter(delta==min(delta))
time=max[,6]-min[,6]
value_min=as.numeric(as.character(min$value))
value_max=as.numeric(as.character(max$value))
interval=value_max-value_min
r=time/interval
s=ggplot(r)
class(max[,6])
dataset %>%
  group_by(value)
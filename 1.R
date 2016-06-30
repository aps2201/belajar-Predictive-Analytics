#https://www.youtube.com/watch?v=32o0DnuRjfg
library(dplyr)
library(ggplot2)
train = read.csv("train.csv")
test = read.csv("test.csv")

test = mutate(test, Survived = NA)

combine = rbind(test,train)

combine$Pclass=as.factor(combine$Pclass)
combine$Survived=as.factor(combine$Survived)
library(plotly)

k=ggplot(train,aes(x=Sex, fill=as.factor(Survived)))+
  facet_grid(.~Pclass)+
  geom_bar(position = "dodge")
ggplotly(k)

combine$Name=as.character(combine$Name)

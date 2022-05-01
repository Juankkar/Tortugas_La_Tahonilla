library(tidyverse)
library(dslabs)
library(states)
library(matrixStats)
library(nortest)
library(ggthemes)



asesinatos <- murders
class(asesinatos)
attach(asesinatos)
head(asesinatos)
state

mean(population, trim = .1)
median(population)


weighted.mean(population)
weightedMedian(population)

quantile(asesinatos[['population']], p=c(.25,.5,.75))

hist(total,freq = F)
lines(density(total), lwd=2,col='red')

shapiro.test(total)


iris %>% 
  ggplot(aes(x=Petal.Width, fill=Species)) +
  geom_histogram(col='black',
               show.legend = F) +
  scale_fill_manual(values = c(
    "yellow","skyblue","yellowgreen"
  )) +
  scale_y_continuous(expand = expansion(0),
                     limits = c(0,40)) +
  #facet_wrap(~Species, ncol = 2) +
  theme_pander() +
  theme(axis.line = element_line())

tapply(iris$Petal.Width,iris$Species,shapiro.test)

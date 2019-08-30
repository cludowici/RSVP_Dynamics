#######################################
####Confidence Mixed Model Analysis####
#######################################
library(lme4)
library(magrittr)
library(dplyr)

rm(list=ls())

timeStamp <- Sys.time() %>% strftime(format = '%d-%m-%Y_%H-%M')

setwd('~/gitCode/nStream/')

allData <- read.table('Analysis/allErrors.txt', sep = '\t', stringsAsFactors = F, header = T)

allData %<>% rename(SPE = error, targetSP=cuePos0)

allData %<>% filter(button %in% c(0,2))

allData %<>% 
  mutate(conf = ifelse(button == 0, 'unsure', 'sure')) %>% 
  mutate(conf = factor(conf, levels = c('sure', 'unsure'))) %>%
  mutate(condition = factor(condition, levels = c('twoStreams', 'eightStreams')))


interceptModel <- glmer(conf~1+(1|ID), data = allData, family = binomial(link = 'logit'))
SPEModel <- glmer(conf~abs(SPE)+(1|ID), data = allData, family = binomial(link='logit'))
conditionModel <- glmer(conf~condition+(1|ID), data = allData, family = binomial(link='logit'))
mainEffectsModel <- glmer(conf~condition+abs(SPE)+(1|ID), data = allData, family = binomial(link='logit'))
interactionModel <- glmer(conf~abs(SPE)*condition+(1|ID), data = allData, family = binomial(link='logit'))


x <- anova(interceptModel, SPEModel,conditionModel, mainEffectsModel, interactionModel)


summary(mainEffectsModel)

countPlot <-  allData %>% 
  mutate(condition = ifelse(condition == 'twoStreams', 'Two Streams', 'Eight Streams')) %>%
  mutate(condition = ordered(condition, levels = c('Two Streams', 'Eight Streams'))) %>%
  group_by(ID, condition) %>% 
  summarise(Sure = sum(conf == 'sure'), 
            Unsure = sum(conf == 'unsure')) %>% 
  melt() %>%
  ggplot(aes(x = condition, y = value))+
  geom_point(aes(colour = variable))+
  stat_summary(fun.y = mean, geom='point', aes(colour = variable), size = 4)+
  stat_summary(fun.data = mean_se, geom = 'errorbar', aes(group = variable), width = .2)+
  theme_apa(base_size = 25)+
  labs(x = 'Condition', y = 'Count', colour = 'Confidence')

countPlot

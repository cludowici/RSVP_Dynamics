###############################
###Date: 2019-04-25 14:18:37###
###Author: Charlie Ludowici####
###############################
#Eighteen Streams Gamma fitting
setwd('~/gitCode/RSVPDynamics/')

library(ggplot2)
library(dplyr)
library(magrittr)
library(reshape2)
library(purrr)
devtools::load_all('~/gitCode/mixRSVP/')

rm(list=ls())

analyses <- function(params, modelKind = NULL, bestFitting = FALSE, nIterations = 10000){
  if(bestFitting){
    params %<>% filter(model == favouredModel)
    modelKind = 'Best Fitting'
  } else {
    params %<>% filter(model == modelKind)
    x = params %>% filter(condition == 'eightStreams') #For t-tests. If bestFitting is true then we get different Ns, so must use a linear model instead
    y = params %>% filter(condition == 'twoStreams')
  }
  
  results <- list()
  
  #######################
  ###Efficacy Analyses###
  #######################
  if(bestFitting){
    efficacyBFFullVSNull <- anovaBF(efficacy ~ condition + participant,
                                    data=params,
                                    whichRandom = 'participant', 
                                    progress = FALSE
    )
  } else {
    xEfficacy <- x %>% pull(efficacy)
    yEfficacy <- y %>% pull(efficacy)
    efficacyBFFullVSNull <- ttestBF(x = xEfficacy, y = yEfficacy, paired = T)
  }
  
  
  BayesFactorLabel <- efficacyBFFullVSNull %>% as.vector %>% round(2) %>% paste0('BF[10]==', .)
  
  #Only evparticipantence for an effect of ring
  
  efficacyPlot = ggplot(params, aes(x=condition, y = efficacy))+
    #geom_violin(position = position_dodge(.9))+
    geom_point(alpha=1, colour = '#ffa951', size = 4)+
    geom_line(aes(group = participant),alpha = .3)+
    stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9), shape = 23, size = 4, fill = '#23375f')+
    stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9), width = .2, colour = '#23375f')+
    labs(x = "Number of Precues", y = "Efficacy")+
    scale_x_discrete(breaks = c('twoStreams', 'eightStreams'), labels = c('2', '8'))+
    lims(y = c(0,1))+
    theme_apa()
  
  
  results[['Efficacy']] <- list(
    'BF' = efficacyBFFullVSNull,
    'Plot' = efficacyPlot
  )
  
  #######################
  ###Latency Analyses###
  ####################### 
  if(bestFitting){
    latencyBFFullVSNull <- anovaBF(latency ~ condition + participant,
                                   data=params,
                                   whichRandom = 'participant', 
                                   progress = FALSE
    )
  } else {
    xLatency <- x %>% pull(latency)
    yLatency <- y %>% pull(latency)
    latencyBFFullVSNull <- ttestBF(x = xLatency, y = yLatency, paired = T)
  }
  
  
  BayesFactorLabelOne <- latencyBFFullVSNull %>% as.vector %>% round(2) %>% paste0('BF[10]==', .)
  
  
  latencyPlot <- ggplot(params, aes(x=condition, y = latency))+
    #geom_violin(position = position_dodge(.9))+
    geom_point(alpha=1, colour = '#ffa951', size = 4)+
    geom_line(aes(group = participant),alpha = .3)+
    stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9), shape = 23, size = 4, fill = '#23375f')+
    stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9), width = .2, colour = '#23375f')+
    scale_colour_brewer(palette = 'Spectral')+
    labs(x = 'Number of Precues', y = 'Latency (ms)')+
    scale_x_discrete(breaks = c('twoStreams', 'eightStreams'), labels = c('2', '8'))+
    theme_apa()
  
  
  results[['Latency']] <- list(
    'BF' = latencyBFFullVSNull,
    'Plot' = latencyPlot 
  )
  
  ########################
  ###Precision Analyses###
  ########################
  if(bestFitting){
    precisionBFFullVSNull <- anovaBF(precision ~ condition + participant,
                                     data=params,
                                     whichRandom = 'participant', 
                                     progress = FALSE
    )
  } else {
    xPrecision <- x %>% pull(precision)
    yPrecision <- y %>% pull(precision)
    precisionBFFullVSNull <- ttestBF(x = xPrecision, y = yPrecision, paired = T)
  }
  
  BayesFactorLabelOne <- precisionBFFullVSNull %>% as.vector %>% round(2) %>% paste0('BF[10]==', .)
  
  
  precisionPlot <- ggplot(params, aes(x=condition, y = precision))+
    #geom_violin(position = position_dodge(.9))+
    geom_point(alpha=1, colour = '#ffa951', size = 4)+
    geom_line(aes(group = participant),alpha = .3)+
    stat_summary(geom = 'point', fun.y = mean, position = position_dodge(.9), shape = 23, size = 4, fill = '#23375f')+
    stat_summary(geom= 'errorbar', fun.data = mean_se, position = position_dodge(.9), width = .2, colour = '#23375f')+
    scale_colour_brewer(palette = 'Spectral')+
    labs(x = 'Number of Precues', y = 'Precision (ms)')+
    scale_x_discrete(breaks = c('twoStreams', 'eightStreams'), labels = c('2', '8'))+
    scale_y_continuous(limits = c(0,230), breaks = seq(0, 200, 50))+
    theme_apa()
  
  results[['Precision']] <- list(
    'BF' =  precisionBFFullVSNull,
    'Plot' = precisionPlot 
  )
  
  results
}

guessingDistributionBIC <- function(df){
  columnsPresent <- all(c('val','valGuessing') %in% colnames(df))
  if(!columnsPresent){
    missingCols <- c('val','valGuessing')[!c('val','valGuessing') %in% colnames(df)]
    stop('Missing ', missingCols, ' from df')
  }
  
  BICVal <- log(100)*3+(2*df$val)
  BICValGuessing <- log(100)*0+(2*df$valGuessing)
  
  deltaBIC <- BICValGuessing - BICVal
  
  data.frame(BFGuessVsMix = exp(deltaBIC/2))
}


timeStamp <- Sys.time() %>% strftime(format = '%d-%m-%Y_%H-%M')

allData <- read.table('Experiment3/Analysis/allErrors.txt', sep = '\t', stringsAsFactors = F, header = T)

allData %<>% rename(SPE = error)


IDs <- allData %>% pull(ID) %>% unique()
conditions <-  allData %>% pull(condition) %>% unique()

nReplications = 20
numLettersInStream <- 24

paramsDF <- expand.grid(
  participant = IDs,
  condition = conditions,
  efficacy = numeric(1),
  latency = numeric(1),
  precision = numeric(1),
  val = numeric(1),
  valGuessing = numeric(1),
  pLRtest = 999,
  model = c('Gamma','Normal')
)

parameterBoundsGamma <- data.frame(
  lower = c(0,0.1,0.1),
  upper = c(1,8,5)
)

parameterBoundsNormal <- data.frame(
  lower = c(0, -4, .001),
  upper = c(1, 4, 3)
)

nObs <- allData %>%
  filter(!fixationReject) %>%
  group_by(ID, condition) %>%
  summarise(n = n()) %>%
  rename(participant = ID)

nParams <- c('Gamma' = 3, 'Normal' = 3)

paramFiles <- list.files(path = 'Experiment3/Analysis/Gamma Fits',
                         pattern = 'paramsDFPrecue.*csv',
                         full.name = T)

if(length(paramFiles)>0){
  paramsDF <- read.csv(paramFiles[1], stringsAsFactors = F)
} else{
  for(thisParticipant in IDs){
    for(thisCondition in conditions){
        theseData <- allData %>% filter(ID == thisParticipant, condition == thisCondition, !fixationReject)
        
        cat('Participant: ', thisParticipant, '. Condition: ', thisCondition,'. Fitting gamma model                                  \r', sep = '')
        paramsGamma <- theseData %>% analyzeOneConditionDF(numLettersInStream, parameterBoundsGamma, nReplicates = nReplications, modelKind = 'Gamma')
        cat('Participant: ', thisParticipant, '. Condition: ', thisCondition, '. Fitting normal model                                  \r', sep = '')
        paramsN <- theseData %>% analyzeOneConditionDF(numLettersInStream, parameterBoundsNormal, nReplicates = nReplications, modelKind = 'Normal')
        
        paramsDF %<>% mutate(
          efficacy = replace(efficacy, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$efficacy),
          latency = replace(latency, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$latency),
          precision = replace(precision, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$precision),
          val = replace(val, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$val),
          valGuessing = replace(valGuessing, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$valGuessing),
          pLRtest = replace(pLRtest, participant == thisParticipant & condition == thisCondition & model == 'Normal', paramsN$pLRtest)
        )
        
        paramsDF %<>% mutate(
          efficacy = replace(efficacy, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$efficacy),
          latency = replace(latency, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$latency),
          precision = replace(precision, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$precision),
          val = replace(val, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$val),
          valGuessing = replace(valGuessing, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$valGuessing),
          pLRtest = replace(pLRtest, participant == thisParticipant & condition == thisCondition & model == 'Gamma', paramsGamma$pLRtest)
        )
    }
  }
  BICs <- paramsDF %>%
    compareMixtureModels(params = .,
                         nParam = nParams,
                         nObs = nObs,
                         variables = c('condition', 'participant'))
  
  
  paramsDF <- BICs %>% mutate(favouredModel = 'Neither') %>%
    mutate(favouredModel = replace(favouredModel, BF >3, 'Normal')) %>%
    mutate(favouredModel = replace(favouredModel, BF<3^-1, 'Gamma')) %>%
    select(participant, condition, BF, favouredModel) %>%
    left_join(paramsDF, ., by = c('participant', 'condition')) #Add all this information to the paramDF 
  
  paramsFile <- paste0('Experiment3/Analysis/Gamma Fits/paramsDF8_', timeStamp, '.csv')
  
  write.csv(file = paramsFile,
            x = paramsDF, 
            row.names = F)
}

#########################################################
###Compute BFs comparing the mixtture to guessing only###
#########################################################

paramsDF %<>% 
  group_by(participant, condition, model) %>% 
  do(guessingDistributionBIC(.)) %>% 
  mutate(
    guessOrMixture = ifelse(BFGuessVsMix < 1/3, 'Guessing Only', 
                            ifelse(BFGuessVsMix > 3, 'Mixture', 'Neither')
    )
  ) %>% 
  left_join(paramsDF, ., by = c('participant', 'condition', 'model'))

paramsDF %<>%  mutate(
  latency = replace(latency, guessOrMixture == 'Guessing Only', NA),
  precision = replace(precision, guessOrMixture == 'Guessing Only', NA),
  efficacy = replace(efficacy, guessOrMixture == 'Guessing Only', 0)
)


#################################################################
###Create shape and scale variables. Compute gamma mean and sd###
#################################################################

paramsDF %<>% mutate(
  shape = ifelse(model == 'Gamma', latency, NA),
  scale = ifelse(model == 'Gamma', precision, NA),
) %>% mutate(
  latency = ifelse(model == 'Gamma', shape*scale, latency),
  precision = ifelse(model == 'Gamma', sqrt(shape*(scale^2)), precision)
)

######################
###Model fit counts###
######################

paramsDF %>% filter(model == 'Normal') %>% 
  group_by(condition) %>%
  summarise(
    Normal = sum(favouredModel == 'Normal'),
    Attention = sum(favouredModel == 'Gamma'),
    Neither = sum(favouredModel == 'Neither')
  )


#############
###Density###
#############


densities <- paramsDF %>% #Purrr is new to me
  select(participant, condition, efficacy, latency, precision, shape, scale, model, favouredModel)%>% #Select the columns with the variables we want
  pmap_dfr(function(participant, condition, efficacy, latency, shape, scale, precision, model, favouredModel){ #For each row, compute the density over a range of milliseconds and return a dataframe
    SPE <- seq(-5,10,.1)
    print(paste0('favouredModel: ', favouredModel, '. model: ', model, '. participant: ', participant))
    if(efficacy > 0){
      if(favouredModel == 'Gamma'){
        if(model == 'Gamma'){
          density =dgamma(SPE, shape = shape, scale = scale)
          data.frame(ID = participant,
                     condition = condition,
                     SPE = SPE,
                     density = density,
                     model = model,
                     favouredModel = favouredModel,
                     stringsAsFactors = F)
        } 
      } else if(favouredModel == 'Normal'){
        if(model == 'Normal'){
          density = dnorm(SPE, latency, precision)
          data.frame(ID = participant,
                     condition = condition,
                     SPE = SPE,
                     density = density,
                     model = model,
                     favouredModel = favouredModel,
                     stringsAsFactors = F)
        }
      } else {
        if(model == 'Gamma'){
          density =dgamma(SPE, shape = shape, scale = scale)
          data.frame(ID = participant,
                     condition = condition,
                     SPE = SPE,
                     density = density,
                     model = model,
                     favouredModel = favouredModel,
                     stringsAsFactors = F)
        } else if(model == 'Normal'){
          density = dnorm(SPE, latency, precision)
          data.frame(ID = participant,
                     condition = condition,
                     SPE = SPE,
                     density = density,
                     model = model,
                     favouredModel = favouredModel,
                     stringsAsFactors = F)
        }
      }
    }
  }
  )



for(thisID in IDs){
  theseObservations <- allData %>% filter(ID == thisID)
  
  theseDensities <- densities %>% filter(ID == thisID)
  
  thisIDNoSpace <- thisID %>% gsub(pattern = ' ', replacement = '', x = .)
  
  thisPlot <- ggplot(theseObservations, aes(x = SPE))+
    geom_histogram(binwidth = 1)+
    geom_line(data = theseDensities, aes(x = SPE, y = density*100, colour = model)) +
    labs(title = paste0('Participant: ', thisIDNoSpace))+
    geom_vline(xintercept = 0, linetype = 'dashed')+
    facet_grid(cols = vars(condition))
  
  ggsave(filename = paste0('Experiment3/Analysis/Gamma Fits/GammaPlots/',thisIDNoSpace,'.png'),
         plot = thisPlot, width = 29.21, height = 12.09, units = 'cm'
        )
} 

#################################
###Latencies relative to onset###
#################################

paramsDF %<>% mutate(
  latency = latency * (1000/12),
  precision = precision*(1000/12),
  latencyRelativeOnset = latency + 11.5,
  condition = ordered(condition, levels = c('twoStreams', 'eightStreams'))
)

normalAnalysis <- analyses(paramsDF,modelKind = 'Normal')

plotHeight <- 19*(9/16)
plotWidth <- 19

ggsave('Experiment3/Analysis/Gamma Fits/efficacy.png', plot = normalAnalysis$Efficacy$Plot,height = plotHeight, width = plotWidth, units = 'cm')
ggsave('Experiment3/Analysis/Gamma Fits/latency.png', plot = normalAnalysis$Latency$Plot,height = plotHeight, width = plotWidth, units = 'cm')
ggsave('Experiment3/Analysis/Gamma Fits/precision.png', plot = normalAnalysis$Precision$Plot,height = plotHeight, width = plotWidth, units = 'cm')


allResponses = allData %>% 
  filter(!fixationReject) %>%
  mutate(condition = ifelse(condition == 'twoStreams', 'Two Precues', 'Eight Precues'))%>%
  mutate(condition = ordered(condition, levels = paste(c('Two', 'Eight'), 'Precues')))%>%
  ggplot(aes(x = SPE))+
  geom_histogram(aes(x = SPE), binwidth = 1, size = 1, fill = '#FFFFFF', colour = 'black')+
  facet_grid(cols = vars(condition))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  labs(y = 'Count')+
  theme_apa(base_size = 30)

allResponses

ggsave('Experiment3/Analysis/aggregateHistograms.png', allResponses,width = 16, height = 9, units = 'in')

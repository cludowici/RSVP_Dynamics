nStreams Analysis
================
Charlie Ludowici
5/18/2017

``` r
library(ggplot2)
library(reshape2)
library(papaja)
library(BayesFactor)
```

    ## Loading required package: coda

    ## Loading required package: Matrix

    ## ************
    ## Welcome to BayesFactor 0.9.12-4.1. If you have questions, please contact Richard Morey (richarddmorey@gmail.com).
    ## 
    ## Type BFManual() to open the manual.
    ## ************

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(magrittr)
library(truncnorm)
library(effsize)
theme_set(theme_apa(base_size = 15) ) 

inclusionBF <- function(model, variable){
  
  ###https://www.cogsci.nl/blog/interpreting-bayesian-repeated-measures-in-jasp###

  
  priorProbs <- model %>% newPriorOdds() %>% `*`(model) %>% as.BFprobability() %>% as.vector() 
  
  theseNames <- names(priorProbs)
  nProbs <- 1:length(priorProbs)
  variableMatches <- grep(variable, theseNames)
  
  if(grepl(':', variable)){
    subordinateVariables <- variable %>% strsplit(':') %>% unlist()
    
    thisRegex <- paste0(subordinateVariables,collapse = '.*\\+.*')
    
    subordinateEffects <- grep(thisRegex, theseNames, perl = T)
    subordinateEffects <- subordinateEffects[!subordinateEffects %in% variableMatches]
    
    
    sum(priorProbs[variableMatches])/sum(priorProbs[subordinateEffects])
  } else {
    interactionMatches <- grep(paste0(variable,'(?=:)|(?<=:)',variable), theseNames, perl = T)
    
    variableMainEffects <- variableMatches[!variableMatches %in% interactionMatches]
    
    
    otherMainEffects <- nProbs[!nProbs %in% c(variableMainEffects,interactionMatches)]
    
    
    sum(priorProbs[variableMainEffects])/sum(priorProbs[otherMainEffects])
  }
}

savePlots <- T

posterior <- function(t, N1, N2=NULL, delta, lo=-Inf, hi = Inf,
                      priorMean=0,priorSD=1) {
        N = ifelse(is.null(N2), N1, N1*N2/(N1+N2))
        df  = ifelse(is.null(N2), N1 - 1, N1 + N2 - 2)
        
        #prior and likelihood
        #prior <- function(delta) dnorm(delta, priorMean, priorSD)*as.integer(delta >= lo)*as.integer(delta <= hi) 
        prior <- function(delta) dcauchy(delta, priorMean, priorSD)*as.integer(delta >= lo)*as.integer(delta <= hi) 
        K=1/integrate(prior,lower=lo,upper=hi)[[1]]
        f=function(delta) K*prior(delta)
        
        #(The as.integer bits above just provide bounds for the prior if you want them)
      
        likelihood <- function(delta) dt(t, df, delta*sqrt(N))
        
        #marginal likelihood
        marginal <- integrate(function(x) f(x)*likelihood(x), lo, hi)[[1]]
        
        #posterior
        post <- function(x) f(x)*likelihood(x) / marginal
        return(post(delta))
}
```

``` r
null = 0

allErrors <- read.table('../allErrors.txt', sep = '\t', header = T)

LatencyNorm <- read.csv('../../modelOutput/8Streams/CSV/TGRSVP_Exp2_LatencyNorm.csv')
LatencyTNorm <- read.csv('../../modelOutput/8Streams/CSV/TGRSVP_Exp2_LatencyTruncNorm.csv')

PrecisionNorm <- read.csv('../../modelOutput/8Streams/CSV/TGRSVP_Exp2_PrecisionNorm.csv')
PrecisionTNorm <- read.csv('../../modelOutput/8Streams/CSV/TGRSVP_Exp2_PrecisionTruncNorm.csv')

EfficacyNorm <- read.csv('../../modelOutput/8Streams/CSV/TGRSVP_Exp2_EfficacyNorm.csv')
EfficacyTNorm <- read.csv('../../modelOutput/8Streams/CSV/TGRSVP_Exp2_EfficacyTruncNorm.csv')
```

\#Model
fits

``` r
BFs <- read.csv('../../modelOutput/8Streams/BF_ByParticipant.csv', header = T)

BFsWide <- BFs %>% dcast(Participant~Group, value.var = 'BF') %>% mutate(
  eightStreams = round(eightStreams,2), 
  twoStreams = formatC(twoStreams, format = 'e', digits = 2))

allEfficacy <- rbind(EfficacyNorm,EfficacyTNorm)
allEfficacy$participantN <- factor(rep(1:10, times =4))

efficacyModelBF <- anovaBF(Estimate~Model*Group+participantN, data = allEfficacy, whichRandom = 'participantN')

efficacyModelInclusionBF <- expand.grid(factor = c('Group','Model','Group:Model'), BF = numeric(1))

for(thisFactor in efficacyModelInclusionBF$factor){
  efficacyModelInclusionBF %<>% mutate(BF = replace(BF, factor == thisFactor, inclusionBF(efficacyModelBF, thisFactor)))
  print(inclusionBF(efficacyModelBF, thisFactor))
}
```

    ## [1] 22.82452
    ## [1] 11.56858
    ## [1] 8.11659

``` r
knitr::kable(efficacyModelInclusionBF)
```

| factor      |        BF |
| :---------- | --------: |
| Group       | 22.824517 |
| Model       | 11.568584 |
| Group:Model |  8.116589 |

``` r
allLatency <- rbind(LatencyNorm,LatencyTNorm)
allLatency$participantN <- factor(rep(1:10, times =4))

latencyModelBF <- anovaBF(Estimate~Model*Group+participantN, data = allLatency, whichRandom = 'participantN')

latencyModelInclusionBF <- expand.grid(factor = c('Group','Model','Group:Model'), BF = numeric(1))

for(thisFactor in latencyModelInclusionBF$factor){
  latencyModelInclusionBF %<>% mutate(BF = replace(BF, factor == thisFactor, inclusionBF(latencyModelBF, thisFactor)))
  print(inclusionBF(latencyModelBF, thisFactor))
}
```

    ## [1] 19848277
    ## [1] 1.002693
    ## [1] 1.134213

``` r
knitr::kable(latencyModelInclusionBF)
```

| factor      |           BF |
| :---------- | -----------: |
| Group       | 1.984828e+07 |
| Model       | 1.002693e+00 |
| Group:Model | 1.134213e+00 |

``` r
allPrecision <- rbind(PrecisionNorm,PrecisionTNorm)
allPrecision$participantN <- factor(rep(1:10, times =4))

precisionModelBF <- anovaBF(Estimate~Model*Group+participantN, data = allPrecision, whichRandom = 'participantN')

precisionModelInclusionBF <- expand.grid(factor = c('Group','Model','Group:Model'), BF = numeric(1))

for(thisFactor in precisionModelInclusionBF$factor){
  precisionModelInclusionBF %<>% mutate(BF = replace(BF, factor == thisFactor, inclusionBF(precisionModelBF, thisFactor)))
  print(inclusionBF(precisionModelBF, thisFactor))
}
```

    ## [1] 472.6124
    ## [1] 9.95198
    ## [1] 9.695074

``` r
knitr::kable(precisionModelInclusionBF)
```

| factor      |         BF |
| :---------- | ---------: |
| Group       | 472.612399 |
| Model       |   9.951980 |
| Group:Model |   9.695074 |

\#Latency
Analyses

``` r
latencyTwo <- LatencyNorm %>% filter(Group == 'twoStreams') %>% pull(Estimate)
latencyEight <- LatencyNorm %>% filter(Group == 'eightStreams') %>% pull(Estimate)

latencyTFreq  <- t.test(x = latencyTwo,
                        y = latencyEight, 
                        paired = T)

tLatency <- latencyTFreq$statistic[[1]]

N1 <- 10
N2 <- 10

priorMean = null
priorSD = sqrt(.5)

#examples of BF via savage-dickey ratio
#2-sided
BF10 = dcauchy(null,priorMean,priorSD) / posterior(tLatency, N1, delta=null,
                                              priorMean=priorMean,priorSD=priorSD)

#one-sided BF
BFplus = ( 2 * dcauchy(null,priorMean,priorSD) ) / posterior(tLatency, N1, delta=null, lo=0,
                                            priorMean=priorMean,priorSD=priorSD)

BF10
```

    ## [1] 242.5306

``` r
BFplus
```

    ## [1] 0.09212442

``` r
delta  <- seq(-2, 4, .01)

posteriorAndPriorDF <- data.frame(
  delta = delta, 
  posterior = posterior(tLatency,N1,delta=delta, priorMean=priorMean,priorSD=priorSD), 
  prior = dcauchy(delta, priorMean,priorSD))

posteriorModeLatency <- optimize(function(delta) posterior(tLatency, N1, delta=delta, priorMean=priorMean, priorSD=priorSD), 
                                 interval=c(-4,4),
                                 maximum = T)[[1]]

cohen.d(Estimate ~ Group, LatencyNorm, paired = T)
```

    ## 
    ## Cohen's d
    ## 
    ## d estimate: -2.033029 (large)
    ## 95 percent confidence interval:
    ##        inf        sup 
    ## -3.1901202 -0.8759371

``` r
#This would only work for normal, we use Cauchy!
#credibleIntervalDensityLower <- mean(posteriorAndPriorDF$posterior)-sd(posteriorAndPriorDF$posterior)*1.96
#credibleIntervalDensityUpper <- mean(posteriorAndPriorDF$posterior)+sd(posteriorAndPriorDF$posterior)*1.96


LatencyNormPlot <- ggplot(LatencyNorm,aes(x=Group, y=Estimate))+
  geom_violin()+
  geom_line(aes(group=Participant, colour=Participant))+
  geom_point(aes(colour=Participant), size = 3)+
  scale_colour_brewer(palette = 'Spectral')+
  labs(x='Condition',y='Estimate (ms)',title='Latency')+
  theme(plot.title = element_text(hjust=.5))

show(LatencyNormPlot)
```

![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
wideFormatLatency <- dcast(data=LatencyNorm,formula = Participant~Group)
```

    ## Using Estimate as value column: use value.var to override.

``` r
LatencyNormScatter <- ggplot(wideFormatLatency, aes(x=twoStreams, y=eightStreams))+
  geom_point(size = 4, aes(colour=ordered(1:10)))+
  scale_color_brewer(palette='Spectral', name='Participant')+
  lims(x=c(20,120), y=c(20,120))+
  labs(title='Latency Estimates (ms)', x = 'Two Streams', y='Eight Streams')+
  theme(plot.title = element_text(size=15, hjust=.5))+
  annotate('text', x=100, y=45, label = paste0('BF10 = ', round(BF10,2)))+
  annotate('text', x = 100, y=37, label = paste0('Effect size = ', round(posteriorModeLatency,2)))+
  geom_abline(intercept = 0, slope = 1,linetype='dashed')

show(LatencyNormScatter)
```

![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-4-2.png)<!-- -->

``` r
LatencyNormBayesPlot <- ggplot(posteriorAndPriorDF, aes(x=delta))+
  geom_line(aes(y=posterior, linetype = 'Posterior'))+
  geom_line(aes(y=prior, linetype = 'Prior'))+
  scale_linetype_manual(values = c('solid','dashed'),  guide = 'legend', name = NULL)+
  labs(x = expression(delta), y='Density', title = 'Latency Effect Size')

show(LatencyNormBayesPlot)
```

![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-4-3.png)<!-- -->

\#Precision
Analysis

``` r
precisionTwo <- PrecisionNorm %>% filter(Group == 'twoStreams') %>% pull(Estimate)
precisionEight <- PrecisionNorm %>% filter(Group == 'eightStreams') %>% pull(Estimate)

precisionTFreq  <- t.test(x = precisionTwo,
                        y = precisionEight, 
                        paired = T)

tPrecision <- precisionTFreq$statistic[[1]]

t  <- tPrecision
N1 <- 10
N2 <- 10

priorMean =0
priorSD = sqrt(.5)

#examples of BF via savage-dickey ratio
#2-sided
BF10 = dcauchy(0,priorMean,priorSD) / posterior(tPrecision, N1, delta=0,
                                              priorMean=priorMean,priorSD=priorSD)

#one-sided BF
BFplus = ( 2 * dcauchy(0,priorMean,priorSD) ) / posterior(tPrecision, N1, delta=0, lo=0,
                                            priorMean=priorMean,priorSD=priorSD)

BF10
```

    ## [1] 41.11226

``` r
BFplus
```

    ## [1] 82.12754

``` r
delta  <- seq(-4, 2, .01)

posteriorModePrecision <- optimize(function(delta) posterior(tPrecision, N1, delta=delta,priorMean=priorMean,priorSD=priorSD), interval=c(-4,4),maximum = T)[[1]]

posteriorAndPriorDF <- data.frame(delta = delta, 
                                  posterior = posterior(tPrecision,N1,delta=delta,
                                                    priorMean=priorMean,priorSD=priorSD), 
                                  prior = dcauchy(delta, priorMean,priorSD))

posteriorModePrecision <- optimize(function(delta) posterior(tPrecision, N1, delta=delta, priorMean=priorMean, priorSD=priorSD), interval=c(-4,4),maximum = T)[[1]]


PrecisionNormPlot <- ggplot(PrecisionNorm,aes(x=Group, y=Estimate))+
  geom_violin()+
  geom_line(aes(group=Participant, colour=Participant))+
  geom_point(aes(colour=Participant),alpha=.8, size = 3)+
  scale_colour_brewer(palette = 'Spectral')+
  labs(x='Condition',y='Estimate (ms)',title='Precision')+
  theme(plot.title = element_text(hjust=.5))

show(PrecisionNormPlot)
```

![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
wideFormatPrecision <- dcast(data=PrecisionNorm,formula = Participant~Group)
```

    ## Using Estimate as value column: use value.var to override.

``` r
PrecisionNormScatter <- ggplot(wideFormatPrecision, aes(x=twoStreams, y=eightStreams, colour=ordered(1:10)))+
  geom_point(size = 4)+
  scale_color_brewer(palette='Spectral', name='Participant')+
  lims(x=c(40,100), y=c(40,100))+
  labs(title='Precision Estimates (ms)')+
  theme(plot.title = element_text(size=15, hjust=.5))+
  annotate('text', x=90, y=70, label = paste0('BF10 = ', round(BF10,2)))+
  annotate('text', x = 90, y=66, label = paste0('Effect size = ', round(posteriorModePrecision,2)))+
  geom_abline(intercept = 0, slope = 1,linetype='dashed')

show(PrecisionNormScatter)
```

![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
PrecisionNormBayesPlot <- ggplot(posteriorAndPriorDF, aes(x=delta))+
  geom_line(aes(y=posterior, linetype = 'Posterior'))+
  geom_line(aes(y=prior, linetype = 'Prior'))+
  scale_linetype_manual(values = c('solid','dashed'),  guide = 'legend', name = NULL)+
  labs(x = expression(delta), y='Density', title = 'Precision Effect Size')
```

\#Efficacy
Analysis

``` r
efficacyTwo <- EfficacyNorm %>% filter(Group == 'twoStreams') %>% pull(Estimate)
efficacyEight <- EfficacyNorm %>% filter(Group == 'eightStreams') %>% pull(Estimate)

efficacyTFreq  <- t.test(x = efficacyTwo,
                        y = efficacyEight, 
                        paired = T)

tEfficacy <- efficacyTFreq$statistic[[1]]
N1 <- 10
N2 <- 10

priorMean =0
priorSD = sqrt(.5)

#examples of BF via savage-dickey ratio
#2-sided
BF10 = dcauchy(0,priorMean,priorSD) / posterior(tEfficacy, N1, delta=0,
                                              priorMean=priorMean,priorSD=priorSD)

#one-sided BF
BFplus = ( 2 * dcauchy(0,priorMean,priorSD) ) / posterior(tEfficacy, N1, delta=0, lo=0,
                                            priorMean=priorMean,priorSD=priorSD)

BF10
```

    ## [1] 0.3755812

``` r
BFplus
```

    ## [1] 0.2042808

``` r
delta  <- seq(-2, 4, .01)

posteriorAndPriorDF <- data.frame(delta = delta, posterior = posterior(tEfficacy ,N1,delta=delta,
                                                                       priorMean=priorMean,priorSD=priorSD), prior = dcauchy(delta, priorMean,priorSD))

posteriorModeEfficacy <- optimize(function(delta) posterior(tEfficacy, N1, delta=delta,priorMean=priorMean,priorSD=priorSD), interval=c(-4,4),maximum = T)[[1]]


EfficacyNormPlot <- ggplot(EfficacyNorm, aes(x=Group, y=Estimate))+
  geom_violin()+
  geom_line(aes(group=Participant, colour=Participant))+
  geom_point(aes(colour = Participant), size = 3)+
  labs(x='Condition',y='Estimate',title='Efficacy')+
  theme(plot.title = element_text(hjust=.5))+
  scale_colour_brewer(palette = 'Spectral')

show(EfficacyNormPlot)
```

![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
wideFormatEfficacy <- dcast(data=EfficacyNorm,formula = Participant~Group)
```

    ## Using Estimate as value column: use value.var to override.

``` r
EfficacyNormScatter <- ggplot(wideFormatEfficacy, aes(x=twoStreams, y=eightStreams, colour=ordered(1:10)))+
  geom_point(size = 4)+
  scale_color_brewer(palette='Spectral', name='Participant')+
  lims(x=c(0,1), y=c(0,1))+
  labs(title='Efficacy Estimates [1 - P(Guess)]', y = 'Eight Streams', y='Eight Streams')+
  theme(plot.title = element_text(size=15, hjust=.5))+
  annotate('text', x=.8, y=.45, label = paste0('BF10 = ', round(BF10,2)))+
  annotate('text', x = .8, y=.37, label = paste0('Effect size = ', round(posteriorModeEfficacy,2)))+
  geom_abline(intercept = 0, slope = 1,linetype='dashed')

show(EfficacyNormScatter)
```

![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-6-2.png)<!-- -->

``` r
EfficacyNormBayesPlot <- ggplot(posteriorAndPriorDF, aes(x=delta))+
  geom_line(aes(y=posterior, linetype = 'Posterior'))+
  geom_line(aes(y=prior, linetype = 'Prior'))+
  scale_linetype_manual(values = c('solid','dashed'),  guide = 'legend', name = NULL)+
  labs(x = expression(delta), y='Density', title = 'Efficacy Effect Size')
```

``` r
propBeforeCue <- expand.grid(
  Participant = unique(LatencyNorm$Participant),
  Group = unique(LatencyNorm$Group),
  Proportion = -999,
  nTrialsBeforeCue = -999
)

for(thisParticipant in unique(LatencyNorm$Participant)){
  for(thisCondition in unique(LatencyNorm$Group)){
    
    thisNormLatency <- LatencyNorm %>% filter(Participant == thisParticipant & Group == thisCondition) %>% pull(Estimate)
    thisNormPrecision <- PrecisionNorm %>% filter(Participant == thisParticipant & Group == thisCondition) %>% pull(Estimate)
    
    thisNormEfficacy <- EfficacyNorm %>% filter(Participant == thisParticipant & Group == thisCondition) %>% pull(Estimate)
    
    thisNTrials <- allErrors %>% filter(ID == thisParticipant & condition == thisCondition & !fixationReject) %>% nrow 
    
    thisProportionBeforeCue <- pnorm(0, thisNormLatency, thisNormPrecision)
    thisNEfficaciousBeforeCue <- thisNTrials * thisNormEfficacy * thisProportionBeforeCue
    
    propBeforeCue %<>% mutate(Proportion = replace(Proportion, Participant==thisParticipant & Group == thisCondition, thisProportionBeforeCue))
   
    propBeforeCue %<>% mutate(nTrialsBeforeCue = replace(nTrialsBeforeCue, Participant==thisParticipant & Group == thisCondition, thisNEfficaciousBeforeCue)) 
  }
}

propBeforeCue %>% ggplot(., aes(x = Group,y = Proportion))+
  geom_violin()+
  geom_point()+
  stat_summary(fun.y = mean, geom = 'point', size = 5, shape = 5)+
  stat_summary(fun.data = mean_se, geom='errorbar', width = .2)
```

![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
propBeforeCue %>% group_by(Group) %>% summarise(proportion = mean(Proportion))
```

    ## # A tibble: 2 x 2
    ##   Group        proportion
    ##   <fct>             <dbl>
    ## 1 eightStreams     0.0636
    ## 2 twoStreams       0.270

``` r
propBeforeCue %>% group_by(Group) %>% summarise(mean = mean(nTrialsBeforeCue), sd = sd(nTrialsBeforeCue))
```

    ## # A tibble: 2 x 3
    ##   Group         mean    sd
    ##   <fct>        <dbl> <dbl>
    ## 1 eightStreams  6.63  3.37
    ## 2 twoStreams   27.3  12.2

``` r
knitr::kable(propBeforeCue)
```

| Participant | Group        | Proportion | nTrialsBeforeCue |
| :---------- | :----------- | ---------: | ---------------: |
| AJ7         | twoStreams   |  0.3723110 |        40.512273 |
| AN14        | twoStreams   |  0.2076868 |        20.186414 |
| BB6         | twoStreams   |  0.3449912 |        40.281685 |
| IK4         | twoStreams   |  0.2010520 |        22.676133 |
| JA8         | twoStreams   |  0.4560700 |        45.892684 |
| LH9         | twoStreams   |  0.1715194 |        18.358339 |
| LS2         | twoStreams   |  0.3321078 |        29.860646 |
| LT5         | twoStreams   |  0.2016522 |        25.142964 |
| RN12        | twoStreams   |  0.0873701 |         5.547129 |
| YZ15        | twoStreams   |  0.3271893 |        24.086366 |
| AJ7         | eightStreams |  0.0939610 |        10.288167 |
| AN14        | eightStreams |  0.0371011 |         3.493398 |
| BB6         | eightStreams |  0.0837935 |         9.306187 |
| IK4         | eightStreams |  0.0301000 |         2.662444 |
| JA8         | eightStreams |  0.1123735 |        11.191320 |
| LH9         | eightStreams |  0.0875259 |        10.723908 |
| LS2         | eightStreams |  0.0410124 |         4.243084 |
| LT5         | eightStreams |  0.0461194 |         5.663203 |
| RN12        | eightStreams |  0.0680447 |         5.339233 |
| YZ15        | eightStreams |  0.0359095 |         3.425442 |

``` r
ttestBF(x = propBeforeCue$Proportion[propBeforeCue$Group=='twoStreams'],
        y = propBeforeCue$Proportion[propBeforeCue$Group=='eightStreams'],
        data = propBeforeCue,
        paired = T)
```

    ## Bayes factor analysis
    ## --------------
    ## [1] Alt., r=0.707 : 214.9873 ±0%
    ## 
    ## Against denominator:
    ##   Null, mu = 0 
    ## ---
    ## Bayes factor type: BFoneSample, JZS

``` r
x <- ttestBF(x = propBeforeCue$Proportion[propBeforeCue$Group=='eightStreams'],
        data = propBeforeCue,
        mu = 0) 

ttestBF(x = propBeforeCue$Proportion[propBeforeCue$Group=='twoStreams'],
        data = propBeforeCue, mu = 0)
```

    ## Bayes factor analysis
    ## --------------
    ## [1] Alt., r=0.707 : 723.6108 ±0%
    ## 
    ## Against denominator:
    ##   Null, mu = 0 
    ## ---
    ## Bayes factor type: BFoneSample, JZS

``` r
# 
# EfficacyNorm$Parameter <- as.character('Efficacy')
# LatencyNorm$Parameter <- as.character('Latency')
# PrecisionNorm$Parameter <- as.character('Precision')
# 
# allParams <- rbind(EfficacyNorm,LatencyNorm,PrecisionNorm)
# 
# allParams <- melt(allParams, measure.vars = c('twoStreams','eightStreams'), variable.name = 'Condition',value.name = 'Estimate')
# 
# paramBar <- ggplot(allParams[!allParams$Parameter=='Efficacy',], aes(x=Parameter,y=Estimate, fill = Condition))+
#   stat_summary(geom='bar', fun.y = mean, position = position_dodge(.9))+
#   stat_summary(geom='errorbar', fun.data=mean_se, position = position_dodge(.9), width = .3)+
#   scale_fill_brewer(palette = 'Spectral')
# 
# paramBar
# 
# predictions <- data.frame(twoStreams = rnorm(1000, LatencyNorm$twoStreams[7], PrecisionNorm$twoStreams[7]), eightStreams = rnorm(1000, LatencyNorm$eightStreams[7], PrecisionNorm$eightStreams[7]))
# 
# predictions <- melt(predictions, measure.vars = c('twoStreams', 'eightStreams'), variable.name = 'Condition', value.name = 'response')
# 
# meanLatencyTwo <- mean(LatencyNorm$twoStreams)
# meanLatencyEight <- mean(LatencyNorm$eightStreams)
# 
# meanPrecisionTwo <- mean(PrecisionNorm$twoStreams)
# meanPrecisionEight <- mean(PrecisionNorm$eightStreams)
# 
# 
# predictionPlot <- ggplot()+
#   stat_function(data=data.frame(x=c(-400:400)/83.33), aes(x, fill = 'Eight Streams'), fun = dnorm, args = list(mean = meanLatencyEight/83.33, sd = meanPrecisionEight/83.33), geom='area', alpha = .9)+
#   stat_function(data=data.frame(x=c(-400:400)/83.33), aes(x, fill = 'Two Streams'), fun = dnorm, args = list(mean = meanLatencyTwo/83.33, sd = meanPrecisionTwo/83.33), geom='area', alpha = .9)+
#   scale_fill_manual(values=c('Two Streams' = '#628093', 'Eight Streams' = '#dca951'))+
#   scale_x_continuous(breaks = -3:4,limits = c(-3,4))+
#   labs(x='SPE', y=NULL, fill = 'Condition')
# 
# predictionPlot
# 
# 
# if(savePlots){
#   ggsave(PrecisionNormPlot, file = 'PrecisionNormViolin.png', height=15, width=20,units='cm')
#   ggsave(LatencyNormPlot, file = 'LatencyNormViolin.png', height=15, width=20,units='cm')
#   ggsave(EfficacyNormPlot, file = 'EfficacyNormViolin.png', height=15, width=20,units='cm')
#   
#   ggsave(PrecisionNormScatter, file = 'PrecisionNormScatter.png', height=15, width=20,units='cm')
#   ggsave(LatencyNormScatter, file = 'LatencyNormScatter.png', height=15, width=20,units='cm')
#   ggsave(EfficacyNormScatter, file = 'EfficacyNormScatter.png', height=15, width=20,units='cm')
#   
#   
#   ggsave(EfficacyNormBayesPlot, file = 'EfficacyNormEffectSize.png', height=15, width=20,units='cm')
#   ggsave(LatencyNormBayesPlot, file = 'LatencyNormEffectSize.png', height=15, width=20,units='cm')
#     ggsave(PrecisionNormBayesPlot, file = 'PrecisionNormEffectSize.png', height=15, width=20,units='cm')
# }
```

``` r
for(thisParticipant in unique(allErrors$ID)){
  densities <- data.frame(SPE = numeric(0), condition = character(0), normDensity = numeric(0), tNormDensity = numeric(0))
  for(thisCondition in unique(allErrors$condition)){
    
    thisNormLatency <- LatencyNorm %>% filter(Participant == thisParticipant & Group == thisCondition) %>% pull(Estimate)/(1000/12)
    thisNormPrecision <- PrecisionNorm %>% filter(Participant == thisParticipant & Group == thisCondition) %>% pull(Estimate)/(1000/12)
    
    thisTNormLatency <- LatencyTNorm %>% filter(Participant == thisParticipant & Group == thisCondition) %>% pull(Estimate)/(1000/12)
    thisTNormPrecision <- PrecisionTNorm %>% filter(Participant == thisParticipant & Group == thisCondition) %>% pull(Estimate)/(1000/12)
    
    theseErrors <- allErrors %>% filter(ID == thisParticipant & condition == thisCondition)
    
    #print(paste0('Participant: ', thisParticipant, '. Condition: ', thisCondition,'. N = ', nrow(theseErrors)))
    minError <- theseErrors %>% pull(error) %>% min
    maxError <- theseErrors %>% pull(error) %>% max
    thisRange <- seq(minError,maxError,.1)
    
    theseDensities <- data.frame(SPE = thisRange, 
                                 condition = rep(thisCondition, times = length(thisRange)),
                                 normDensity = dnorm(thisRange, thisNormLatency, thisNormPrecision), 
                                 tNormDensity = dtruncnorm(thisRange, a = thisTNormLatency-thisTNormPrecision, thisTNormLatency, thisTNormPrecision, b = Inf))
    
    densities <- rbind(densities, theseDensities)
    
    if(any(is.nan(theseDensities$density))){
      print(theseDensities)
    }
    
  
    # thisFileName <- paste0('modelOutput/Plots/',thisCondition,'/',thisRing,'/',thisParticipant,'-',format(Sys.time(), "%d-%m-%Y_%H-%M-%S"),'.png')
    # ggsave(filename = thisFileName, thisPlot,width = 16, height = 9)
  }
  errors <- allErrors %>% filter(ID == thisParticipant)
  thisPlot <- ggplot(errors, aes(x=error))+
      geom_histogram(binwidth = 1)+
      geom_line(data = densities, aes(x = SPE, y=tNormDensity*150))+ #scale density to histogram with density * N * binwidth
      geom_line(data = densities, aes(x = SPE, y=normDensity*150))+
      scale_y_continuous(sec.axis = sec_axis(~./nrow(theseErrors), name = 'Density'))+
      labs(y = 'Frequency', title = thisParticipant)+
    facet_wrap(~condition)
    
    show(thisPlot)
}
```

![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-9-5.png)<!-- -->![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-9-6.png)<!-- -->![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-9-7.png)<!-- -->![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-9-8.png)<!-- -->![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-9-9.png)<!-- -->![](nStreams_Bayesian_files/figure-gfm/unnamed-chunk-9-10.png)<!-- -->

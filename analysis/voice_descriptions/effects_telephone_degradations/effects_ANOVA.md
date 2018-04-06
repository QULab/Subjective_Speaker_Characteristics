Effects of speech bandwidth on perceived voice descriptions (ANOVA)
================
Laura Fernández Gallardo
December 2017

-   [Objectives](#objectives)
-   [2-way repeated measures ANOVA](#way-repeated-measures-anova)
    -   [Interaction plots](#interaction-plots)
    -   [Voice Descriptions](#voice-descriptions)
        -   [VD: Male speakers](#vd-male-speakers)
        -   [VD: Female speakers](#vd-female-speakers)
    -   [Discussion](#discussion)

``` r
# Libraries needed:

library(ggplot2) # for plots
library(ez) # for ezANOVA
library(data.table) # for dt significance
library(knitr) # for kable
library(xtable) # for table for LaTeX
```

Objectives
----------

The goal of this analysis is to evaluate the differences between the narrowband and wideband effects on the human impressions of voices, related to perceived signal quality, which might also assist decisions in the communication channel design process.

To examine the influence of both, channel bandwidth and speakers' WAAT, and their interaction effects, two-way repeated measures ANOVA significance tests have been conducted for each of the voice description items, and separately for each speaker gender.

This study is similar to what was done for speaker characteristics in:

Fernández Gallardo, L., "Effects of Transmitted Speech Bandwidth on Subjective Assessments of Speaker Characteristics," Int. Conf. on Quality of Multimedia Experience (QoMEX), 2018.

2-way repeated measures ANOVA
=============================

-   For each speaker gender:
-   For each item of voice descriptions (VD):
-   We perform 2-way repeated measures, to test the effects of
    -   channel bandwidth (bw)
    -   speakers' WAAT class (class)
    -   interaction (bw:class)

We use the package ez to compute ezANOVA: Bakeman, R. (2005). Recommended effect size statistics for repeated measures designs. Behavior Research Methods, 37 (3), 379-384.

``` r
# input: ratings (VD or SC) corresponding to the same speaker gender
#    and item names 
#    p-value of the significance effects required
# output: table with booleans for each item with effects found

# example input:
# data <- mydata_m
# itemnames <- items.VD
# pvalue <- 0.01

perform.ezANOVA <- function(data, itemnames, pvalue){
  
 
all_effects <- NULL

# (workaround to assign dv dinamically)

for (x in itemnames){
  eval(parse(text=
               paste0('ezANOVA2.output <- ezANOVA(data=data,
                      dv=', x,',
                      wid=nameListener,
                      within=.(bw, class),
                      type = 2)')
  ))

  all_effects <- rbind(all_effects,data.frame(x,rbind(ezANOVA2.output$ANOVA$p)))

}

names(all_effects)<-c("item","bw", "class","bw:class")

# assign unique group of effects to each item 

signif <- data.frame(item=all_effects$item, all_effects[2:4] < pvalue)

dt <- as.data.table(signif[2:4])[, list(list(.I)), by = signif[2:4]]

signif$group<-NA
for (i in c(1:length(dt$V1))){
  
  indexes <- dt$V1[[i]]
  signif[indexes,]$group <- i
  
}

# sort dataframe by unique effects
signif <- signif[order(signif$group),]

return(signif)


}
```

Interaction plots
-----------------

For each speaker gender, create interaction plot:

-   view effects of:
    -   speech bandwidth (bw)
    -   speakers' WAAT class (WAAT)
    -   interaction (bw:WAAT)
-   if lines are parallel: there is no interaction effect

``` r
# input d: data corresponding to one speaker gender: mydata_m or mydata_f
create.interaction_plots <-  function(d){
    
  for (item in items.VD){  
    
    mydata_agg <- aggregate(d[item], by = list(d$bw, d$class), mean)
    names(mydata_agg)[1]<-'bw'
    names(mydata_agg)[2]<-'class' 
    names(mydata_agg)[3]<-'mean' 
    
    mplot <- ggplot(d, aes(x = factor(class), y = d[item], fill = bw)) + 
      geom_violin(adjust = .5, draw_quantiles = c(0.5)) + 
      geom_point(data = mydata_agg, aes_string(y = "mean", group = "bw", color = "bw")) + 
      geom_line(data = mydata_agg, aes_string(y = "mean", group = "bw", color = "bw")) + 
      theme_bw() +
      xlab("speaker WAAT class") +
      ylab(item) +  
      scale_y_continuous(limits = c(0, 100)) +
      theme(legend.position = "bottom")
    
    print(mplot)
  }
}
```

Voice Descriptions
------------------

Analyze ANOVA significant effects for male and for female speech

``` r
# get item names
items.VD <- names(mydata)[9:(9+33)]

# split by gender
mydata_split = split(mydata, mydata$spk_gender)
mydata_m <- mydata_split$m # male speakers
mydata_f <- mydata_split$w # female speakers
```

Exploring mean ratings for each gender, for each class, for each bandwidth. "G.711" is a narrowband codec, while "G.722" is a wideband codec.

``` r
mydata.agg <- aggregate(mydata[,c(9:(9+33))], by=list(mydata$bw, mydata$spk_gender, mydata$class), mean)

kable(t(mydata.agg))
```

|                 |          |          |          |          |          |          |          |          |
|:----------------|:---------|:---------|:---------|:---------|:---------|:---------|:---------|:---------|
| Group.1         | G.711    | G.722    | G.711    | G.722    | G.711    | G.722    | G.711    | G.722    |
| Group.2         | m        | m        | w        | w        | m        | m        | w        | w        |
| Group.3         | high     | high     | high     | high     | low      | low      | low      | low      |
| klanglos        | 40.31304 | 24.27826 | 38.19130 | 26.60870 | 67.60870 | 61.79130 | 59.96522 | 53.28696 |
| hoch            | 45.28696 | 39.33913 | 67.57391 | 66.38261 | 39.02609 | 33.81739 | 44.06957 | 44.63478 |
| nicht.nasal     | 54.80000 | 69.06087 | 62.54783 | 71.24348 | 41.47826 | 48.43478 | 46.53913 | 55.29565 |
| scharf          | 48.34783 | 51.45217 | 54.39130 | 57.15652 | 31.44348 | 29.83478 | 34.97391 | 37.91304 |
| ungleichmäßig   | 40.82609 | 30.13913 | 40.39130 | 30.91304 | 43.73913 | 39.77391 | 45.74783 | 40.20870 |
| mit.Akzent      | 14.21739 | 13.73043 | 14.26957 | 14.26957 | 27.02609 | 29.64348 | 20.64348 | 26.85217 |
| hell            | 54.53913 | 48.80000 | 66.46957 | 68.13913 | 40.78261 | 36.51304 | 42.95652 | 43.26087 |
| laut            | 61.46087 | 62.93913 | 59.04348 | 60.20000 | 42.55652 | 45.89565 | 52.83478 | 50.57391 |
| nicht.knarrend  | 54.37391 | 72.76522 | 52.87826 | 71.39130 | 48.98261 | 60.23478 | 48.53043 | 63.55652 |
| monoton         | 38.86087 | 31.75652 | 32.91304 | 27.95652 | 67.66957 | 67.59130 | 57.20870 | 58.81739 |
| unangenehm      | 42.36522 | 28.85217 | 41.32174 | 25.30435 | 64.66957 | 60.01739 | 59.47826 | 56.21739 |
| undeutlich      | 28.13913 | 19.01739 | 28.73913 | 19.33043 | 49.96522 | 41.69565 | 38.95652 | 35.86957 |
| glatt           | 54.51304 | 68.65217 | 59.01739 | 72.33913 | 43.09565 | 51.43478 | 46.01739 | 55.00870 |
| heiser          | 37.17391 | 24.05217 | 32.19130 | 21.50435 | 51.99130 | 44.66957 | 46.60000 | 39.45217 |
| auffällig       | 47.71304 | 49.89565 | 51.34783 | 51.42609 | 45.33913 | 45.09565 | 46.54783 | 44.91304 |
| langsam         | 36.64348 | 37.13043 | 42.46087 | 42.33043 | 59.87826 | 59.81739 | 54.44348 | 54.67826 |
| warm            | 48.97391 | 62.96522 | 56.00000 | 64.34783 | 43.86087 | 51.97391 | 49.12174 | 53.56522 |
| natürlich       | 63.57391 | 73.13043 | 64.60870 | 73.33043 | 55.94783 | 61.92174 | 54.87826 | 61.03478 |
| zittrig         | 34.98261 | 26.44348 | 36.24348 | 24.80000 | 50.95652 | 42.35652 | 43.13913 | 38.71304 |
| präzise         | 64.29565 | 72.48696 | 67.42609 | 73.73913 | 46.69565 | 50.06957 | 53.30435 | 54.56522 |
| fest            | 57.46957 | 72.95652 | 59.08696 | 71.63478 | 40.32174 | 48.32174 | 48.43478 | 57.42609 |
| melodisch       | 55.54783 | 67.11304 | 61.84348 | 73.62609 | 32.88696 | 36.17391 | 42.98261 | 45.65217 |
| entspannt       | 57.57391 | 66.80870 | 59.85217 | 69.19130 | 42.71304 | 50.09565 | 47.66087 | 53.80000 |
| gleitend        | 55.08696 | 66.66087 | 58.96522 | 71.17391 | 41.98261 | 50.13043 | 49.33043 | 54.69565 |
| kurz            | 52.67826 | 53.79130 | 54.12174 | 53.93043 | 45.30435 | 43.08696 | 46.29565 | 47.40000 |
| gepresst        | 39.22609 | 26.93913 | 37.28696 | 25.36522 | 57.60870 | 50.83478 | 50.73043 | 44.06957 |
| kraftlos        | 40.35652 | 29.20870 | 39.27826 | 30.38261 | 64.80000 | 63.89565 | 56.60000 | 56.20870 |
| stockend        | 36.32174 | 26.45217 | 35.11304 | 23.45217 | 55.06087 | 48.81739 | 46.71304 | 43.00870 |
| hart            | 44.27826 | 35.27826 | 40.89565 | 27.94783 | 47.43478 | 42.29565 | 49.18261 | 39.86957 |
| unprofessionell | 43.23478 | 32.27826 | 43.73913 | 29.45217 | 67.40870 | 62.63478 | 61.25217 | 57.95652 |
| unbetont        | 36.54783 | 28.88696 | 30.96522 | 25.06087 | 60.27826 | 61.93913 | 52.36522 | 49.85217 |
| schrill         | 44.66087 | 34.98261 | 44.62609 | 30.33043 | 44.65217 | 40.16522 | 47.35652 | 38.49565 |
| verbunden       | 57.24348 | 64.11304 | 56.59130 | 66.85217 | 43.26957 | 51.50435 | 48.93913 | 50.26087 |
| behaucht        | 31.93913 | 32.65217 | 35.02609 | 31.57391 | 41.40000 | 43.64348 | 39.50435 | 42.80870 |

Obtain the significant effects by calling our function. Generate tables for LaTeX.

``` r
# male speakers
effects.VD_m <- perform.ezANOVA(mydata_m, items.VD, 0.01)

# female speakers
effects.VD_f <- perform.ezANOVA(mydata_f, items.VD, 0.01)

# generate tables for latex
# load translations
ques.VD <- read.csv(paste0("../../../data/subjective_ratings/VD_Questionnaire.csv"), header=T)
names(ques.VD)[3]<-'item'
effects.VD_m <-merge(effects.VD_m,ques.VD[,c(3,5)])
effects.VD_f <-merge(effects.VD_f,ques.VD[,c(3,5)])

# sort English items alphabetically
effects.VD_m <- effects.VD_m[order(effects.VD_m$right_English),]
effects.VD_f <- effects.VD_f[order(effects.VD_f$right_English),]

# display table (3 first columns correspond to male speakers and the rest to female speakers)
mftable <- cbind(effects.VD_m[,c(6,2,3,4)],effects.VD_f[,c(2,3,4)])
kable(mftable)
```

|     | right\_English  | bw    | class | bw.class | bw    | class | bw.class |
|-----|:----------------|:------|:------|:---------|:------|:------|:---------|
| 1   | breathy         | FALSE | TRUE  | FALSE    | FALSE | FALSE | FALSE    |
| 9   | bright          | FALSE | TRUE  | FALSE    | FALSE | TRUE  | FALSE    |
| 3   | firm            | TRUE  | TRUE  | TRUE     | TRUE  | TRUE  | FALSE    |
| 11  | flat            | TRUE  | TRUE  | FALSE    | TRUE  | TRUE  | FALSE    |
| 20  | halting         | TRUE  | TRUE  | FALSE    | TRUE  | TRUE  | FALSE    |
| 7   | hard            | TRUE  | FALSE | FALSE    | TRUE  | TRUE  | FALSE    |
| 10  | high            | FALSE | FALSE | FALSE    | FALSE | TRUE  | FALSE    |
| 8   | hoarse          | TRUE  | TRUE  | FALSE    | TRUE  | TRUE  | FALSE    |
| 23  | inarticulate    | TRUE  | TRUE  | FALSE    | TRUE  | TRUE  | FALSE    |
| 25  | jointed         | TRUE  | TRUE  | FALSE    | FALSE | TRUE  | TRUE     |
| 15  | loud            | FALSE | TRUE  | FALSE    | FALSE | TRUE  | FALSE    |
| 16  | melodious       | TRUE  | TRUE  | FALSE    | FALSE | TRUE  | TRUE     |
| 17  | monotonous      | FALSE | TRUE  | FALSE    | FALSE | TRUE  | FALSE    |
| 5   | not\_coarse     | TRUE  | TRUE  | FALSE    | TRUE  | TRUE  | FALSE    |
| 22  | not\_emphasized | TRUE  | TRUE  | TRUE     | FALSE | TRUE  | FALSE    |
| 12  | powerless       | TRUE  | TRUE  | TRUE     | FALSE | TRUE  | FALSE    |
| 4   | pressed         | TRUE  | TRUE  | FALSE    | TRUE  | TRUE  | FALSE    |
| 2   | relaxed         | TRUE  | TRUE  | FALSE    | TRUE  | TRUE  | FALSE    |
| 27  | shaky           | TRUE  | TRUE  | FALSE    | FALSE | TRUE  | FALSE    |
| 18  | sharp           | FALSE | TRUE  | FALSE    | FALSE | TRUE  | FALSE    |
| 13  | short           | FALSE | TRUE  | FALSE    | FALSE | FALSE | FALSE    |
| 19  | shrill          | TRUE  | FALSE | FALSE    | TRUE  | FALSE | FALSE    |
| 14  | slow            | FALSE | TRUE  | FALSE    | FALSE | TRUE  | FALSE    |
| 6   | smooth          | TRUE  | TRUE  | FALSE    | TRUE  | TRUE  | FALSE    |
| 21  | unpleasant      | TRUE  | TRUE  | FALSE    | TRUE  | TRUE  | TRUE     |
| 24  | unprofessional  | TRUE  | TRUE  | FALSE    | TRUE  | TRUE  | TRUE     |
| 26  | warm            | TRUE  | FALSE | FALSE    | TRUE  | TRUE  | FALSE    |

``` r
#print(xtable(mftable),include.rownames=FALSE)
```

### VD: Male speakers

View items, only when significant effect found for bw or for bw:class

``` r
# effects.VD_m <- effects.VD_m[,-ncol(effects.VD_m)]

effects.VD_m_BW <- effects.VD_m[which(effects.VD_m$bw==T | effects.VD_m$bw.class==T), ]
effects.VD_m_BW <- effects.VD_m[which(effects.VD_m$bw==T | effects.VD_m$bw.class==T), ]

kable(effects.VD_m_BW, row.names = FALSE)
```

| item            | bw   | class | bw.class |  group| right\_English  |
|:----------------|:-----|:------|:---------|------:|:----------------|
| fest            | TRUE | TRUE  | TRUE     |      5| firm            |
| klanglos        | TRUE | TRUE  | FALSE    |      1| flat            |
| stockend        | TRUE | TRUE  | FALSE    |      1| halting         |
| hart            | TRUE | FALSE | FALSE    |      4| hard            |
| heiser          | TRUE | TRUE  | FALSE    |      1| hoarse          |
| undeutlich      | TRUE | TRUE  | FALSE    |      1| inarticulate    |
| verbunden       | TRUE | TRUE  | FALSE    |      1| jointed         |
| melodisch       | TRUE | TRUE  | FALSE    |      1| melodious       |
| glatt           | TRUE | TRUE  | FALSE    |      1| not\_coarse     |
| unbetont        | TRUE | TRUE  | TRUE     |      5| not\_emphasized |
| kraftlos        | TRUE | TRUE  | TRUE     |      5| powerless       |
| gepresst        | TRUE | TRUE  | FALSE    |      1| pressed         |
| entspannt       | TRUE | TRUE  | FALSE    |      1| relaxed         |
| zittrig         | TRUE | TRUE  | FALSE    |      1| shaky           |
| schrill         | TRUE | FALSE | FALSE    |      4| shrill          |
| gleitend        | TRUE | TRUE  | FALSE    |      1| smooth          |
| unangenehm      | TRUE | TRUE  | FALSE    |      1| unpleasant      |
| unprofessionell | TRUE | TRUE  | FALSE    |      1| unprofessional  |
| warm            | TRUE | FALSE | FALSE    |      4| warm            |

Interaction plots for male speakers.

``` r
create.interaction_plots( mydata_m )
```

![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-1.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-2.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-3.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-4.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-5.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-6.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-7.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-8.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-9.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-10.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-11.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-12.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-13.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-14.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-15.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-16.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-17.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-18.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-19.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-20.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-21.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-22.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-23.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-24.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-25.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-26.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-27.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-28.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-29.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-30.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-31.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-32.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-33.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-10-34.png)

### VD: Female speakers

View items, only when significant effect found for bw or for bw:class

``` r
# effects.VD_f <- effects.VD_f[,-ncol(effects.VD_f)]

effects.VD_f_BW <- effects.VD_f[which(effects.VD_f$bw==T | effects.VD_f$bw.class==T), ]
effects.VD_f_BW <- effects.VD_f[which(effects.VD_f$bw==T | effects.VD_f$bw.class==T), ]

kable(effects.VD_f_BW, row.names = FALSE)
```

| item            | bw    | class | bw.class |  group| right\_English |
|:----------------|:------|:------|:---------|------:|:---------------|
| fest            | TRUE  | TRUE  | FALSE    |      1| firm           |
| klanglos        | TRUE  | TRUE  | FALSE    |      1| flat           |
| stockend        | TRUE  | TRUE  | FALSE    |      1| halting        |
| hart            | TRUE  | TRUE  | FALSE    |      1| hard           |
| heiser          | TRUE  | TRUE  | FALSE    |      1| hoarse         |
| undeutlich      | TRUE  | TRUE  | FALSE    |      1| inarticulate   |
| verbunden       | FALSE | TRUE  | TRUE     |      6| jointed        |
| melodisch       | FALSE | TRUE  | TRUE     |      6| melodious      |
| glatt           | TRUE  | TRUE  | FALSE    |      1| not\_coarse    |
| gepresst        | TRUE  | TRUE  | FALSE    |      1| pressed        |
| entspannt       | TRUE  | TRUE  | FALSE    |      1| relaxed        |
| schrill         | TRUE  | FALSE | FALSE    |      3| shrill         |
| gleitend        | TRUE  | TRUE  | FALSE    |      1| smooth         |
| unangenehm      | TRUE  | TRUE  | TRUE     |      4| unpleasant     |
| unprofessionell | TRUE  | TRUE  | TRUE     |      4| unprofessional |
| warm            | TRUE  | TRUE  | FALSE    |      1| warm           |

Interaction plots for female speakers.

``` r
create.interaction_plots( mydata_m )
```

![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-1.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-2.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-3.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-4.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-5.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-6.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-7.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-8.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-9.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-10.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-11.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-12.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-13.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-14.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-15.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-16.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-17.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-18.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-19.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-20.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-21.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-22.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-23.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-24.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-25.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-26.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-27.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-28.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-29.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-30.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-31.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-32.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-33.png)![](effects_ANOVA_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-12-34.png)

Discussion
----------

Significant effects (p&lt;0.01) by conducting two-way repeated measures ANOVA:

Male speakers:

-   Effects of bandwidth in isolation (*bw* regardless of WAAT): if the voices are transmitted through wideband instead of narrowband they are perceived as significantly warmer, softer, and gentler.

-   Effects of bandwidth in isolation (*bw* regardless of WAAT) and of WAAT in isolation (*WAAT* regardless of bandwidth). The following voice aspects are perceived higher if speakers have high WAAT and/or when the speech is transmitted through wideband instead of narrowband: relaxed, smooth, jointed, not coarse, melodious, lax, fluent, professional, pleasant, articulate, clear, sonorous, and not creaky.

-   Interaction effect (*bw:WAAT*): When speakers have high WAAT, their voices are perceived as more powerful, emphasized, and firm in wideband compared to narrowband. Otherwise, if speakers have low WAAT, their voices are perceived powerless, not emphasized and brittle.

-   No effects of bandwidth in isolation or interacting with WAAT on: short, breathy, high, nasal, sharp, even, accented, bright, loud, creaky, monotonous, remarkable, slow, natural, precise.

Female speakers:

-   Effects of bandwidth in isolation (*bw* regardless of WAAT): if the voices are transmitted through wideband instead of narrowband they are perceived as significantly shriller.

-   Effects of bandwidth in isolation (*bw* regardless of WAAT) and of WAAT in isolation (*WAAT* regardless of bandwidth). The following voice aspects are perceived higher if speakers have high WAAT and/or when the speech is transmitted through wideband instead of narrowband: not coarse, warm, firm, relaxed, smooth, sonorous, articulate, clear, lax, fluent, soft.

-   Interaction effect (*bw:WAAT*), *bw* effect regardless of WAAT, and *WAAT* effect regardless of bandwidth: if wideband and if high WAAT speakers, voices are perceived as more pleasant and professional. The difference in perception between narrowband and wideband is larger if speakers have high WAAT compared to speakers of low WAAT.

-   Interaction effect (*bw:WAAT*) and *WAAT* effect regardless of bandwidth: When speakers have high WAAT, their voices are perceived as more melodious in wideband compared to narrowband. Otherwise, if speakers have low WAAT, their voices are perceived less melodious with no difference between wideband and narrowband.

-   No effects of bandwidth in isolation or interacting with WAAT on: short, breathy, high, nasal, sharp, even, accented, bright, loud, creaky, monotonous, remarkable, slow, natural, shaky, precise, powerful, emphasized.

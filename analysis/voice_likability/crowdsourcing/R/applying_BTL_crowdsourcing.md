Applying BTL model to paired-comparison crowdsourcing data
================
Laura Fernández Gallardo
November 2016

-   [Objectives](#objectives)
-   [Preference matrix from crowdsourcing (Listening Test 8)](#preference-matrix-from-crowdsourcing-listening-test-8)
    -   [Consistency checks](#consistency-checks)
    -   [Scaling listeners' preferences](#scaling-listeners-preferences)
-   [Preference matrix from laboratory (Listening Test 7)](#preference-matrix-from-laboratory-listening-test-7)
-   [Comparison of likability scores between Crowdee and lab](#comparison-of-likability-scores-between-crowdee-and-lab)

Clear and set path.

``` r
# clear
rm(list=ls())

path_github <- "https://raw.githubusercontent.com/laufergall/Subjective_Speaker_Characteristics/master/data/subjective_ratings/data_listeningtest8"
```

``` r
# Libraries needed:

library(RCurl) # to read raw data from repo
library(eba) # BTL model
library(Hmisc) # for correlations
```

Objectives
----------

With data from a paired-comparison listening test conducted via crowdsourcing (Listening Test 8):

-   Apply BTL model to obtain ratio-scaled preferences for voices
-   Compare obtained values to those from Listening Test 7

Preference matrix from crowdsourcing (Listening Test 8)
-------------------------------------------------------

Preference matrix built from only accepted and automatically\_accepted answers in crowdsroucing. See Matlab scripts. Row and column names are speaker pseudonyms.

``` r
matrix_crowdee <- read.csv(text=getURL(paste0(path_github,"/preferencematrix_onlyaccepted.csv")), header=TRUE, sep=",")

rownames(matrix_crowdee) <- colnames(matrix_crowdee)
```

### Consistency checks

Following the same steps as in the paired-comparison [data analysis](https://github.com/laufergall/Subjective_Speaker_Characteristics/tree/master/analysis/voice_likability/paired_comparison/R) (Listening Test 7).

Concordance among listeners' preferences not applied to crowdsrouccing data.

Transitivity violations:

``` r
strans(matrix_crowdee)
```

    ## 
    ## Stochastic Transitivity
    ## 
    ##          Violations ErrorRatio MeanDev MaxDev Deviance Df Pr(>Chi)
    ## Weak             28     0.0615  0.0549  0.192     23.4 14   0.0537
    ## Moderate         65     0.1429  0.1144  0.385       NA NA       NA
    ## Strong          220     0.4835  0.1520  0.615       NA NA       NA
    ## ---
    ## Number of Tests: 455

### Scaling listeners' preferences

Since the BTL model has been shown to hold, utility scale (upsilon-scale) values were assigned to each speaker, representing their voice likability.

``` r
btl1_crowdee <- eba(matrix_crowdee) # fit Bradley-Terry-Luce model
btl1_crowdee
```

    ## 
    ## Elimination by aspects (EBA) models
    ## 
    ## Parameter estimates:
    ##       1        2        3        4        5        6        7        8  
    ## 0.11726  0.07821  0.03332  0.04574  0.05226  0.15674  0.04783  0.01538  
    ##       9       10       11       12       13       14       15  
    ## 0.04474  0.03737  0.14053  0.02826  0.05169  0.05711  0.05169  
    ## 
    ## Goodness of fit (-2 log likelihood ratio):
    ##  G2(91) = 88.31, p = 0.5605

``` r
#   G2(91) = 88.31, p = 0.5605 it fits
```

Deriving utility scale values.

``` r
# utility scale
usr_crowdee<-uscale(btl1_crowdee) #### no norm

# ordering from most liked to least liked
iir_crowdee<-order(usr_crowdee)
```

Preference matrix from laboratory (Listening Test 7)
----------------------------------------------------

``` r
path_github2 <- "https://raw.githubusercontent.com/laufergall/Subjective_Speaker_Characteristics/master/data/subjective_ratings/data_listeningtest7"
  
matrix_lab <- read.csv(text=getURL(paste0(path_github2,"/preferencematrix_total.csv")), header=TRUE, sep=",")

rownames(matrix_lab) <- colnames(matrix_lab)

# Fit BTL
btl1_lab <- eba(matrix_lab) # fit Bradley-Terry-Luce model
btl1_lab
```

    ## 
    ## Elimination by aspects (EBA) models
    ## 
    ## Parameter estimates:
    ##       1        2        3        4        5        6        7        8  
    ## 0.07771  0.07034  0.01968  0.05792  0.05521  0.15104  0.04904  0.01093  
    ##       9       10       11       12       13       14       15  
    ## 0.02405  0.02920  0.15588  0.01092  0.03062  0.05391  0.02465  
    ## 
    ## Goodness of fit (-2 log likelihood ratio):
    ##  G2(91) = 66.08, p = 0.9772

``` r
# G2(91) = 66.08, p = 0.9772 it fits

# utility scale
usr_lab<-uscale(btl1_lab) #### no norm

# ordering from most liked to least liked
iir_lab<-order(usr_lab)
```

Comparison of likability scores between Crowdee and lab
-------------------------------------------------------

``` r
cbind(usr_crowdee,usr_lab)
```

    ##                         usr_crowdee    usr_lab
    ## linden                   0.12238485 0.09464458
    ## nicosia                  0.08162310 0.08566530
    ## rabat                    0.03478013 0.02396865
    ## klaksvik                 0.04774320 0.07053318
    ## beirut                   0.05454829 0.06724153
    ## rotterdam                0.16358601 0.18394959
    ## banjul                   0.04991558 0.05972345
    ## edinburghofthesevenseas  0.01605162 0.01330530
    ## dakhla                   0.04669132 0.02929542
    ## marseille                0.03900426 0.03555758
    ## malabo                   0.14667461 0.18983382
    ## kigali                   0.02949438 0.01330492
    ## westbay                  0.05394686 0.03729441
    ## debrecen                 0.05960893 0.06565947
    ## sanaa                    0.05394686 0.03002279

``` r
rp <- cor.test(usr_crowdee,usr_lab)
rp
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  usr_crowdee and usr_lab
    ## t = 10.563, df = 13, p-value = 9.491e-08
    ## alternative hypothesis: true correlation is not equal to 0
    ## 95 percent confidence interval:
    ##  0.8426391 0.9823899
    ## sample estimates:
    ##       cor 
    ## 0.9463901

``` r
# standard error
stderror_p <- sqrt((1-rp$estimate*rp$estimate)/(15-2)) 
stderror_p
```

    ##        cor 
    ## 0.08959127

``` r
# Pearson's correlation: r = 0.95, Standard error = 0.09




# Spearman's correlation:

rs <- rcorr(cbind(usr_crowdee,usr_lab), type = "spearman")
rs
```

    ##             usr_crowdee usr_lab
    ## usr_crowdee        1.00    0.91
    ## usr_lab            0.91    1.00
    ## 
    ## n= 15 
    ## 
    ## 
    ## P
    ##             usr_crowdee usr_lab
    ## usr_crowdee              0     
    ## usr_lab      0

``` r
stderror_s <- sqrt((1-rs$r[1,2]*rs$r[1,2])/(15-2)) 
stderror_s
```

    ## [1] 0.1167152

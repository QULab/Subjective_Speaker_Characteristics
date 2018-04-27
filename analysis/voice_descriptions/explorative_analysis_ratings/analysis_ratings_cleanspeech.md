Explorative analisys of voice descriptions (clean speech)
================
Laura Fernández Gallardo
September 2017

-   [Objectives](#objectives)
-   [Load subjective ratings](#load-subjective-ratings)
-   [Generate plots](#generate-plots)

``` r
# Libraries needed:

library(RCurl) # to read raw data from repo
library(ggplot2) # for plots
```

Objectives
----------

Let us explore the data from Listening Test 2: 26 participants rated voice descriptions of the 20 "extreme" speakers, with high and low warmth-attractiveness (WAAT). More details [here](https://github.com/laufergall/Subjective_Speaker_Characteristics/tree/master/doc/listening_tests).

Load subjective ratings
-----------------------

Set paths and read data.

Generate plots
--------------

``` r
# function in utils_R
source('../../utils_R/summarySE.R')

# items from the questionnaire (German)

nameitems = c("Item 1 klangvoll - klanglos",
              "Item 2 tief - hoch",
              "Item 3 nasal - nicht nasal",
              "Item 4 stumpf - scharf",
              "Item 5 gleichmäßig - ungleichmäßig",
              "Item 6 akzentfrei - mit Akzent",
              "Item 7 dunkel - hell",
              "Item 8 leise - laut",
              "Item 9 knarrend - nicht knarrend",
              "Item 10 variabel - monoton",
              "Item 11 angenehm - unangenehm",
              "Item 12 deutlich - undeutlich",
              "Item 13 rau - glatt",
              "Item 14 klar - heiser",
              "Item 15 unauffällig - auffällig",
              "Item 16 schnell - langsam",
              "Item 17 kalt - warm",
              "Item 18 unnatürlich - natürlich",
              "Item 19 stabil - zittrig",
              "Item 20 unpräzise - präzise",
              "Item 21 brüchig - fest",
              "Item 22 unmelodisch - melodisch",
              "Item 23 angespannt - entspannt",
              "Item 24 holprig - gleitend",
              "Item 25 lang - kurz",
              "Item 26 locker - gepresst",
              "Item 27 kraftvoll - kraftlos",
              "Item 28 flüssig - stockend",
              "Item 29 weich - hart",
              "Item 30 professionell - unprofessionell",
              "Item 31 betont - unbetont",
              "Item 32 sanft - schrill",
              "Item 33 getrennt - verbunden",
              "Item 34 nicht behaucht - behaucht"
)
```

Plots of voice description items of low and high warm-attractive speakers.

For each scale we grate a plot: for each speaker, we get mean sd, se, and ci of the mean.

``` r
for (i in 1:34){ ## for each scale
  
resumen <- summarySE(data_raw , measurevar=names(data_raw)[15+i], groupvars = 'speaker_pseudonym')

# sort by speaker pseudonym, WAAT, and gender
resumen <- merge(resumen, data_raw[,c(7,8,15)], by = 'speaker_pseudonym')

resumen <- resumen[order(resumen$speaker_pseudonym),]
resumen <- resumen[order(resumen$speaker_highlow_SC),]
resumen <- resumen[order(resumen$speaker_gender),]

# "fix" the items in this ordering for the ggplot
resumen$speaker_pseudonym <- factor(resumen$speaker_pseudonym, 
                                    levels=unique(as.character(resumen$speaker_pseudonym)) )

# create plot
myplot <- ggplot(resumen, aes(resumen[3], speaker_pseudonym, colour = speaker_highlow_SC, shape = speaker_gender)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmax = resumen[3] + resumen[6], xmin = resumen[3] - resumen[6])) +      
  ylab("speaker pseudonym") +
  xlab(nameitems[i]) +
  scale_x_continuous(limits = c(0, 101), breaks = seq(0, 100, length.out = 11)) +
  theme_bw() +
  theme(legend.position = "bottom")      

# # save plot
# png(paste0(path_Figures,"/vd_items/",nameitems[i],".png"), family="Times")#, width=20, height=15) 
# print(myplot)
# dev.off()

print(myplot)

}
```

![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-1.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-2.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-3.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-4.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-5.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-6.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-7.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-8.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-9.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-10.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-11.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-12.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-13.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-14.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-15.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-16.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-17.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-18.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-19.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-20.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-21.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-22.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-23.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-24.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-25.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-26.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-27.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-28.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-29.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-30.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-31.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-32.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-33.png)![](analysis_ratings_cleanspeech_files/figure-markdown_github-ascii_identifiers/unnamed-chunk-5-34.png)

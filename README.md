## This repository

As part of my [Postdoc project](http://www.qu.tu-berlin.de/?id=lfernandez), I collected subjective ratings of speaker and voice characteristics. In this repository, I explore data resulting from listening tests with speech stimuli from the [Nautilus Speaker Characterization (NSC) Corpus](http://www.qu.tu-berlin.de/?id=nsc-corpus) [3]:

* Factor analysis to find traits of social speaker characteristics (SC), with data from Listening Test 1.

  (Folders: \analysis\speaker_characteristics\explorative_analysis_ratings, \analysis\speaker_characteristics\factor_analysis)

​    The resulting dimensions (referred to as "traits") were: _warmth_, _attractiveness_, _confidence_, _compliance_, _maturity_. 

​    "WAAT" stands for "warmth-attractiveness", the first two traits resulting from this factor analysis.

* Factor analysis to find dimensions of voice descriptions (VD), with data from Listening Test 2.

  (Folders: \analysis\voice_descriptions\explorative_analysis_ratings, \analysis\voice_descriptions\factor_analysis)

* Statistical data analysis of effects of  [telephone degradations](https://github.com/laufergall/ML_Speaker_Characteristics/tree/master/data/distortions) (channel bandwidth) on speaker characteristics, with data from Listening Test 3.

  (Folder: \analysis\speaker_characteristics\effects_telephone_degradations)

  (Publication: [1])

* Statistical data analysis of effects of  [telephone degradations](https://github.com/laufergall/ML_Speaker_Characteristics/tree/master/data/distortions) (channel bandwidth) on voice descriptions, with data from Listening Test 4.

  (Folder: \analysis\voice_descriptions\effects_telephone_degradations)

* Analyzing the relationship between speech quality and speakers' WAAT, with data from Listening Test 5.

  (Folder: \analysis\relationships_quality_WAAT)

  (Publication: [2])

I also employed these subjective data to perform predictive modeling of speaker characteristics from speech features (see [this](https://github.com/laufergall/ML_Speaker_Characteristics) repository).



## References

[1] Fernández Gallardo, L., "Effects of Transmitted Speech Bandwidth on Subjective Assessments of Speaker Characteristics," Int. Conf. on Quality of Multimedia Experience (QoMEX), 2018.

[2] Fernández Gallardo, L., Mittag, G., Möller, S. and Beerends, J., "Variable Voice Likability Affecting Subjective Speech Quality Assessments," Int. Conf. on Quality of Multimedia Experience (QoMEX), 2018.

[3] Fernández Gallardo, L. and Weiss, B., "The Nautilus Speaker Characterization Corpus: Speech Recordings and Labels of Speaker Characteristics and Voice Descriptions," in International Conference on Language Resources and Evaluation (LREC), 2018.

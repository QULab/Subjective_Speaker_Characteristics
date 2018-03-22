## This repository

As part of my [Postdoc project](http://www.qu.tu-berlin.de/?id=lfernandez), I collected subjective ratings of speaker and voice characteristics. In this repository, I explore data resulting from listening tests with speech stimuli from the [Nautilus Speaker Characterization (NSC) Corpus](http://www.qu.tu-berlin.de/?id=nsc-corpus):

* Factor analysis to find traits of social speaker characteristics (SC), with data from Listening Test 1.

​    The resulting dimensions (referred to as "traits") were: _warmth_, _attractiveness_, _confidence_, _compliance_, _maturity_. 

​    "WAAT" stands for "warmth-attractiveness", the first two traits resulting from this factor analysis.

* Factor analysis to find dimensions of voice descriptions (VD), with data from Listening Test 2.
* Statistical data analysis of effects of  [telephone degradations](https://github.com/laufergall/ML_Speaker_Characteristics/tree/master/data/distortions) (channel bandwidth) on speaker characteristics, with data from Listening Test 3.
* Statistical data analysis of effects of  [telephone degradations](https://github.com/laufergall/ML_Speaker_Characteristics/tree/master/data/distortions) (channel bandwidth) on voice descriptions, with data from Listening Test 4.
* Analyzing the relationship between speech quality and speakers' WAAT, with data from Listening Test 5.

I also employed these subjective data to perform predictive modeling of speaker characteristics and of the derived speaker traits (see [this](https://github.com/laufergall/ML_Speaker_Characteristics) repository).



## Listening tests

The raw data from listening tests 1) and 2) and the scores resulting from factor analysis are included as documentation of the [NSC](http://www.qu.tu-berlin.de/?id=nsc-corpus). 

For the listening tests, this [GUI](https://github.com/laufergall/GUI_SpeakerCharacteristics) written in Java (adapted depending on the questionnaire items and audio stimuli) was employed.

Tests administered using a laptop, a mouse, and Shure SRH240 headphones (diotic listening, frequency range 20-20,000 Hz), except for Listening Test 5, where the AKG K271 MKII (16-28,000 Hz) were employed. Individual test sessions of max. duration of about 1-1.5 hours, with short breaks every 10-15 minutes. No test participant performed more than one test. 



### 1) NSC labeling of speaker characteristics (SC)

This listening test was conducted in order to label the NSC corpus with continuous numeric subjective ratings of speaker characteristics, given to all speakers.

- Speakers: 300 speakers (126 males, 174 females), the entire NSC corpus.
- Speech stimuli:
  - Clean microphone speech
  - Semi-spontaneous "Pizza" dialogs (shortened dialog version)
  - The speech stimuli can be found under the /stimuli folder of the [NSC release](http://www.qu.tu-berlin.de/?id=nsc-corpus).
- Questionnaire: 34 items of interpersonal speaker characteristics (German) (see below)
- Test participants: 114 (70 males and 44 females)
  - Mean age = 24.5, standard deviation = 3.4
  - 11 languages (93 German and 21 other)
- Each test participant filled in the questionnaire for, on average, 16,4 male speakers and 23.2 female speakers



### 2) NSC labeling of voice descriptions (VD)

This listening test was conducted in order to label the NSC corpus with continuous numeric subjective ratings of speaker characteristics, given to all speakers.

- Speakers: 
  - 20 "extreme" speakers, selected based on their salient scores on WAAT
  - 10 males (5 of low WAAT and 5 of high WAAT), and 10 females (5 of low WAAT and 5 of high WAAT)
  - Speakers' IDs: 
- Speech stimuli:
  - Clean microphone speech
  - Semi-spontaneous "Pizza" dialogs (shortened dialog version)
  - The speech stimuli can be found under the /stimuli folder of the [NSC release](http://www.qu.tu-berlin.de/?id=nsc-corpus).
- Questionnaire: 34 items of naive voice descriptions (German) (see below)
- Test participants: 26 (13 males and 13 females)
  - Mean age = 26.6, standard deviation = 4.8
  - German as mother tongue
- Each test participant filled in the questionnaire for all speech stimuli




### 3) Effects of bandwidth on speaker characteristics

This listening test was conducted with degraded speech stimuli to study the effects of telephone bandwidth on subjective impressions of speaker characteristics. 

* Speakers: same as in Listening test 2
* Speech stimuli:
  * Semi-spontaneous "Pizza" dialogs (shortened dialog version)
  * 2 channel conditions (speech degradations): 
    * Narrowband: IRS8 filter, G.712 NB filter (300-3,400 Hz), G.711 A-law codec at 64 kbit/s 
    * Wideband: modified P.341 filter (135-7000 Hz), G.722 at 64 kbit/s
  * The average length of these files was 22.3 s (sd=2.7 s, range: 18.6-27.3 s).
* Questionnaire: 34 items of interpersonal speaker characteristics (German) (see below)
* Test participants: 23 (10 male and 13 female)
  * Mean age =27.7, standard deviation = 5.9, range = 18-40
  * German as mother tongue
* Each test participant filled in the questionnaire for all speech stimuli



### 4) Effects of bandwidth on voice descriptions

This listening test was conducted with degraded speech stimuli to study the effects of telephone bandwidth on subjective voice descriptions.

- Speakers: same as in Listening test 2 and Listening test 3
- Speech stimuli: same as in Listening test 3
- Questionnaire: 34 items of naive voice descriptions (German) (see below)
- Test participants: 23 (9 male and 14 female)
  - Mean age = 28.6, standard deviation = 6.0, range = 20-40
  - German as mother tongue
- Each test participant filled in the questionnaire for all speech stimuli



### 5) Speech quality of speakers of extreme WAAT

The relationships between speech quality and speakers' WAAT was studied with the data from this listening test. 

* Speakers: 
  - 12 "extreme" speakers, selected based on their salient scores on WAAT
  - 10 males (5 of low WAAT and 5 of high WAAT), and 10 females (5 of low WAAT and 5 of high WAAT)
  - Speakers' IDs: 
    - 3 females of high WAAT: w008_dakar, w125_hamhung, w257_nasinu
    - 3 females of low WAAT: w174_reykjavik, w094_hakupu, w148_phuntsholing
    - 3 males of high WAAT: m061_alrayyan, m041_bucharest, m097_basseterre
    - 3 males of low WAAT: m171_barentsburg, m096_helsinki, m263_andorralavella
  - 11 out of the 12 speakers are the same as the ones considered in Listening Test 2, Listening Test 3, and Listening Test 4.
* Speech stimuli: 
  * 8 Degradations: 
    * Reference (48 kHz) - no degradation
    * (SWB) Enhanced Voice Services (EVS) at 16.4 kbit/s
    * (WB) G.722 at 64 kbit/s
    * (WB) AMR-WB at 6.6 kbit/s
    * (NB) G.711 at 64 kbit/s
    * (NB) AMR-NB at 4.65 kbit/s
    * Bandpass filtering 900-2500 Hz
    * Modulated Noise Reference Unit (MNRU) of 10 dB
  * 8 different scripted excerpts of 2 to 3 sentences each (content) were extracted from the NSC corpus corresponding to the 12 speakers. We then selected 8 different channel conditions (reference and channel degradations). Randomly for each speaker, one of her 8 excerpts was assigned to one condition in a balanced manner, so that the same speaker, the same content, and the same distortion appears the same number of times in the resulting 12 x 8 = 96 stimulus files. 
  * Total: 96 speech stimuli with mean duration = 8.7 s, sd = 1.1 s, min = 5.5 s, max = 12.0 s
* Test: Perceptual quality test with MOS continuous scale from "extremely bad" to "ideal" (see screenshot)
* Test participants: 20 (10 male and 10 female)
  - Mean age =26.4, range = 19-34
  - German as mother tongue
* Each test participant rated all speech stimuli

Thanks to Gabriel Mittag (Technische Universität Berlin, Germany) for the preparation of speech stimuli and support with the test GUI.




## Semantic differential questionnaire items

Items based on previous research by Dr. Benjamin Weiss (Technische Universität Berlin, Germany).

Used for listening tests 1), 2), and 3).

These items can also be seen in:
Fernández Gallardo, L. and Weiss, B., "The Nautilus Speaker Characterization Corpus: Speech Recordings and Labels of Speaker Characteristics and Voice Descriptions," in International Conference on Language Resources and Evaluation (LREC), 2018.

#### German (original)

34 items of interpersonal speaker characteristics:

item left | item right 
--- | ---
sympathisch | unsympathisch
unsicher | sicher
unattraktiv | attraktiv
verstaendnisvoll | verstaendnislos
entschieden | unentschieden
aufdringlich | unaufdringlich
nah | distanziert
interessiert | gelangweilt
emotionslos | emotional
genervt | nicht_genervt
passiv | aktiv
unangenehm | angenehm
charaktervoll | charakterlos
reserviert | gesellig
nervoes | entspannt
distanziert | mitfuehlend
unterwuerfig | dominant
affektiert | unaffektiert
gefuehlskalt | herzlich
jung | alt
sachlich | unsachlich
aufgeregt | ruhig
kompetent | inkompetent
schoen | haesslich
unfreundlich | freundlich
weiblich | maennlich
provokativ | gehorsam
engagiert | gleichgueltig
langweilig | interessant
folgsam | zynisch
unaufgesetzt | aufgesetzt
dumm | intelligent
erwachsen | kindlich
frech | bescheiden



34 items of naive voice descriptions:

item left | item right 
--- | ---
klangvoll | klanglos
tief | hoch
nasal | nicht_nasal
stumpf | scharf
gleichmaessig | ungleichmaessig
akzentfrei | mit_Akzent
dunkel | hell
leise | laut
knarrend | nicht_knarrend
variabel | monoton
angenehm | unangenehm
deutlich | undeutlich
rau | glatt
klar | heiser
unauffaellig | auffaellig
schnell | langsam
kalt | warm
unnatuerlich | natuerlich
stabil | zittrig
unpraezise | praezise
bruechig | fest
unmelodisch | melodisch
angespannt | entspannt
holprig | gleitend
lang | kurz
locker | gepresst
kraftvoll | kraftlos
fluessig | stockend
weich | hart
professionell | unprofessionell
betont | unbetont
sanft | schrill
getrennt | verbunden
nicht_behaucht | behaucht



#### English (translated) 

34 items of interpersonal speaker characteristics:

item left | item right 
--- | ---
likable | non_likable
unsecure | secure
unattractive | attractive
sympathetic | unsympathetic
decided | indecisive
obtrusive | unobtrusive
close | distant
interested | bored
unemotional | emotional
irritated | not_irritated
passive | active
unpleasant | pleasant
characterful | characterless
reserved | sociable
nervous | relaxed
distant | affectionate
conformable | dominant
affected | unaffected
cold | hearty
young | old
impersonal | personal
excited | calm
competent | incompetent
beautiful | ugly
unfriendly | friendly
feminine | masculine
offensive | submissive
committed | indifferent
boring | interesting
compliant | cynical
genuine | artificial
stupid | intelligent
adult | childish
impudent | modest



34 items of naive voice descriptions:

item left | item right 
--- | ---
sonorous | flat
low | high
nasal | not_nasal
blunt | sharp
even | uneven
accented | without_accent
dark | bright
quiet | loud
creaky | not_creaky
variable | monotonous
pleasant | unpleasant
articulate | inarticulate
coarse | not_coarse
clear | hoarse
not_remarkable | remarkable
quick | slow
cold | warm
unnatural | natural
stable | shaky
imprecise | precise
brittle | firm
not_melodious | melodious
tense | relaxed
bumpy | smooth
long | short
lax | pressed
powerful | powerless
fluent | halting
soft | hard
professional | unprofessional
emphasized | not_emphasized
gentle | shrill
disjointed | jointed
not_breathy | breathy
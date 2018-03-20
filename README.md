## This repository

As part of my [Postdoc project](http://www.qu.tu-berlin.de/?id=lfernandez), I collected subjective ratings of speaker and voice characteristics. In this repository, I explore these data:

* Factor analysis to find dimensions of social speaker characteristics
* Factor analysis to find dimensions of voice descriptions
* Statistical data analysis of effects of  [telephone degradations](https://github.com/laufergall/ML_Speaker_Characteristics/tree/master/data/distortions) (channel bandwidth)
  * on speaker characteristics
  * on voice descriptions
* Analyzing the relationship between speech quality and speakers' WAAT

I also employed these subjective data to perform [predictive modeling](https://github.com/laufergall/ML_Speaker_Characteristics) of speaker characteristics.



## Listening tests

The raw data from listening tests 1) and 2) and the scores resulting from factor analysis are included as documentation of the  [Nautilus Speaker Characterization (NSC) Corpus](http://www.qu.tu-berlin.de/?id=nsc-corpus). 

For the listening tests, this [GUI](https://github.com/laufergall/GUI_SpeakerCharacteristics) written in Java (adapted depending on the questionnaire items and audio stimuli) was employed.

### 1) NSC labeling of speaker characteristics

The semi-spontaneous "Pizza" dialogs (clean microphone speech, and shortened) from 300 speakers (126 males, 174 females) have been evaluated employing a questionnaire involving a 34-item semantic differential rating scale.

- 114 participants (raters) performed the test (70 males and 44 females)
- Mean age = 24.5, standard deviation = 3.4
- 11 languages (93 German and 21 other)
- each test participant rated, on average, 16,4 male speakers and 23.2 female speakers



### 2) NSC labeling of voice descriptions



### 3) Effects of bandwidth on speaker characteristics



### 4) Effects of bandwidth on voice descriptions



### 5) Speech quality of speakers of extreme WAAT

WAAT stands for "warmth-attractiveness", the first two dimensions resulting from the factor analysis of speaker characteristics. 










## Semantic differential questionnaire items

Used for listening tests 1), 2), and 3).

These items can also be seen in:
Fernández Gallardo, L. and Weiss, B., "The Nautilus Speaker Characterization Corpus: Speech Recordings and Labels of Speaker Characteristics and Voice Descriptions," in International Conference on Language Resources and Evaluation (LREC), 2018.

### German (original)

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



### English (translated) 

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
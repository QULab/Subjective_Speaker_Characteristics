### This repository

As part of my [Postdoc project](http://www.qu.tu-berlin.de/?id=lfernandez), I collected subjective ratings of speaker and voice characteristics. In this repository, I explore these data:

* Factor analysis to find dimensions of interpersonal speaker characteristics
* Statistical data analysis of effects of communication channels

The raw data from listening tests and the factor scores are included as documentation in the  [Nautilus Speaker Characterization (NSC) Corpus](http://www.qu.tu-berlin.de/?id=nsc-corpus). For the listening tests, this [GUI](https://github.com/laufergall/GUI_SpeakerCharacteristics) written in Java (adapted depending on the questionnaire items and audio stimuli) was employed.

I also employed these data to perform [predictive modeling](https://github.com/laufergall/ML_Speaker_Characteristics) of speaker characteristics.



### Listening tests

A set of 300 speakers (126 males, 174 females) has been evaluated employing a questionnaire involving a 34-item semantic differential rating scale.

- 114 participants (raters) performed the test (70 males and 44 females)
- Mean age = 24.5, standard deviation = 3.4
- 11 languages (93 German and 21 other)
- each test participant rated, on average, 16,4 male speakers and 23.2 female speakers



### Questionnaire items

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
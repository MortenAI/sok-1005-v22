# MAPPEOPPGAVE 2
# Av Morten Ivarrud

# Jeg har samarbeidet sammen med:
# Tobias Pedersen
# Erlend Kristensen
# Jørgen Johansen


install.packages('jsonlite')

library(jsonlite)
library(tidyverse)
library(ggplot2)

covid_deaths <- #laster inn datasett med JSON. 
  fromJSON(readLines('https://static01.nyt.com/newsgraphics/2021/12/20/us-coronavirus-deaths-2021/ff0adde21623e111d8ce103fedecf7ffc7906264/scatter.json'))


##---OPPGAVE 1

#covid_deaths <- covid_deaths %>% 
#mutate(fully_vaxt_pct = fully_vaccinated_pct_of_pop * 100)


covid_deaths %>% #henter fra datasett
  ggplot(aes(x= fully_vaccinated_pct_of_pop, y = deaths_per_100k, label = name, color = name))+ #lager plot. Legger til x -og y-akse + farge og labels.
  geom_point(size = 3, alpha = 0.5)+ #størrelse og gjennomsiktighet på "points"
  geom_text(hjust=0.5, vjust=-0.9, size=2)+ #tekst ved points. Størrelse og plassering.
  scale_x_continuous(labels = scales::percent, limits = c(0.45, 0.8), #x-akse vises i prosent.
                     breaks = seq(0.45, 0.8, by = 0.05))+ #bestemmer skala og oppdeling til x-aksen.
  scale_y_continuous(limits = c(0, 20))+ #bestemmer skala til y-aksen.
  theme_bw()+ #svart/hvitt tema
  theme(legend.position = "none", plot.title = element_text(face = "bold"))+ #fjerner annotasjon på siden av grafen + gjør overskrifent "fet"
  ylab('Avarage monthly deaths per 100,000')+ #tekst ved y-akse
  xlab('Share of total population fully vaccinated')+ #tektst ved x-akse
  labs(title = 'Covid-19 deaths since universal adult vaccine 
       eligibility compared with vaccination rates') #overskrift



##---OPPGAVE 2---

lm(deaths_per_100k ~ fully_vaccinated_pct_of_pop, data = covid_deaths)

lm(formula = deaths_per_100k ~ fully_vaccinated_pct_of_pop, data = covid_deaths)

# "Intercept" viser hvor y-aksen skjæres når x = 0. 
# I dette tilfellet betyr det at når vaksinasjonsraten er lik 0%, så dør 31,15 per 100 000 av covid i tidsperioden dataen er hentet fra. 
# Dette er ikke emperiske tall, men et anslag gjort med regresjons-modellen.
#
# Tallet under "fully_vaccinated_pct_of_pop" vil være mellom -100 og 100 og vise "korrelasjon-styrken" mellom variablene.
# Jo nærmere tallet er -100 eller 100 jo sterkere er korrelasjonen. Jo nærmere tallet er 0, jo svakere er korrelasjon.
# Vårt tall er -36.66. Dette viser en negativ korrelasjon mellom antall døde og prosentandell vaksinerte.
# Altså, når færre dør, så øker prosentandelen vaksinerte. Eller omvendt, som jo er den mest logiske antakelsen i dette tilfellet.
# Flere vaksinerte fører til færre døde.


##-- Plot med regresjonslinje 

covid_deaths %>% 
  ggplot(aes(x= fully_vaccinated_pct_of_pop, y = deaths_per_100k, label = name, color = name))+
  geom_point(size = 3, alpha = 0.5)+
  geom_text(hjust=0.5, vjust=-0.9, size=2)+
  scale_x_continuous(labels = scales::percent, limits = c(0.45, 0.8),
                     breaks = seq(0.45, 0.8, by = 0.05))+
  scale_y_continuous(limits = c(0, 20))+
  theme_bw()+
  theme(legend.position = "none", plot.title = element_text(face = "bold"))+
  ylab('Avarage monthly deaths per 100,000')+
  xlab('Share of total population fully vaccinated')+
  labs(title = 'Covid-19 deaths since universal adult vaccine
       eligibility compared with vaccination rates')+
  geom_smooth(method = lm, col = 'deepskyblue3', se = FALSE) #legger til regresjonslinje med geom_smooth. Legger til farge og fjerner "skyggen" til linjen.

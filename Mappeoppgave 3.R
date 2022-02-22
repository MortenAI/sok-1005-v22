# MAPPEOPPGAVE 3
# Av Morten Ivarrud

# Jeg har samarbeidet sammen med:
# Tobias Pedersen
# Erlend Kristensen
# Jørgen Johansen



library(rvest)
library(tidyverse)
library(ggrepel)

vintertest <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132%22")


vintertest <- vintertest %>%
  html_element("table") %>%
  html_table(header = TRUE)


vintertest <- vintertest %>%
  subset(!STOPP == "x") %>% 
  mutate(STOPP = str_remove_all(STOPP, "km"),
         STOPP = as.numeric(STOPP),
         Avvik = str_remove_all(Avvik, "%"),            #Rydder i datasettet.
         Avvik = str_replace(Avvik, ",","."),       #Gjør variabler om til numeric    
         Avvik = as.numeric(Avvik),              # ..og fjerner undøvendige bokstaver/siffer/symboler.
         `WLTP-tall` = substr(`WLTP-tall`, 0, 3),# Fjerner også et par rader uten relevant informasjon.
         `WLTP-tall` = as.numeric(`WLTP-tall`)
         )

##-- OPPGAVE 1 
##-- Plot


vintertest_plot <- vintertest %>% 
  ggplot(aes(x = `WLTP-tall`, y = STOPP))+
  geom_point(color = 'steelblue3', size = 2, alpha = 0.7)+
  scale_x_continuous(breaks = seq(200, 600, by = 100), limits=c(200, 600))+ 
  scale_y_continuous(breaks = seq(200, 600, by = 100), limits=c(200, 600))+
  geom_abline(slope= 1, size = 0.5, color = "red")+
  theme_bw()+
  geom_text(aes(label = "Her burde vi vært", x = 350, y = 440))+
  geom_segment(aes(x = 350, y = 425, xend = 370 , yend = 375),
               arrow = arrow(length = unit(0.5, "cm")), color = 'red')+
  labs(title = 'Rekkevidde på el-biler under norske vintertemperaturer (0 - 10 minus deg. C)')+
  ylab('Faktisk rekkevidde')+
  xlab('Rekkevidde oppgitt av bilprodusentene')+
  theme(plot.title = element_text(face = "bold"))
  
  
  
vintertest_plot



##-- OPPGAVE 2


lm(STOPP ~ `WLTP-tall`, data = vintertest)

# Intercept viser hvor regresjonslinja skjærer y-aksen.
# 0.8671 er stigningstallet til regresjonslinja og viser at det er et avvik
# mellom bilprodusentenes påståtte kjørelengde og faktisk kjørelengde.


##-- Plot med regresjonslinje

vintertest_plot + 
  geom_smooth(method = lm, col = 'olivedrab2', se = FALSE)+
  geom_text(aes(label = "Her er vi", x = 575, y = 380))+
  geom_segment(aes(x = 575, y = 390, xend = 560 , yend = 450),
               arrow = arrow(length = unit(0.5, "cm")), color = 'olivedrab2')


# Vi ser at regresjonslinjen ligger mer eller mindre parallelt med
# den røde linjen som viser bilprodusentenes påståtte kjørelengde.
# Samtlige av bilene oppnår litt kortere kjørelengde under kalde forhold.
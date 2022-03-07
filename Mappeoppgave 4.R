# MAPPEOPPGAVE 4
# Morten Ivarrud
#
# Jeg har samarbeidet med:
# Erlend Kristensen Olsen
# Jørgen Johansen
# Tobias Pedersen


library(purrr)
library(tidyverse)
library(rlist)
library(rvest)


# Skraper timeplanen til 1005, 1016 og 1006. Legger disse inn i en liste.

emne_liste <- list("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&week=1-20&View=list",
                  "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1016-1&week=1-20&View=list",
                  "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1006-1&week=1-20&View=list")


# Lager en funksjon med hjelp av "scrape_timeplan" koden.

timeplan_funksjon <- function(emne_liste)
{
  df <-
  read_html(emne_liste) %>% 
  html_nodes(., 'table') %>% 
  html_table(., fill=TRUE) %>% 
  list.stack(.)
  colnames(df) <- df[1,]
  df <- df %>% filter(!Dato=="Dato") 
  df <- df %>% separate(Dato, into = c("Dag", "Dato"), sep = "(?<=[A-Za-z])(?=[0-9])")
  df$Dato <- as.Date(df$Dato, format="%d.%m.%Y")
  df$Uke <- strftime(df$Dato, format = "%V")
  df <- df %>% select(Dag,Dato,Uke,Tid,Rom, Emnekode)
  return(df)
}

##------------

# Mapper timeplanen

komplett_timeplan <- map(emne_liste, timeplan_funksjon)


# Binder sammen og fjerner radene som inneholder "NA"

komplett_timeplan <- 
  bind_rows(komplett_timeplan) %>% 
  na.omit(komplett_timeplan)


view(komplett_timeplan)

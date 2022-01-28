install.packages("tidyverse")
install.packages("zoo")

library(tidyverse)
library(ggplot2)
library(readr)
library(zoo)
library(cowplot)


##----LOWER_TROPOSPHERE------

uahncdc_lt_6_0 <- read_table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")

lower_troposphere <- uahncdc_lt_6_0 %>%
  filter(Year <= 3000) %>% 
  mutate(NoPol1 = NoPol) %>% 
  mutate('Atmospheric layer' = 'Lower\n troposphere') %>% 
  relocate(`Atmospheric layer`, .before = Year) %>% 
  mutate(date = paste(Year, Mo, sep = ".")) %>% 
  arrange(Year)


lower_troposphere[ , 2:32] <- apply(lower_troposphere[ , 2:32], 2, function(x) as.numeric(as.character(x)))

  
##---MID_TROPOSPHERE---

uahncdc_mt_6_0 <- read_table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt")

mid_troposphere <- uahncdc_mt_6_0 %>%
  filter(Year <= 3000) %>% 
  mutate(NoPol2 = NoPol) %>% 
  mutate('Atmospheric layer' = 'Mid\n troposphere') %>% 
  relocate(`Atmospheric layer`, .before = Year) %>% 
  mutate(date = paste(Year, Mo, sep = ".")) %>% 
  arrange(Year)


mid_troposphere[ , 2:32] <- apply(mid_troposphere[ , 2:32], 2, function(x) as.numeric(as.character(x)))


##---TROPOPAUSE---

uahncdc_tp_6_0 <- read_table("http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt")

tropopause <- uahncdc_tp_6_0 %>%
  filter(Year <= 3000) %>% 
  mutate(NoPol3 = NoPol) %>% 
  mutate('Atmospheric layer' = 'Tropopause') %>% 
  relocate(`Atmospheric layer`, .before = Year) %>% 
  mutate(date = paste(Year, Mo, sep = ".")) %>% 
  arrange(Year)


tropopause[ , 2:32] <- apply(tropopause[ , 2:32], 2, function(x) as.numeric(as.character(x)))


##---LOWER_STRATOSPHERE---

uahncdc_ls_6_0 <- read_table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt")

lower_stratosphere <- uahncdc_ls_6_0 %>%
  filter(Year <= 3000) %>% 
  mutate(NoPol4 = NoPol) %>% 
  mutate('Atmospheric layer' = 'Lower\n stratosphere') %>% 
  relocate(`Atmospheric layer`, .before = Year) %>% 
  mutate(date = paste(Year, Mo, sep = ".")) %>% 
  arrange(Year)


lower_stratosphere[ , 2:32] <- apply(lower_stratosphere[ , 2:32], 2, function(x) as.numeric(as.character(x)))


##---ALLE4----

alle4 <- bind_rows(lower_troposphere, mid_troposphere, tropopause, lower_stratosphere)

alle4 <- alle4 %>% 
  arrange(Year, Mo) %>% 
  relocate(date, .after = Mo) %>% 
  relocate(NoPol, .after = NoPol4) %>% 
  mutate(RollmeanNoPol = rollmean(NoPol, k = 13, fill = NA))


##---OPPGAVE 1----

lower_troposphere_2 <- lower_troposphere %>% 
  mutate(RollmeanGlobe = rollmean(Globe, k = 13, fill = NA))

lower_troposphere_2[ , 2:33] <- apply(lower_troposphere_2[ , 2:33], 2, function(x) as.numeric(as.character(x)))

##---PLOT---

lower_troposphere_2 %>% 
  ggplot(aes(x = date, y = Globe))+
  geom_point(alpha = 0.5, color = 'dark blue')+
  geom_line(alpha = 0.2, color = 'dark blue')+
  geom_smooth(aes(y = RollmeanGlobe, col = "13-måneders \n glidende \n gjennomsnitt"), span = 0.1)+
  theme_bw()+
  geom_hline(yintercept= 0, alpha = 0.5)+
  scale_y_continuous(name="Temperatur (deg. C)", limits=c(-0.7, 0.9), 
                     breaks = scales::breaks_width(0.1))+
  scale_x_continuous(breaks = scales::breaks_width(2))+
  labs(title = 'Nedre Troposfære', subtitle = 'Temperaturer 1978-2021', x = 'År', col = '')


##---OPPGAVE 2---
##---ENKELT PLOT MED GJENNOMSNITT----

alle4 %>% 
  ggplot()+
  geom_point(aes(x = date, y = NoPol1, group = 'Atmospheric layer', colour = 'Lower\n troposphere'), alpha = 0.8)+
  geom_point(aes(x = date, y = NoPol2, group = 'Atmospheric layer', colour = 'Mid\n troposphere'), alpha = 0.8)+
  geom_point(aes(x = date, y = NoPol3, group = 'Atmospheric layer', colour = 'Tropopause'), alpha = 0.8)+
  geom_point(aes(x = date, y = NoPol4, group = 'Atmospheric layer', colour = 'Lower\n stratosphere'), alpha = 0.8)+
  geom_line(aes(x = date, y = NoPol4), color = 'red', alpha = 0.2, data = alle4[!is.na(alle4$NoPol4),])+
  geom_line(aes(x = date, y = NoPol2), color = 'dark green', alpha = 0.2, data = alle4[!is.na(alle4$NoPol2),])+
  geom_line(aes(x = date, y = NoPol1), color = 'dark blue', alpha = 0.2, data = alle4[!is.na(alle4$NoPol1),])+
  geom_line(aes(x = date, y = NoPol3), color = 'dark violet', alpha = 0.2, data = alle4[!is.na(alle4$NoPol3),])+
  theme_bw()+
  scale_y_continuous(name="Temperatures (deg. C)", limits=c(-10, 10), 
                     breaks = scales::breaks_width(1))+
  scale_x_continuous(name = 'Year', limits = c(1978, 2022), breaks = scales::breaks_width(2))+
  geom_smooth(aes(x = date, y = RollmeanNoPol, col = 'Running,\n centered\n 13-month\n average'))+
  labs(title = 'Atmospheric temperatureres 1978-2021', col = '')


##---PLOT GRID------

p1 <- alle4 %>% 
  ggplot(aes(x = date, y = NoPol1))+
  geom_point(aes(x = date, y = NoPol1), color = 'dark green',  alpha = 0.1)+
  geom_line(aes(x = date, y = NoPol1), color = 'dark green', alpha = 0.6, data = alle4[!is.na(alle4$NoPol1),])+
  scale_y_continuous(name="Temperature (deg. C)", limits=c(-10, 10), 
                     breaks = scales::breaks_width(2))+
  scale_x_continuous(limits = c(1978, 2022), breaks = scales::breaks_width(8))+
  xlab('Year')

p2 <- alle4 %>% 
  ggplot(aes(x = date, y = NoPol2))+
  geom_point(aes(x = date, y = NoPol2), color = 'dark blue',  alpha = 0.1)+
  geom_line(aes(x = date, y = NoPol2), color = 'dark blue', alpha = 0.6, data = alle4[!is.na(alle4$NoPol2),])+
  scale_y_continuous(name="Temperature (deg. C)", limits=c(-10, 10), 
                     breaks = scales::breaks_width(2))+
  scale_x_continuous(limits = c(1978, 2022), breaks = scales::breaks_width(8))+
  xlab('Year')

p3 <- alle4 %>% 
  ggplot(aes(x = date, y = NoPol3))+
  geom_point(aes(x = date, y = NoPol3), color = 'red',  alpha = 0.1)+
  geom_line(aes(x = date, y = NoPol3), color = 'red', alpha = 0.6, data = alle4[!is.na(alle4$NoPol3),])+
  scale_y_continuous(name="Temperature (deg. C)", limits=c(-10, 10), 
                     breaks = scales::breaks_width(2))+
  scale_x_continuous(limits = c(1978, 2022), breaks = scales::breaks_width(8))+
  xlab('Year')

p4 <- alle4 %>% 
  ggplot(aes(x = date, y = NoPol4))+
  geom_point(aes(x = date, y = NoPol4), color = 'dark violet',  alpha = 0.1)+
  geom_line(aes(x = date, y = NoPol4), color = 'dark violet', alpha = 0.6, data = alle4[!is.na(alle4$NoPol4),])+
  scale_y_continuous(name="Temperature (deg. C)", limits=c(-10, 10), 
                     breaks = scales::breaks_width(2))+
  scale_x_continuous(limits = c(1978, 2022), breaks = scales::breaks_width(8))+
  xlab('Year')

p5 <- alle4 %>% 
  ggplot(aes(x = date, y = NoPol))+
  geom_smooth(aes(y = RollmeanNoPol), color = 'brown', span = 0.1)+
  scale_y_continuous(name="Temperature (deg. C)", limits=c(-10, 10), 
                     breaks = scales::breaks_width(2))+
  scale_x_continuous(limits = c(1978, 2022), breaks = scales::breaks_width(8))+
  xlab('Year')


plot_grid(p1,p2,p3,p4,p5, labels = c('Lower Troposphere', 'Mid-troposohere',
                                     'Tropopause', 'Lower stratosphere', 'Average of all four'))


##---FACET_WRAP---

alle4 %>% 
  ggplot(aes(col = `Atmospheric layer`))+
  geom_point(aes(x = date, y = NoPol),  alpha = 0.1)+
  geom_line(aes(x = date, y = NoPol), alpha = 0.6)+
  scale_y_continuous(name="Temperature (deg. C)", limits=c(-10, 10), 
                     breaks = scales::breaks_width(2))+
  scale_x_continuous(limits = c(1978, 2022), breaks = scales::breaks_width(4))+
  facet_wrap(~ `Atmospheric layer`)+
  theme(strip.background = element_rect(fill="brown"))+
  labs(subtitle = 'Atmospheric temperatures 1978-2021', x = 'År', col = 'Atmospheric\n layer')+
  theme_bw()

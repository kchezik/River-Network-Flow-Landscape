library(tidyverse)
setwd("~/sfuvault/Simon_Fraser_University/PhD_Research/Projects/Data/Original_Data/IAEA WISER Isotope Data")
dat = read.table("Chesik_161007_Summary copy.txt", header = T)

dat = dat %>% select(-Date, -Tray.Pos, -Sample)
dat = dat %>% mutate(type = substring(dat$Name,1,1))

ggplot(dat, aes(Mean_d18O, Mean_dD, color = type)) +
	geom_point() +
	geom_smooth(method = "lm")

lake = dat %>% filter(Mean_d18O>(-16.4), type == "S") %>% arrange(Name) %>% mutate(impact = "lake")
stream = dat %>% filter(Mean_d18O<(-16.4), type == "S") %>% arrange(Name) %>% mutate(impact = "snow/rain")
rain = dat %>% filter(type == "R") %>% mutate(impact = "rain")

dat2 = bind_rows(lake,stream,rain)

ggplot(dat2, aes(Mean_d18O, Mean_dD, color = impact)) +
	geom_point() +
	geom_smooth(method = "lm")

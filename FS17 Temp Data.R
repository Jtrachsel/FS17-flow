setwd("~/Documents/FS Studies/FS17/FS17 Obj2 Temp Data/")
library(tidyverse) ##tidyverse contains ggplot2, dplyr, tidr, readr

Temp <- read.csv(file = "FS17 Obj2 Temp Data.csv", header = T, stringsAsFactors = F)
head(Temp)
Temp %>% ## shows temperature by dpi for each trt group individually
  ggplot(aes(x=dpi,y=TempC)) + 
  geom_boxplot(aes(group = dpi, color = Trt)) +
  theme(axis.text.x=element_text(angle = -45, hjust =0)) +
  facet_wrap( ~ Trt)

Temp %>% filter(Trt == "inBCG-Ch") %>% ##looks at individual pigs by treatment groups
  ggplot(aes(x=dpi, y=TempC)) +
  geom_line(aes(group = pig, color = pig))


## need function to calculate standard error for each treatment group by day post infection (dpi)
## something like... sem <- sd(x)/sqrt(length(x))
## and then use it in the code below and switch out sd for sem in geom_errorbar
Mean <- Temp %>%
  group_by(Trt, dpi) %>% 
  summarize(mean=mean(TempC), sd=sd(TempC), n=length(TempC)) %>%
  ggplot(aes(x=dpi, y=mean)) +
  geom_line(aes(group = Trt, color = Trt)) +
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd, color=Trt))

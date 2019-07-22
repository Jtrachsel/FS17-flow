## code base is FS12

setwd("~/Desktop/FS17 Flow FCS FilesData/")
library(ggplot2)
library(tidyverse)
library(reshape2)

FS17 <- read.csv(file = "FS17 Flow Results All.csv", header = T, stringsAsFactors = F)
head(FS17)
FS17_subset <- FS17[, c(2, 6:16)]
head(FS17_subset)
FS17_long <- melt(FS17_subset, value.name = "PercentSingleCell")
head(FS17_long)
FS17_plot_14Feb <- FS17_long %>% 
  filter(date == "14-Feb") %>% 
  ggplot(aes(x=Trt,y=PercentSingleCell)) + 
  geom_boxplot() +
  geom_jitter(aes(color=Trt)) +
  theme(axis.text.x=element_text(angle = -45, hjust =0)) +
  facet_wrap( ~ variable, scales = "free_y")
FS17_plot_14Feb
ggsave("FS17_plot_14Feb.pdf", plot = FS17_plot_14Feb, width = 20, height = 20, path = "~/Desktop")

FS17_plot_15Feb <- FS17_long %>% 
  filter(date == "15-Feb") %>% 
  ggplot(aes(x=Trt,y=PercentSingleCell)) + 
  geom_boxplot() +
  geom_jitter(aes(color=Trt)) +
  theme(axis.text.x=element_text(angle = -45, hjust =0)) +
  facet_wrap( ~ variable, scales = "free_y")
FS17_plot_15Feb
ggsave("FS17_plot_15Feb.pdf", plot = FS17_plot_15Feb, width = 20, height = 20, path = "~/Desktop")

FS17_plot_16Feb <- FS17_long %>% 
  filter(date == "16-Feb") %>% 
  ggplot(aes(x=Trt,y=PercentSingleCell)) + 
  geom_boxplot() +
  geom_jitter(aes(color=Trt)) +
  theme(axis.text.x=element_text(angle = -45, hjust =0)) +
  facet_wrap( ~ variable, scales = "free_y")
FS17_plot_16Feb
ggsave("FS17_plot_16Feb.pdf", plot = FS17_plot_16Feb, width = 20, height = 20, path = "~/Desktop")

FS17_plot_17Feb <- FS17_long %>% 
  filter(date == "17-Feb") %>% 
  ggplot(aes(x=Trt,y=PercentSingleCell)) + 
  geom_boxplot() +
  geom_jitter(aes(color=Trt)) +
  theme(axis.text.x=element_text(angle = -45, hjust =0)) +
  facet_wrap( ~ variable, scales = "free_y")
FS17_plot_17Feb
ggsave("FS17_plot_17Feb.pdf", plot = FS17_plot_17Feb, width = 20, height = 20, path = "~/Desktop")

FS17_plot_19Feb <- FS17_long %>% 
  filter(date == "19-Feb") %>% 
  ggplot(aes(x=Trt,y=PercentSingleCell)) + 
  geom_boxplot() +
  geom_jitter(aes(color=Trt)) +
  theme(axis.text.x=element_text(angle = -45, hjust =0)) +
  facet_wrap( ~ variable, scales = "free_y")
FS17_plot_19Feb
ggsave("FS17_plot_19Feb.pdf", plot = FS17_plot_19Feb, width = 20, height = 20, path = "~/Desktop")

FS17_plot_22Feb <- FS17_long %>% 
  filter(date == "22-Feb") %>% 
  ggplot(aes(x=Trt,y=PercentSingleCell)) + 
  geom_boxplot() +
  geom_jitter(aes(color=Trt)) +
  theme(axis.text.x=element_text(angle = -45, hjust =0)) +
  facet_wrap( ~ variable, scales = "free_y")
FS17_plot_22Feb
ggsave("FS17_plot_22Feb.pdf", plot = FS17_plot_22Feb, width = 20, height = 20, path = "~/Desktop")

FS17_plot_25Feb <- FS17_long %>% 
  filter(date == "25-Feb") %>% 
  ggplot(aes(x=Trt,y=PercentSingleCell)) + 
  geom_boxplot() +
  geom_jitter(aes(color=Trt)) +
  theme(axis.text.x=element_text(angle = -45, hjust =0)) +
  facet_wrap( ~ variable, scales = "free_y")
FS17_plot_25Feb
ggsave("FS17_plot_25Feb.pdf", plot = FS17_plot_25Feb, width = 20, height = 20, path = "~/Desktop")




## the above graphs allow us to look at treatment groups for each day, but would like to follow
## individual pigs and treatment groups across time within the same graphs
## this webpage might be helpful... uses spaghettie plots for longitudinal data 
## https://stats.idre.ucla.edu/r/faq/how-can-i-visualize-longitudinal-data-in-ggplot2/
setwd("~/Desktop/FS17 Flow FCS Files/")
library(tidyverse) ##tidyverse contains ggplot2, dplyr, tidr, readr
library(methods)

FS17 <- read.csv(file = "FS17 Flow Results All.csv", header = T, stringsAsFactors = F)
head(FS17)

FS17[, c(-2, -3, -4, -17)] %>% ##removing unnecessary columns
  gather(key = CellType, value = Percentage, CD3p:Monocytes) %>% ##moving to long format
  ggplot(aes(dpc, Percentage)) +
  geom_smooth(method = 'loess', aes(fill = Trt, color=Trt)) +
  facet_wrap( ~ CellType, scales = "free_y") ## code isn't bad, gives you a decent graph


##attempting to look at individual pigs over time
FS17[, c(-2, -3, -4, -17)] %>% ##removing unnecessary columns
  gather(key = CellType, value = Percentage, CD3p:Monocytes) %>% ##moving to long format
  filter(Trt == "Mock") %>%
  ggplot(aes(dpc, Percentage)) +
  geom_line(aes(group = Pig, color=Pig)) +
  facet_wrap( ~ CellType, scales = "free_y")

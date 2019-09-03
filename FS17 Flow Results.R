## code base is FS12


# library(ggplot2)
library(tidyverse)
# library(reshape2)

FS17 <- read.csv(file = "./data/FS17 Flow Results All.csv", header = T, stringsAsFactors = F)
head(FS17)

### select columns... ###

# FS17_subset <- FS17[, c(2, 6:16)]
# head(FS17_subset)

# Jules #

FS17_subset <- FS17 %>% select(-FlowID)

#
# FS17_long <- melt(FS17_subset, value.name = "PercentSingleCell")
# head(FS17_long)

## Maybe try this instead...
# keep as much metadata as possible. 
# the -c(1:6) is telling the gather function to not gather up the metadata
# or another way to think of it is to gather up everyhitng except for cols 1:6

FS17_long <- FS17_subset %>% gather(key='cell_type', value = 'value', -c(1:6))


FS17_plot_14Feb <- FS17_long %>% 
  filter(date == "14-Feb") %>% 
  ggplot(aes(x=Trt,y=value)) + 
  geom_boxplot() +
  geom_jitter(aes(color=Trt)) +
  theme(axis.text.x=element_text(angle = -45, hjust =0)) +
  facet_wrap( ~ cell_type, scales = "free_y")

FS17_plot_14Feb

# ggsave("FS17_plot_14Feb.pdf", plot = FS17_plot_14Feb, width = 20, height = 20, path = "~/Desktop")

FS17_plot_15Feb <- FS17_long %>% 
  filter(date == "15-Feb") %>% 
  ggplot(aes(x=Trt,y=value)) + 
  geom_boxplot() +
  geom_jitter(aes(color=Trt)) +
  theme(axis.text.x=element_text(angle = -45, hjust =0)) +
  facet_wrap( ~ cell_type, scales = "free_y")
FS17_plot_15Feb
# ggsave("FS17_plot_15Feb.pdf", plot = FS17_plot_15Feb, width = 20, height = 20, path = "~/Desktop")

FS17_plot_16Feb <- FS17_long %>% 
  filter(date == "16-Feb") %>% 
  ggplot(aes(x=Trt,y=value)) + 
  geom_boxplot() +
  geom_jitter(aes(color=Trt)) +
  theme(axis.text.x=element_text(angle = -45, hjust =0)) +
  facet_wrap( ~ cell_type, scales = "free_y")
FS17_plot_16Feb
# ggsave("FS17_plot_16Feb.pdf", plot = FS17_plot_16Feb, width = 20, height = 20, path = "~/Desktop")

FS17_plot_17Feb <- FS17_long %>% 
  filter(date == "17-Feb") %>% 
  ggplot(aes(x=Trt,y=value)) + 
  geom_boxplot() +
  geom_jitter(aes(color=Trt)) +
  theme(axis.text.x=element_text(angle = -45, hjust =0)) +
  facet_wrap( ~ cell_type, scales = "free_y")
FS17_plot_17Feb
# ggsave("FS17_plot_17Feb.pdf", plot = FS17_plot_17Feb, width = 20, height = 20, path = "~/Desktop")

FS17_plot_19Feb <- FS17_long %>% 
  filter(date == "19-Feb") %>% 
  ggplot(aes(x=Trt,y=value)) + 
  geom_boxplot() +
  geom_jitter(aes(color=Trt)) +
  theme(axis.text.x=element_text(angle = -45, hjust =0)) +
  facet_wrap( ~ cell_type, scales = "free_y")
FS17_plot_19Feb
# ggsave("FS17_plot_19Feb.pdf", plot = FS17_plot_19Feb, width = 20, height = 20, path = "~/Desktop")

FS17_plot_22Feb <- FS17_long %>% 
  filter(date == "22-Feb") %>% 
  ggplot(aes(x=Trt,y=value)) + 
  geom_boxplot() +
  geom_jitter(aes(color=Trt)) +
  theme(axis.text.x=element_text(angle = -45, hjust =0)) +
  facet_wrap( ~ cell_type, scales = "free_y")
FS17_plot_22Feb
# ggsave("FS17_plot_22Feb.pdf", plot = FS17_plot_22Feb, width = 20, height = 20, path = "~/Desktop")

FS17_plot_25Feb <- FS17_long %>% 
  filter(date == "25-Feb") %>% 
  ggplot(aes(x=Trt,y=value)) + 
  geom_boxplot() +
  geom_jitter(aes(color=Trt)) +
  theme(axis.text.x=element_text(angle = -45, hjust =0)) +
  facet_wrap( ~ cell_type, scales = "free_y")
FS17_plot_25Feb

# ggsave("FS17_plot_25Feb.pdf", plot = FS17_plot_25Feb, width = 20, height = 20, path = "~/Desktop")




## the above graphs allow us to look at treatment groups for each day, but would like to follow
## individual pigs and treatment groups across time within the same graphs
## this webpage might be helpful... uses spaghettie plots for longitudinal data 
## https://stats.idre.ucla.edu/r/faq/how-can-i-visualize-longitudinal-data-in-ggplot2/
# setwd("~/Desktop/FS17 Flow FCS Files/")
# library(tidyverse) ##tidyverse contains ggplot2, dplyr, tidr, readr
# library(methods)

FS17 <- read.csv(file = "./data/FS17 Flow Results All.csv", header = T, stringsAsFactors = F)
head(FS17)


FS17 %>% select(-FlowID) %>% 
  gather(key = 'CellType', value = 'Percentage', -c(1:6)) %>% 
  ggplot(aes(dpc, Percentage)) +
  geom_smooth(method = 'loess', aes(fill = Trt, color=Trt)) +
  facet_wrap( ~ CellType, scales = "free_y")

# FS17[, c(-2, -3, -4, -17)] %>% ##removing unnecessary columns
#   gather(key = CellType, value = Percentage, CD3p:Monocytes) %>% ##moving to long format
#   ggplot(aes(dpc, Percentage)) +
#   geom_smooth(method = 'loess', aes(fill = Trt, color=Trt)) +
#   facet_wrap( ~ CellType, scales = "free_y") ## code isn't bad, gives you a decent graph


##attempting to look at individual pigs over time
FS17 %>% select(-FlowID) %>% 
  gather(key = 'CellType', value = 'Percentage', -c(1:6)) %>% ##moving to long format
  filter(Trt == "Mock") %>%
  ggplot(aes(dpc, Percentage)) +
  geom_line(aes(group = Pig, color=Pig)) +
  facet_wrap( ~ CellType, scales = "free_y")

# this looks weird because you need to make your pig variable a factor or a character


##########


FS17 %>% select(-FlowID) %>% 
  gather(key = 'CellType', value = 'Percentage', -c(1:6)) %>%
  mutate(Pig = factor(Pig)) %>% 
  filter(Trt == "Mock") %>%
  ggplot(aes(dpc, Percentage)) +
  geom_line(aes(group = Pig, color=Pig)) +
  facet_wrap( ~ CellType, scales = "free_y")




######################################################

FS17 <- read_csv(file = "./data/FS17 Flow Results All.csv")

str(FS17)

FS17_clean <- FS17 %>%
  select(-FlowID) %>%
  mutate(Pig=as.character(Pig))

FS17_long <- FS17_clean %>%
  gather(key='cell_type', value = 'percentage', -c(1:6))


FS17_sum <- FS17_long %>%
  group_by(dpc, Trt, cell_type) %>% 
  summarise(mean_p = mean(percentage), 
            num_obs = n(), 
            std_dev = sd(percentage), 
            std_err = std_dev/sqrt(num_obs))


FS17_sum %>% 
  ggplot(aes(x=dpc, y=mean_p, color=Trt, fill = Trt)) +
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax = mean_p + std_err), alpha = .3) +
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw()

FS17_sum$dpc






FS17_sum %>% 
  filter(cell_type == 'CD21p') %>% 
  ggplot(aes(x=dpc, y=mean_p, color=Trt, fill = Trt)) +
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax = mean_p + std_err), alpha = .3) +
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw()


FS17_sum %>% 
  filter(cell_type == 'CD4pCD8ap') %>% 
  ggplot(aes(x=dpc, y=mean_p, color=Trt, fill = Trt)) +
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax = mean_p + std_err), alpha = .3) +
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw()





FS17_sum %>% 
  filter(Trt %in% c('inBCG-Ch', 'lvBCG-Ch', 'noBCG-Ch', 'weird')) %>% 
  ggplot(aes(x=dpc, y=mean_p, color=Trt, fill = Trt)) +
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax = mean_p + std_err), alpha = .3) +
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw()

#####

FS17_long %>% 
  filter(Trt == 'PRRSVsed') %>% 
  ggplot(aes(x=dpc, y=percentage, color = Pig)) + geom_line() + 
  facet_wrap(~cell_type, scales = 'free')







######## tempdata



temps <- read_csv('data/FS17 Obj2 Temp Data.csv')
temps$pig <- as.character(temps$pig)

temps$pig_day <- paste(temps$pig, temps$dpi)


temps %>% group_by(Trt) %>% summarise(meanT=mean(TempC))

temps %>% ggplot(aes(x=dpi, y=TempC)) + 
  geom_line(aes(group = pig)) +
  facet_wrap(~Trt)+ geom_hline(yintercept = 39.8, color='red')

# same pig 2x measurements per day #
weird_ones <- temps %>% 
  group_by(pig_day) %>%
  tally() %>%
  filter(n != 1) %>% 
  select(pig_day) %>% 
  unlist()

temps %>% filter((pig_day %in% weird_ones)) %>% 
  ggplot(aes(x=dpi, y=TempC, color=pig)) + geom_point() 


# sum(temps$pig %in% weird_ones$pig & temps$dpi %in% weird_ones$dpi)


temps <- temps %>%
  group_by(pig, dpi) %>%
  mutate(new_temp = mean(TempC))# %>% ggplot(aes(x=dpi, y=TempC)) + geom_line(aes(group=pig))

### NEED TO REMOVE REDUND
temps %>% group_by(pig_day) %>% tally() %>% filter(n>1) %>% select(pig_day) %>% unlist()

temps %>% ggplot(aes(x=dpi, y=new_temp, color=PRRSV)) + geom_line(aes(group=pig))

#######

library(ggforce)




meta <- temps %>%
  ungroup() %>%
  select(pig, Room, BCGTrt, PRRSV, Trt) %>%
  unique()

temp_sum <- temps %>% group_by(pig) %>% 
  summarise(max_temp = max(new_temp), 
            mean_temp = mean(new_temp), 
            min_temp = min(new_temp), 
            sd_temp = sd(new_temp), 
            day_max = dpi[which.max(new_temp)], 
            dif_temp = max_temp - min_temp) %>% left_join(meta)




temp_sum %>% ggplot(aes(x=Trt, y=max_temp, fill=Trt)) + geom_boxplot() + theme_bw()
temp_sum %>% ggplot(aes(x=Trt, y=min_temp, fill=Trt)) + geom_boxplot()+ theme_bw()
temp_sum %>% ggplot(aes(x=Trt, y=sd_temp, fill=Trt)) + geom_boxplot()+ theme_bw()
temp_sum %>% ggplot(aes(x=Trt, y=mean_temp, fill=Trt)) + geom_boxplot()+ theme_bw()
temp_sum %>% ggplot(aes(x=Trt, y=day_max, fill=Trt)) + geom_point(shape=21, size=3)+ theme_bw()
temp_sum %>% ggplot(aes(x=Trt, y=dif_temp, fill=mean_temp)) + geom_point(shape=21, size=4)+ theme_bw()



temp_sum %>%
  ggplot(aes(x=dif_temp, y=mean_temp, fill=max_temp)) +
  geom_point(shape=21, color='black', size=3)+ scale_fill_viridis_c()


temp_sum %>%filter(!(Trt %in% c('PRRSVsed', 'Mock'))) %>% 
  ggplot(aes(x=dif_temp, y=mean_temp, fill=max_temp)) +
  geom_point(shape=21, color='black', size=3)+ scale_fill_viridis_c() +
  geom_text(aes(label=pig), nudge_x = .055) +
  annotate(geom = 'rect',
           xmin = 1.75,
           xmax = 2.7,
           ymin=40.07,
           ymax = 40.8,
           fill=NA,
           color='black')





int_pigs <- temp_sum %>%filter(!(Trt %in% c('PRRSVsed', 'Mock'))) %>%
  filter(dif_temp >1.75 & mean_temp>40.2) %>% 
  select(pig) %>% unlist()



# temp_sum %>% ggplot(aes(x=min_temp, y=max_temp, color=dif_temp)) + geom_point()
# temp_sum %>% ggplot(aes(x=dif_temp, y=mean_temp, color=day_max)) + geom_point()



##########




FS17_long$Trt2 <- ifelse(FS17_long$Pig %in% int_pigs, 'weird', FS17_long$Trt)

FS17_long %>% group_by(Trt, Trt2) %>% tally()

FS17_sum <- FS17_long %>%
  group_by(dpc, Trt2, cell_type) %>% 
  summarise(mean_p = mean(percentage), 
            num_obs = n(), 
            std_dev = sd(percentage), 
            std_err = std_dev/sqrt(num_obs))

FS17_sum %>%
  ggplot(aes(x=dpc, y=mean_p, fill=Trt2)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  facet_wrap(~cell_type, scales = 'free')



FS17_sum %>% filter(cell_type == 'CD4pCD8ap') %>% 
  ggplot(aes(x=dpc, y=mean_p, fill=Trt2)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  facet_wrap(~cell_type, scales = 'free')

FS17_sum %>% filter(dpc != 11) %>% 
  ggplot(aes(x=dpc, y=mean_p, fill=Trt2)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  facet_wrap(~cell_type, scales = 'free')


##

FS17_sum %>% filter(cell_type == 'CD4pCD8ap') %>% 
  filter(!Trt2 %in% c('Mock', 'PRRSVsed')) %>% 
  ggplot(aes(x=dpc, y=mean_p, fill=Trt2)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  facet_wrap(~cell_type, scales = 'free')




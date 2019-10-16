

library(tidyverse)

FS17 <- read.csv(file = "./data/FS17 Flow Results All.csv", header = T, stringsAsFactors = F)



FS17$date
# because the dpc doesnt match up im trying out of Day Of Study time variable
FS17$DOS <- as.numeric(sub('-Feb','',FS17$date)) - 14
FS17 <- FS17[,c(1:6, 18, 7:17)] # this moves the DOS variable up with the metadata
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
# the -c(1:7) is telling the gather function to not gather up the metadata
# or another way to think of it is to gather up everyhitng except for cols 1:6

FS17_long <- FS17_subset %>% gather(key='cell_type', value = 'value', -c(1:7))


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

# FS17 <- read_csv(file = "./data/FS17 Flow Results All.csv")

# str(FS17)

FS17_clean <- FS17 %>%
  select(-FlowID) %>%
  mutate(Pig=as.character(Pig))

FS17_long <- FS17_clean %>%
  gather(key='cell_type', value = 'percentage', -c(1:7))



# summary statistics, mean and stderr for each combination of celltypes treatments and times
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


#### double pos cells seem lower in all animals that got PRRSV

FS17_sum %>% 
  filter(cell_type == 'CD4pCD8ap') %>% 
  ggplot(aes(x=dpc, y=mean_p, color=Trt, fill = Trt)) +
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax = mean_p + std_err), alpha = .3) +
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw()



# no obvious treatment effect....

FS17_sum %>% 
  filter(Trt %in% c('inBCG-Ch', 'lvBCG-Ch', 'noBCG-Ch')) %>% 
  ggplot(aes(x=dpc, y=mean_p, color=Trt, fill = Trt)) +
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax = mean_p + std_err), alpha = .3) +
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw()

##### spaghetti plots for each treatment

FS17_long %>% 
  filter(Trt == 'PRRSVsed') %>% 
  ggplot(aes(x=dpc, y=percentage, color = Pig)) + geom_line() + 
  facet_wrap(~cell_type, scales = 'free')

FS17_long %>% 
  filter(Trt == 'inBCG-Ch') %>% 
  ggplot(aes(x=dpc, y=percentage, color = Pig)) + geom_line() + 
  facet_wrap(~cell_type, scales = 'free')

FS17_long %>% 
  filter(Trt == 'lvBCG-Ch') %>% 
  ggplot(aes(x=dpc, y=percentage, color = Pig)) + geom_line() + 
  facet_wrap(~cell_type, scales = 'free')

FS17_long %>% 
  filter(Trt == 'noBCG-Ch') %>% 
  ggplot(aes(x=dpc, y=percentage, color = Pig)) + geom_line() + 
  facet_wrap(~cell_type, scales = 'free')

FS17_long %>% 
  filter(Trt == 'Mock') %>% 
  ggplot(aes(x=dpc, y=percentage, color = Pig)) + geom_line() + 
  facet_wrap(~cell_type, scales = 'free')




######## tempdata



temps <- read_csv('data/FS17 Obj2 Temp Data.csv')
temps$pig <- as.character(temps$pig)

temps$pig_day <- paste(temps$pig, temps$dpi)

# mean temp for Mock group is 39.8
temps %>% group_by(Trt) %>% summarise(meanT=mean(TempC))

temps %>% ggplot(aes(x=dpi, y=TempC)) + 
  geom_line(aes(group = pig)) +
  facet_wrap(~Trt)+ geom_hline(yintercept = 39.8, color='red')


### NEED TO FIX THIS ###
# same pig 2x measurements per day #
weird_ones <- temps %>% 
  group_by(pig_day) %>%
  tally() %>%
  filter(n != 1) %>% 
  select(pig_day) %>% 
  unlist()

## Can see from this plot the pigs that have multiple measurements per day
temps %>% filter((pig_day %in% weird_ones)) %>% 
  ggplot(aes(x=dpi, y=TempC, color=pig)) + geom_point() 


# this should get rid of the redundant measurements by taking the mean of the 
# multiples as the true value
temps <- temps %>%
  group_by(pig, dpi) %>%
  mutate(TempC = mean(TempC)) %>% 
  select(-TempF) %>% 
  unique()# %>% ggplot(aes(x=dpi, y=TempC)) + geom_line(aes(group=pig))

# just to double check...
temps %>%
  group_by(pig, dpi) %>%tally() %>% filter(n>1)
# Good, no pigs have more than one measurement per timepoint

# are some pigs missing?
temps %>%
  group_by(pig, Trt) %>%tally() %>% ggplot(aes(x=n))+geom_histogram()

temps %>%
  group_by(pig, Trt) %>%tally() %>% filter(n<14)
temps <- temps %>% filter(pig != 517)

temps %>%
  group_by(dpi, Trt) %>%tally() #%>% filter(n<14)

temps %>% ggplot(aes(x=dpi, y=TempC)) + 
  geom_line(aes(group = pig)) +
  facet_wrap(~Trt)+ geom_hline(yintercept = 39.8, color='red')


## this block collects the metadata about these pigs,
# I removed the Room variable because some pigs were moved between rooms, 
# this was causing some duplication issues down below.
meta <- temps %>%
  ungroup() %>%
  select(pig, BCGTrt, PRRSV, Trt) %>% 
  unique()


# this makes each pig's temperatures relative to the minimum temperature of that pig
temps <- temps %>%
  group_by(pig) %>%
  mutate(TempRmin=TempC - min(TempC)) %>% 
  ungroup()

temps %>% group_by(Trt) %>% summarise(meanTemp=mean(TempRmin))


temps %>% ggplot(aes(x=dpi, y=TempRmin))+ geom_line(aes(group = pig)) +
  geom_hline(yintercept = 0.418, color='red')+
  facet_wrap(~Trt)

library(pracma)
# This temp_sum is based on absolute temperatures
temp_sum <- temps %>% group_by(pig) %>% 
  summarise(max_temp = max(TempC), 
            mean_temp = mean(TempC), 
            min_temp = min(TempC), 
            sd_temp = sd(TempC), 
            day_max = dpi[which.max(TempC)], 
            dif_temp = max_temp - min_temp) %>% left_join(meta)

# this temp_sum is based on relative temperatures (each pig was adjusted relative to their minimum temperature)
temp_sum <- temps %>%
  arrange(dpi) %>% 
  group_by(pig) %>% 
  summarise(max_temp = max(TempRmin), 
            mean_temp = mean(TempRmin), 
            min_temp = min(TempRmin), 
            sd_temp = sd(TempRmin), 
            day_max = dpi[which.max(TempRmin)], 
            dif_temp = max_temp - min_temp, 
            AUC_temp=trapz(dpi,TempRmin)) %>% 
  left_join(meta)


temp_sum %>% ggplot(aes(x=Trt, y=max_temp, fill=Trt)) + geom_boxplot() + theme_bw()
temp_sum %>% ggplot(aes(x=Trt, y=AUC_temp, fill=Trt)) + geom_boxplot()+ theme_bw()
temp_sum %>% ggplot(aes(x=Trt, y=sd_temp, fill=Trt)) + geom_boxplot()+ theme_bw()
temp_sum %>% ggplot(aes(x=Trt, y=mean_temp, fill=Trt)) + geom_boxplot()+ theme_bw()
temp_sum %>% ggplot(aes(x=Trt, y=day_max, fill=Trt)) + geom_point(shape=21, size=3)+ theme_bw()
temp_sum %>% ggplot(aes(x=Trt, y=dif_temp, fill=mean_temp)) + geom_point(shape=21, size=4)+ theme_bw()
temp_sum %>% ggplot(aes(x=Trt, y=sd_temp, fill=mean_temp)) + geom_point(shape=21, size=4)+ theme_bw()


# some exploratory plots here
temp_sum %>%
  ggplot(aes(x=max_temp, y=mean_temp, fill=sd_temp)) +
  geom_point(shape=21, color='black', size=3)+ scale_fill_viridis_c()


temp_sum %>%
  ggplot(aes(x=sd_temp, y=max_temp, fill=mean_temp)) +
  geom_point(shape=21, color='black', size=3)+ scale_fill_viridis_c()

temp_sum %>%
  ggplot(aes(x=mean_temp, y=max_temp, fill=sd_temp)) +
  geom_point(shape=21, color='black', size=3)+ scale_fill_viridis_c()

temp_sum %>%
  ggplot(aes(x=sd_temp, y=dif_temp, fill=mean_temp)) +
  geom_point(shape=21, color='black', size=3)+ scale_fill_viridis_c()

temp_sum %>%
  ggplot(aes(x=AUC_temp, y=max_temp, fill=mean_temp)) +
  geom_point(shape=21, color='black', size=3)+ scale_fill_viridis_c()

temp_sum %>%
  ggplot(aes(x=AUC_temp, y=mean_temp, fill=Trt)) +
  geom_point(shape=21, color='black', size=3)

temp_sum %>%
  ggplot(aes(x=AUC_temp, y=max_temp, fill=Trt)) +
  geom_point(shape=21, color='black', size=3)

temp_sum %>%
  ggplot(aes(x=AUC_temp, y=sd_temp, fill=Trt)) +
  geom_point(shape=21, color='black', size=3)


# this was for when I was using absolute temperatures
# temp_sum %>%filter(!(Trt %in% c('PRRSVsed', 'Mock'))) %>% 
#   ggplot(aes(x=dif_temp, y=mean_temp, fill=max_temp)) +
#   geom_point(shape=21, color='black', size=3)+ scale_fill_viridis_c() +
#   geom_text(aes(label=pig), nudge_x = .055) +
#   annotate(geom = 'rect',
#            xmin = 1.75,
#            xmax = 2.7,
#            ymin=40.07,
#            ymax = 40.8,
#            fill=NA,
#            color='black')

# 
# int_pigs <- temp_sum %>%filter(!(Trt %in% c('PRRSVsed', 'Mock'))) %>%
#   filter(dif_temp >1.75 & mean_temp>40.2) %>% 
#   select(pig) %>% unlist()



# scaled_temps%>%#filter(!(Trt %in% c('PRRSVsed', 'Mock'))) %>% 
#   ggplot(aes(x=dif_temp, y=mean_temp, fill=max_temp)) +
#   geom_point(shape=21, color='black', size=3)+ scale_fill_viridis_c()



scaled_temps <- temp_sum %>% mutate_if(is.numeric, scale) # centers and scales variables

# what happens when I dont scale?
# scaled_temps <- temp_sum


#### FEVER SCORE CALC HERE ####
  
# scaled_temps <- scaled_temps %>% mutate(fever_score=max_temp+mean_temp+dif_temp+sd_temp)

# scaled_temps <- scaled_temps %>% mutate(fever_score=mean_temp+sd_temp) 

scaled_temps <- scaled_temps %>% mutate(fever_score=max_temp+mean_temp+sd_temp)

# scaled_temps <- scaled_temps %>% mutate(fever_score=max_temp+mean_temp)

# scaled_temps <- scaled_temps %>% mutate(fever_score=mean_temp)

# scaled_temps <- scaled_temps %>% mutate(fever_score=max_temp)

###
### FILTER THE MOCK AND PRRSV PIGS OUT BEFORE FEVER_CLASS CALC?###

# scaled_temps <- scaled_temps %>% filter(!(Trt %in% c('PRRSVsed', 'Mock')))

######

# maybe?
# scaled_temps$fever_score <- scale(scaled_temps$fever_score)

scaled_temps$fever_class <- cut(scaled_temps$fever_score, 3, labels = c('low', 'mid', 'high'))
scaled_temps$Pig <- scaled_temps$pig



scaled_temps %>% filter(!(Trt %in% c('PRRSVsed', 'Mock'))) %>% 
  ggplot(aes(x=1, y=fever_score))  +
  geom_violin()+
  geom_jitter(aes(color=fever_class),width = .2) + 
  ggtitle('Fever score distribution, without PRRSVed and Mock pigs')

scaled_temps %>% #filter(!(Trt %in% c('PRRSVsed', 'Mock'))) %>% 
  ggplot(aes(x=1, y=fever_score))  +
  geom_violin()+
  geom_jitter(aes(color=fever_class),width = .2) + 
  ggtitle('Fever score distribution, with PRRSVed and Mock pigs')




scaled_temps %>% ggplot(aes(x=Trt, y=fever_score, color=fever_class)) + geom_jitter(width = .2)

scaled_temps %>% ggplot(aes(x=fever_class, y=fever_score, color=fever_score)) + geom_jitter()


LETSDOTHIS <- scaled_temps %>%
  select(Pig, fever_class, fever_score) %>%
  right_join(FS17_long) %>% filter(Pig !=517)

unique(LETSDOTHIS$dpc)

unique(LETSDOTHIS$DOS)
# 11 to 10
# 8 to 7
# 5 to 4

# temp_sum %>% ggplot(aes(x=min_temp, y=max_temp, color=dif_temp)) + geom_point()
# temp_sum %>% ggplot(aes(x=dif_temp, y=mean_temp, color=day_max)) + geom_point()

LETSDOTHIS <- LETSDOTHIS %>% mutate(dpi=case_when(
  dpc == 11 ~ 10, 
  dpc == 8  ~  7, 
  dpc == 5  ~  4,
  dpc == 3  ~  2,
  
  TRUE      ~  as.numeric(dpc)
))

# these summary stas were calculated excluding the mock and prrsvsed pigs
FS17_sum <- LETSDOTHIS %>% filter(!(Trt %in% c('Mock', 'PRRSVsed'))) %>% 
  group_by(fever_class, dpc, cell_type) %>% 
  summarise(mean_p = mean(percentage), 
            num_obs = n(), 
            std_dev = sd(percentage), 
            std_err = std_dev/sqrt(num_obs))


# If using the block must switch to dpi from dpc
FS17_sum <- LETSDOTHIS %>% filter(!(Trt %in% c('Mock'))) %>%
  group_by(fever_class, dpi, cell_type) %>%
  summarise(mean_p = mean(percentage),
            num_obs = n(),
            std_dev = sd(percentage),
            std_err = std_dev/sqrt(num_obs))
# # 


##########




# FS17_long$Trt2 <- ifelse(FS17_long$Pig %in% int_pigs, 'weird', FS17_long$Trt)
# 
# FS17_long %>% group_by(Trt, Trt2) %>% tally()
# 
# FS17_sum <- FS17_long %>%
#   group_by(dpc, Trt2, cell_type) %>% 
#   summarise(mean_p = mean(percentage), 
#             num_obs = n(), 
#             std_dev = sd(percentage), 
#             std_err = std_dev/sqrt(num_obs))

FS17_sum %>%
  ggplot(aes(x=dpc, y=mean_p, fill=fever_class)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  facet_wrap(~cell_type, scales = 'free')

FS17_sum %>%
  ggplot(aes(x=dpi, y=mean_p, fill=fever_class)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  facet_wrap(~cell_type, scales = 'free')



# 
# FS17_sum %>% filter(cell_type == 'CD4pCD8ap') %>% 
#   ggplot(aes(x=dpc, y=mean_p, fill=fever_class)) +
#   geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
#   facet_wrap(~cell_type, scales = 'free')
# # 
# FS17_sum %>% filter(dpc != 11) %>% 
#   ggplot(aes(x=dpc, y=mean_p, fill=fever_class)) +
#   geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
#   facet_wrap(~cell_type, scales = 'free')
# 

##

FS17_sum %>% filter(cell_type == 'CD4pCD8ap') %>% 
  #filter(!Trt2 %in% c('Mock', 'PRRSVsed')) %>% 
  ggplot(aes(x=dpc, y=mean_p, fill=fever_class, color=fever_class)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  geom_point(shape=21)+geom_path()+
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw() + ylab('percent') + xlab('days post challenge')


FS17_sum %>% filter(cell_type == 'CD4pCD8ap') %>% 
  #filter(!Trt2 %in% c('Mock', 'PRRSVsed')) %>% 
  ggplot(aes(x=dpi, y=mean_p, fill=fever_class, color=fever_class)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  geom_point(shape=21)+geom_path()+
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw() + ylab('percent') + xlab('days post challenge')



###########

FS17_sum %>% filter(cell_type == 'CD172p') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=20), size=3)+
  facet_wrap(~dpc) + ggtitle('CD172p')


FS17_sum %>% filter(cell_type == 'CD172p') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=20), size=3)+
  facet_wrap(~dpi) + ggtitle('CD172p')


#############

FS17_sum %>% filter(cell_type == 'CD21p') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=2), size=3)+
  facet_wrap(~dpc) + ggtitle('CD21p')


FS17_sum %>% filter(cell_type == 'CD21p') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=2), size=3)+
  facet_wrap(~dpi) + ggtitle('CD21p')


#############

FS17_sum %>% filter(cell_type == 'CD3p') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=5), size=3)+
  facet_wrap(~dpc) + ggtitle('CD3p')


FS17_sum %>% filter(cell_type == 'CD3p') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=5), size=3)+
  facet_wrap(~dpi) + ggtitle('CD3p')


############


FS17_sum %>% filter(cell_type == 'CD4nCD8ap') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=1), size=3)+
  facet_wrap(~dpc) + ggtitle('CD4nCD8ap')


FS17_sum %>% filter(cell_type == 'CD4nCD8ap') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=1), size=3)+
  facet_wrap(~dpi) + ggtitle('CD4nCD8ap')

#############


FS17_sum %>% filter(cell_type == 'CD4pCD8an') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=1), size=3)+
  facet_wrap(~dpc)  + ggtitle('CD4pCD8an')



FS17_sum %>% filter(cell_type == 'CD4pCD8an') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=1), size=3)+
  facet_wrap(~dpi)  + ggtitle('CD4pCD8an')

#############

##############


FS17_sum %>% filter(cell_type == 'CD4pCD8ap') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=.5), size=3)+
  facet_wrap(~dpc) + ggtitle('CD4pCD8ap')



FS17_sum %>% filter(cell_type == 'CD4pCD8ap') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=.5), size=3)+
  facet_wrap(~dpi) + ggtitle('CD4pCD8ap')


##############


FS17_sum %>% filter(cell_type == 'gdTCRn') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=5), size=3)+
  facet_wrap(~dpc)+ ggtitle('gdTCRn')


FS17_sum %>% filter(cell_type == 'gdTCRn') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=5), size=3)+
  facet_wrap(~dpi)+ ggtitle('gdTCRn')


#############

##############


FS17_sum %>% filter(cell_type == 'gdTCRp') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=.75), size=3)+
  facet_wrap(~dpc)+ ggtitle('gdTCRp')


FS17_sum %>% filter(cell_type == 'gdTCRp') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=.75), size=3)+
  facet_wrap(~dpi)+ ggtitle('gdTCRp')




library(broom)

#### tests for d7 and d10 all cell types anova with Tukey

tests <- LETSDOTHIS %>% filter(!(Trt %in% c('Mock', 'PRRSVsed'))) %>% 
  ungroup() %>% 
  filter(dpc %in% c(7,10)) %>% group_by(dpc, cell_type) %>% 
  nest() %>% 
  mutate(anova=map(data, ~ aov(data=., formula = percentage ~ fever_class)), 
         tid_ano = map(anova, tidy), 
         tuk = map(anova, TukeyHSD), 
         tid_tuk = map(tuk, tidy)) %>% 
  select(dpc, cell_type, tid_tuk) %>% 
  unnest(cols = c('tid_tuk')) %>%
  mutate(p.adj = p.adjust(adj.p.value, method = 'fdr')) %>% 
  filter(p.adj < 0.05)



tests <- LETSDOTHIS %>% filter(!(Trt %in% c('Mock'))) %>% 
  ungroup() %>% 
  filter(dpi %in% c(7,10)) %>% group_by(dpi, cell_type) %>% 
  nest() %>% 
  mutate(anova=map(data, ~ aov(data=., formula = percentage ~ fever_class)), 
         tid_ano = map(anova, tidy), 
         tuk = map(anova, TukeyHSD), 
         tid_tuk = map(tuk, tidy)) %>% 
  select(dpi, cell_type, tid_tuk) %>% 
  unnest(cols = c('tid_tuk')) %>%
  mutate(p.adj = p.adjust(adj.p.value, method = 'fdr')) %>% 
  filter(p.adj < 0.05)





######### pairwise ttest ########



LETSDOTHIS %>% filter(!(Trt %in% c('Mock', 'PRRSVsed'))) %>% 
  ungroup() %>% 
  filter(dpc %in% c(7,10)) %>% group_by(dpc, cell_type) %>% 
  nest() %>% 
  mutate(ttest=map(data, ~ pairwise.t.test(x = .$percentage, g = .$fever_class, p.adjust.method = 'none')), 
         tid_tt = map(ttest, tidy)) %>% 
  select(dpc, cell_type, tid_tt) %>% 
  unnest(cols = c('tid_tt')) %>% 
  ungroup() %>% 
  mutate(p.adj=p.adjust(p.value, method = 'fdr')) %>% 
  filter(p.adj < 0.055)






######## all timepoints ##########
# with 2nd pval correct

tests <- LETSDOTHIS %>% filter(!(Trt %in% c('Mock', 'PRRSVsed'))) %>% 
  ungroup() %>% 
  group_by(dpc, cell_type) %>% 
  nest() %>% 
  mutate(anova=map(data, ~ aov(data=., formula = percentage ~ fever_class)), 
         tid_ano = map(anova, tidy), 
         tuk = map(anova, TukeyHSD), 
         tid_tuk = map(tuk, tidy)) %>% 
  select(dpc, cell_type, tid_tuk) %>% 
  unnest(cols = c('tid_tuk')) %>%
  mutate(p.adj = p.adjust(adj.p.value, method = 'fdr')) %>% 
  filter(p.adj < 0.05)



# with 2nd pval correct

tests <- LETSDOTHIS %>% filter(!(Trt %in% c('Mock'))) %>% 
  ungroup() %>% 
  group_by(dpi, cell_type) %>% 
  nest() %>% 
  mutate(anova=map(data, ~ aov(data=., formula = percentage ~ fever_class)), 
         tid_ano = map(anova, tidy), 
         tuk = map(anova, TukeyHSD), 
         tid_tuk = map(tuk, tidy)) %>% 
  select(dpi, cell_type, tid_tuk) %>% 
  unnest(cols = c('tid_tuk')) %>%
  mutate(p.adj = p.adjust(adj.p.value, method = 'fdr')) %>% 
  filter(p.adj < 0.05)



# all timepoints without 2nd pval correct


tests <- LETSDOTHIS %>% filter(!(Trt %in% c('Mock', 'PRRSVsed'))) %>% 
  ungroup() %>% 
  group_by(dpc, cell_type) %>% 
  nest() %>% 
  mutate(anova=map(data, ~ aov(data=., formula = percentage ~ fever_class)), 
         tid_ano = map(anova, tidy), 
         tuk = map(anova, TukeyHSD), 
         tid_tuk = map(tuk, tidy)) %>% 
  select(dpc, cell_type, tid_tuk) %>% 
  unnest(cols = c('tid_tuk')) %>% filter(adj.p.value < 0.05)



tests <- LETSDOTHIS %>% filter(!(Trt %in% c('Mock'))) %>% 
  ungroup() %>% 
  group_by(dpi, cell_type) %>% 
  nest() %>% 
  mutate(anova=map(data, ~ aov(data=., formula = percentage ~ fever_class)), 
         tid_ano = map(anova, tidy), 
         tuk = map(anova, TukeyHSD), 
         tid_tuk = map(tuk, tidy)) %>% 
  select(dpi, cell_type, tid_tuk) %>% 
  unnest(cols = c('tid_tuk')) %>% filter(adj.p.value < 0.05)


sigs <- unique(tests$cell_type)




####### SIGS WITH PRRSVsed  ##########

FS17_sum %>% filter(cell_type == 'CD4pCD8ap') %>% 
  #filter(!Trt2 %in% c('Mock', 'PRRSVsed')) %>% 
  ggplot(aes(x=dpi, y=mean_p, fill=fever_class, color=fever_class)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  geom_point(shape=21)+geom_path()+
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw() + ylab('percent') + xlab('days post challenge')



FS17_sum %>% filter(cell_type == 'CD4pCD8an') %>% 
  #filter(!Trt2 %in% c('Mock', 'PRRSVsed')) %>% 
  ggplot(aes(x=dpi, y=mean_p, fill=fever_class, color=fever_class)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  geom_point(shape=21)+geom_path()+
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw() + ylab('percent') + xlab('days post challenge')


FS17_sum %>% filter(cell_type == 'CD172p') %>% 
  #filter(!Trt2 %in% c('Mock', 'PRRSVsed')) %>% 
  ggplot(aes(x=dpi, y=mean_p, fill=fever_class, color=fever_class)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  geom_point(shape=21)+geom_path()+
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw() + ylab('percent') + xlab('days post challenge')



FS17_sum %>% filter(cell_type == 'Monocytes') %>% 
  #filter(!Trt2 %in% c('Mock', 'PRRSVsed')) %>% 
  ggplot(aes(x=dpi, y=mean_p, fill=fever_class, color=fever_class)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  geom_point(shape=21)+geom_path()+
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw() + ylab('percent') + xlab('days post challenge')



  

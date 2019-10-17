library(tidyverse)
library(broom)
library(pracma)
FS17 <- read.csv(file = "./data/FS17 Flow Results All.csv", header = T, stringsAsFactors = F)



FS17$date
# because the dpc doesnt match up im trying out of Day Of Study time variable
FS17$DOS <- as.numeric(sub('-Feb','',FS17$date)) - 14
FS17 <- FS17[,c(1:6, 18, 7:17)] # this moves the DOS variable up with the metadata
### select columns... ###

FS17_subset <- FS17 %>% select(-FlowID)

## Maybe try this instead...
# keep as much metadata as possible. 
# the -c(1:7) is telling the gather function to not gather up the metadata
# or another way to think of it is to gather up everyhitng except for cols 1:6

FS17_long <- FS17_subset %>% gather(key='cell_type', value = 'value', -c(1:7))


# turned your ggplot call into a little function

kristen_plots <- function(data, date_){
  data %>% 
    filter(date == date_) %>% 
    ggplot(aes(x=Trt,y=value)) + 
    geom_boxplot() +
    geom_jitter(aes(color=Trt)) +
    theme(axis.text.x=element_text(angle = -45, hjust =0)) +
    facet_wrap( ~ cell_type, scales = "free_y") + 
    ggtitle(date_)
}

kristen_plots(FS17_long, "14-Feb" )


plots <- list()
for (date in unique(FS17$date)){
  print(date)
  plots[[date]] <- kristen_plots(FS17_long, date)
}

plots

# all of these return the first plot in the list
plots[[1]]
plots[['14-Feb']]
plots$`14-Feb`





## the above graphs allow us to look at treatment groups for each day, but would like to follow
## individual pigs and treatment groups across time within the same graphs
## this webpage might be helpful... uses spaghettie plots for longitudinal data 
## https://stats.idre.ucla.edu/r/faq/how-can-i-visualize-longitudinal-data-in-ggplot2/
# setwd("~/Desktop/FS17 Flow FCS Files/")
# library(tidyverse) ##tidyverse contains ggplot2, dplyr, tidr, readr
# library(methods)

# FS17 <- read.csv(file = "./data/FS17 Flow Results All.csv", header = T, stringsAsFactors = F)
# head(FS17)


FS17 %>% select(-FlowID) %>% 
  gather(key = 'CellType', value = 'Percentage', -c(1:7)) %>% 
  ggplot(aes(dpc, Percentage)) +
  geom_smooth(method = 'loess', aes(fill = Trt, color=Trt)) +
  facet_wrap( ~ CellType, scales = "free_y")

##attempting to look at individual pigs over time
FS17 %>% select(-FlowID) %>% 
  gather(key = 'CellType', value = 'Percentage', -c(1:7)) %>% ##moving to long format
  filter(Trt == "Mock") %>%
  ggplot(aes(dpc, Percentage)) +
  geom_line(aes(group = Pig, color=Pig)) +
  facet_wrap( ~ CellType, scales = "free_y")

# JULES NOTE
# this looks weird because you need to make your pig variable a factor or a character


##########

# This may be what you were looking for.
FS17 %>% select(-FlowID) %>% 
  gather(key = 'CellType', value = 'Percentage', -c(1:7)) %>%
  mutate(Pig = factor(Pig)) %>%   # Here I make the Pig variable a factor
  filter(Trt == "Mock") %>%
  ggplot(aes(dpc, Percentage)) +
  geom_line(aes(group = Pig, color=Pig)) +
  facet_wrap( ~ CellType, scales = "free_y") + theme_bw()




######################################################

FS17_clean <- FS17 %>%
  select(-FlowID) %>%
  mutate(Pig=as.character(Pig))

FS17_long <- FS17_clean %>%
  gather(key='cell_type', value = 'percentage', -c(1:7))



# summary statistics, mean and stderr for each combination of celltypes treatments and times
FS17_sum <- FS17_long %>%
  group_by(DOS, Trt, cell_type) %>% 
  summarise(mean_p = mean(percentage), 
            num_obs = n(), 
            std_dev = sd(percentage), 
            std_err = std_dev/sqrt(num_obs))


FS17_sum %>% 
  ggplot(aes(x=DOS, y=mean_p, color=Trt, fill = Trt)) +
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax = mean_p + std_err), alpha = .3) +
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw()

# FS17_sum$dpc





#### CD21pos cells seem lower in all animals that got PRRSVs
FS17_sum %>% 
  filter(cell_type == 'CD21p') %>% 
  ggplot(aes(x=DOS, y=mean_p, color=Trt, fill = Trt)) +
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax = mean_p + std_err), alpha = .3) +
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw()


#### double pos cells seem lower in all animals that got PRRSV

FS17_sum %>% 
  filter(cell_type == 'CD4pCD8ap') %>% 
  ggplot(aes(x=DOS, y=mean_p, color=Trt, fill = Trt)) +
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax = mean_p + std_err), alpha = .3) +
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw()



# no obvious treatment effect....

FS17_sum %>% 
  filter(Trt %in% c('inBCG-Ch', 'lvBCG-Ch', 'noBCG-Ch')) %>% 
  ggplot(aes(x=DOS, y=mean_p, color=Trt, fill = Trt)) +
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax = mean_p + std_err), alpha = .3) +
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw()

# Statistical tests for treatment differences in each cell_type at each timepoint
# this one is conservative because it corrects the allready corrected TukeyHSD pvalues
treat_tests <- FS17_long %>%
  filter(Trt %in% c('inBCG-Ch', 'lvBCG-Ch', 'noBCG-Ch')) %>%
  group_by(DOS, cell_type) %>% 
  nest() %>% 
  mutate(anova=map(data, ~ aov(data=., formula = percentage ~ Trt)), 
         tid_ano = map(anova, tidy), 
         tuk = map(anova, TukeyHSD), 
         tid_tuk = map(tuk, tidy))%>% 
  select(DOS, cell_type, tid_tuk) %>% 
  unnest(cols = c('tid_tuk')) %>%
  mutate(p.adj = p.adjust(adj.p.value, method = 'fdr')) %>% 
  filter(p.adj < 0.05)

# These are the significant differences among treatment groups...
treat_tests

###################

# this one is liberal as it does not correct for the multiple anovas done
# but the pvalues are already corrected within each comparison
# probably not the one to use...

FS17_long %>%
  filter(Trt %in% c('inBCG-Ch', 'lvBCG-Ch', 'noBCG-Ch')) %>%
  group_by(DOS, cell_type) %>% 
  nest() %>% 
  mutate(anova=map(data, ~ aov(data=., formula = percentage ~ Trt)), 
         tid_ano = map(anova, tidy), 
         tuk = map(anova, TukeyHSD), 
         tid_tuk = map(tuk, tidy))%>% 
  select(DOS, cell_type, tid_tuk) %>% 
  unnest(cols = c('tid_tuk')) %>%
  filter(adj.p.value < 0.05)


##############
# This batch of tests includes all treatments, including the Mocks
# this one is conservative because it corrects the allready corrected TukeyHSD pvalues

ALL_tests <- FS17_long %>%
  filter(DOS !=0) %>%
  group_by(DOS, cell_type) %>% 
  nest() %>% 
  mutate(anova=map(data, ~ aov(data=., formula = percentage ~ Trt)), 
         tid_ano = map(anova, tidy), 
         tuk = map(anova, TukeyHSD), 
         tid_tuk = map(tuk, tidy))%>% 
  select(DOS, cell_type, tid_tuk) %>% 
  unnest(cols = c('tid_tuk')) %>%
  mutate(p.adj = p.adjust(adj.p.value, method = 'fdr')) %>% 
  filter(p.adj < 0.05)

ALL_tests
#####






###########
##### spaghetti plots for each treatment
## shows variation within treatments
FS17_long %>% 
  filter(Trt == 'PRRSVsed') %>% 
  ggplot(aes(x=DOS, y=percentage, color = Pig)) + geom_line() + 
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw()

FS17_long %>% 
  filter(Trt == 'inBCG-Ch') %>% 
  ggplot(aes(x=DOS, y=percentage, color = Pig)) + geom_line() + 
  facet_wrap(~cell_type, scales = 'free')+ 
  theme_bw()

FS17_long %>% 
  filter(Trt == 'lvBCG-Ch') %>% 
  ggplot(aes(x=DOS, y=percentage, color = Pig)) + geom_line() + 
  facet_wrap(~cell_type, scales = 'free')+ 
  theme_bw()

FS17_long %>% 
  filter(Trt == 'noBCG-Ch') %>% 
  ggplot(aes(x=DOS, y=percentage, color = Pig)) + geom_line() + 
  facet_wrap(~cell_type, scales = 'free')+ 
  theme_bw()

FS17_long %>% 
  filter(Trt == 'Mock') %>% 
  ggplot(aes(x=DOS, y=percentage, color = Pig)) + geom_line() + 
  facet_wrap(~cell_type, scales = 'free')+ 
  theme_bw()




######## temperature data  ###########



temps <- read_csv('data/FS17 Obj2 Temp Data.csv')
temps$pig <- as.character(temps$pig)


# mean temp for Mock group is 39.8
temps %>% group_by(Trt) %>% summarise(meanT=mean(TempC))


### Looking at the temperatures of the groups over time. 
### Can see a clear difference between the Mocks and the others
temps %>% ggplot(aes(x=dpi, y=TempC)) + 
  geom_line(aes(group = pig)) +
  facet_wrap(~Trt)+ geom_hline(yintercept = 39.8, color='red')


### NEED TO FIX THIS ###
# same pig 2x measurements per day #

temps %>% 
  group_by(pig, dpi) %>%
  tally() %>%
  filter(n != 1)

temps$pig_day <- paste(temps$pig, temps$dpi) # this makes it easier to fix the duplicates

weird_ones <- temps %>% 
  group_by(pig_day) %>%
  tally() %>%
  filter(n != 1) %>% 
  select(pig_day) %>% 
  unlist()

weird_ones

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

# temps %>%
#   group_by(dpi, Trt) %>%tally() #%>% filter(n<14)

## take another look at the data after fixing those issues...
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

# calculate the mean temps for each group
temps %>% group_by(Trt) %>% summarise(meanTemp=mean(TempRmin))

# plot the temps, differences from the mock are more pronounced
temps %>% ggplot(aes(x=dpi, y=TempRmin))+ geom_line(aes(group = pig)) +
  geom_hline(yintercept = 0.418, color='red')+
  facet_wrap(~Trt)

# looks like everyone with PRRSV has:
# higher peak temperatures, 
# higher mean temperatures
# higher variability in temperatures


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
# max temp and mean temp are correlated
# pigs with higher max and mean temps also seem to have higher variability in their
# temperatures
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



#### FEVER SCORE CALC HERE ####
  

# fever score is a combination of:
# max_temp to capture the highest the temperature spiked
# mean_temp to capture the overall elevation of temperature
# sd_temp to capture the jumpiness or swings in temperature 
# all are scaled so that each portion contributes equally to fever score

scaled_temps <- temp_sum %>% mutate_if(is.numeric, scale) # centers and scales variables

scaled_temps <- scaled_temps %>% mutate(fever_score=max_temp+mean_temp+sd_temp)

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




scaled_temps %>%
  ggplot(aes(x=Trt, y=fever_score, color=fever_class)) +
  geom_jitter(width = .2) + theme_bw()

scaled_temps %>%
  ggplot(aes(x=fever_class, y=fever_score, color=fever_score)) +
  geom_jitter() + theme_bw()


LETSDOTHIS <- scaled_temps %>%
  select(Pig, fever_class, fever_score) %>%
  right_join(FS17_long) %>% filter(Pig !=517)  # removing pig 517, few data points

unique(LETSDOTHIS$dpc)

# 
# these summary stats were calculated excluding the mock and prrsvsed pigs
FS17_sum_1 <- LETSDOTHIS %>% filter(!(Trt %in% c('Mock', 'PRRSVsed'))) %>% 
  group_by(fever_class, dpc, cell_type) %>% 
  summarise(mean_p = mean(percentage), 
            num_obs = n(), 
            std_dev = sd(percentage), 
            std_err = std_dev/sqrt(num_obs))

# these summary stats were calculated leaving the PRRSVsed pigs in, but excluding the mocks
# If using the block must switch to DOS from dpc
FS17_sum_2 <- LETSDOTHIS %>% filter(!(Trt %in% c('Mock'))) %>%
  group_by(fever_class, DOS, cell_type) %>%
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

FS17_sum_1 %>%
  ggplot(aes(x=dpc, y=mean_p, fill=fever_class)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  facet_wrap(~cell_type, scales = 'free')

FS17_sum_2 %>%
  ggplot(aes(x=DOS, y=mean_p, fill=fever_class)) +
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

FS17_sum_1 %>% filter(cell_type == 'CD4pCD8ap') %>% 
  #filter(!Trt2 %in% c('Mock', 'PRRSVsed')) %>% 
  ggplot(aes(x=dpc, y=mean_p, fill=fever_class, color=fever_class)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  geom_point(shape=21)+geom_path()+
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw() + ylab('percent') + xlab('days post challenge')


FS17_sum_2 %>% filter(cell_type == 'CD4pCD8ap') %>% 
  #filter(!Trt2 %in% c('Mock', 'PRRSVsed')) %>% 
  ggplot(aes(x=DOS, y=mean_p, fill=fever_class, color=fever_class)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  geom_point(shape=21)+geom_path()+
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw() + ylab('percent') + xlab('day of study')



###########

FS17_sum_1 %>% filter(cell_type == 'CD172p') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=20), size=3)+
  facet_wrap(~dpc) + ggtitle('CD172p')


FS17_sum_2 %>% filter(cell_type == 'CD172p') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=20), size=3)+
  facet_wrap(~DOS) + ggtitle('CD172p')


#############

FS17_sum_1 %>% filter(cell_type == 'CD21p') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=2), size=3)+
  facet_wrap(~dpc) + ggtitle('CD21p')


FS17_sum_2 %>% filter(cell_type == 'CD21p') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=2), size=3)+
  facet_wrap(~DOS) + ggtitle('CD21p')


#############

FS17_sum_1 %>% filter(cell_type == 'CD3p') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=5), size=3)+
  facet_wrap(~dpc) + ggtitle('CD3p')


FS17_sum_2 %>% filter(cell_type == 'CD3p') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=5), size=3)+
  facet_wrap(~DOS) + ggtitle('CD3p')


############


FS17_sum_1 %>% filter(cell_type == 'CD4nCD8ap') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=1), size=3)+
  facet_wrap(~dpc) + ggtitle('CD4nCD8ap')


FS17_sum_2 %>% filter(cell_type == 'CD4nCD8ap') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=1), size=3)+
  facet_wrap(~DOS) + ggtitle('CD4nCD8ap')

#############


FS17_sum_1 %>% filter(cell_type == 'CD4pCD8an') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=1), size=3)+
  facet_wrap(~dpc)  + ggtitle('CD4pCD8an')



FS17_sum_2 %>% filter(cell_type == 'CD4pCD8an') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=1), size=3)+
  facet_wrap(~DOS)  + ggtitle('CD4pCD8an')

#############

##############


FS17_sum_1 %>% filter(cell_type == 'CD4pCD8ap') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=.5), size=3)+
  facet_wrap(~dpc) + ggtitle('CD4pCD8ap')



FS17_sum_2 %>% filter(cell_type == 'CD4pCD8ap') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=.5), size=3)+
  facet_wrap(~DOS) + ggtitle('CD4pCD8ap')


##############


FS17_sum_1 %>% filter(cell_type == 'gdTCRn') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=5), size=3)+
  facet_wrap(~dpc)+ ggtitle('gdTCRn')


FS17_sum_2 %>% filter(cell_type == 'gdTCRn') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=5), size=3)+
  facet_wrap(~DOS)+ ggtitle('gdTCRn')


#############

##############


FS17_sum_1 %>% filter(cell_type == 'gdTCRp') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=.75), size=3)+
  facet_wrap(~dpc)+ ggtitle('gdTCRp')


FS17_sum_2 %>% filter(cell_type == 'gdTCRp') %>% 
  ggplot(aes(x=fever_class, y=mean_p, fill=fever_class)) +
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin=mean_p - std_err,
                    ymax=mean_p + std_err), 
                width = .2) +
  geom_text(aes(label=num_obs, y=.75), size=3)+
  facet_wrap(~DOS)+ ggtitle('gdTCRp')




#### tests for d7 and d10 all cell types anova with Tukey
# 
# tests <- LETSDOTHIS %>% filter(!(Trt %in% c('Mock', 'PRRSVsed'))) %>% 
#   ungroup() %>% 
#   filter(dpc %in% c(7,10)) %>% group_by(dpc, cell_type) %>% 
#   nest() %>% 
#   mutate(anova=map(data, ~ aov(data=., formula = percentage ~ fever_class)), 
#          tid_ano = map(anova, tidy), 
#          tuk = map(anova, TukeyHSD), 
#          tid_tuk = map(tuk, tidy)) %>% 
#   select(dpc, cell_type, tid_tuk) %>% 
#   unnest(cols = c('tid_tuk')) %>%
#   mutate(p.adj = p.adjust(adj.p.value, method = 'fdr')) %>% 
#   filter(p.adj < 0.05)
# 
# 
# 
# tests <- LETSDOTHIS %>% filter(!(Trt %in% c('Mock'))) %>% 
#   ungroup() %>% 
#   filter(DOS %in% c(8,11)) %>% group_by(DOS, cell_type) %>% 
#   nest() %>% 
#   mutate(anova=map(data, ~ aov(data=., formula = percentage ~ fever_class)), 
#          tid_ano = map(anova, tidy), 
#          tuk = map(anova, TukeyHSD), 
#          tid_tuk = map(tuk, tidy)) %>% 
#   select(DOS, cell_type, tid_tuk) %>% 
#   unnest(cols = c('tid_tuk')) %>%
#   mutate(p.adj = p.adjust(adj.p.value, method = 'fdr')) %>% 
#   filter(p.adj < 0.05)
# 
# 
# 
# tests
######### pairwise ttest ########
# 
# 
# 
# LETSDOTHIS %>% filter(!(Trt %in% c('Mock', 'PRRSVsed'))) %>% 
#   ungroup() %>% 
#   filter(dpc %in% c(7,10)) %>% group_by(dpc, cell_type) %>% 
#   nest() %>% 
#   mutate(ttest=map(data, ~ pairwise.t.test(x = .$percentage, g = .$fever_class, p.adjust.method = 'none')), 
#          tid_tt = map(ttest, tidy)) %>% 
#   select(dpc, cell_type, tid_tt) %>% 
#   unnest(cols = c('tid_tt')) %>% 
#   ungroup() %>% 
#   mutate(p.adj=p.adjust(p.value, method = 'fdr')) %>% 
#   filter(p.adj < 0.05)
# 





######## all timepoints ##########
# EXCLUDING PRRSVSED PIGS
# with 2nd pval correct

tests <- LETSDOTHIS %>% filter(!(Trt %in% c('Mock', 'PRRSVsed'))) %>%
  filter(DOS !=0) %>% 
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

tests

## INCLUDING PRRSVSED pigs BUT NOT MOCK PIGS
# with 2nd pval correct

tests <- LETSDOTHIS %>% filter(!(Trt %in% c('Mock'))) %>% 
  filter(DOS !=0) %>% 
  ungroup() %>% 
  group_by(DOS, cell_type) %>% 
  nest() %>% 
  mutate(anova=map(data, ~ aov(data=., formula = percentage ~ fever_class)), 
         tid_ano = map(anova, tidy), 
         tuk = map(anova, TukeyHSD), 
         tid_tuk = map(tuk, tidy)) %>% 
  select(DOS, cell_type, tid_tuk) %>% 
  unnest(cols = c('tid_tuk')) %>%
  mutate(p.adj = p.adjust(adj.p.value, method = 'fdr')) %>% 
  filter(p.adj < 0.05)


tests
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
  group_by(DOS, cell_type) %>% 
  nest() %>% 
  mutate(anova=map(data, ~ aov(data=., formula = percentage ~ fever_class)), 
         tid_ano = map(anova, tidy), 
         tuk = map(anova, TukeyHSD), 
         tid_tuk = map(tuk, tidy)) %>% 
  select(DOS, cell_type, tid_tuk) %>% 
  unnest(cols = c('tid_tuk')) %>% filter(adj.p.value < 0.05)


sigs <- unique(tests$cell_type)




####### SIGS WITH PRRSVsed  ##########

FS17_sum_2 %>% filter(cell_type == 'CD4pCD8ap') %>% 
  #filter(!Trt2 %in% c('Mock', 'PRRSVsed')) %>% 
  ggplot(aes(x=DOS, y=mean_p, fill=fever_class, color=fever_class)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  geom_point(shape=21)+geom_path()+
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw() + ylab('percent') + xlab('Day of study')



FS17_sum_2 %>% filter(cell_type == 'CD4pCD8an') %>% 
  #filter(!Trt2 %in% c('Mock', 'PRRSVsed')) %>% 
  ggplot(aes(x=DOS, y=mean_p, fill=fever_class, color=fever_class)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  geom_point(shape=21)+geom_path()+
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw() + ylab('percent') + xlab('Day of study')


FS17_sum_2 %>% filter(cell_type == 'CD172p') %>% 
  #filter(!Trt2 %in% c('Mock', 'PRRSVsed')) %>% 
  ggplot(aes(x=DOS, y=mean_p, fill=fever_class, color=fever_class)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  geom_point(shape=21)+geom_path()+
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw() + ylab('percent') + xlab('Day of study')



FS17_sum_2 %>% filter(cell_type == 'Monocytes') %>% 
  #filter(!Trt2 %in% c('Mock', 'PRRSVsed')) %>% 
  ggplot(aes(x=DOS, y=mean_p, fill=fever_class, color=fever_class)) +
  geom_ribbon(aes(ymin=mean_p - std_err, ymax=mean_p + std_err), alpha=.5) +
  geom_point(shape=21)+geom_path()+
  facet_wrap(~cell_type, scales = 'free') + 
  theme_bw() + ylab('percent') + xlab('Day of study')

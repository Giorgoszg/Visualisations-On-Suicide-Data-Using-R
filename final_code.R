# Load data 
suicides <-  read.csv("C:\\...\\suicideOED.csv")
suicides$Flag.Codes<- NULL
#suicides$INDICATOR <- NULL
#suicides$FREQUENCY <- NULL
#suicides$SUBJECT <- NULL

# Loading
library("readxl")
# xlsx files
my_data <- read_excel("C:/../suicide1.xls" , skip = 2 )

library('tidyverse') # load the tidyverse package

suicides2<-pivot_longer(my_data , cols = c('2011','2012','2013','2014','2015'), names_to = "year" , values_to = "suicides")

suicide_only_countries <- suicides2 %>% filter(`GEO/SEX` != 'European Union (current composition)')

# ---------------------------------------- plot3 ------------------------------------

my_data$`2015_2011_change` <- round((( my_data$`2015` * 100 ) / my_data$`2011` - 100 ),1)
subset_change <- my_data %>% filter(`GEO/SEX` != 'European Union (current composition)') %>% select (c(`GEO/SEX`, `2015_2011_change`))

# median change
subset_change$median_change <- median(subset_change$`2015_2011_change`)


ggplot(subset_change, aes(`GEO/SEX`, `2015_2011_change`)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Set1") + coord_flip()

library('forcats') #load forcats package

class(subset_change$`GEO/SEX`) # a character
subset_change$`GEO/SEX.factor` <- as.factor(subset_change$`GEO/SEX`)
class(subset_change$`GEO/SEX.factor`) # a factor

# We can see the order of items in the factor variable, or the levels, using the levels() function. Think of it this way: the countries in country.factor are "secretly" numbers deep down, even though they look the same as the countries in country.
# levels() tells us which number is assigned to each country in country.factor:

levels(subset_change$`GEO/SEX.factor`)

# We want to reorder the countries in country.factor such that they are ranked according to the both sexes variable. This is also called "releveling" the factor variable. The forcats package, another package included in the tidyverse set of packages, makes it easy to to this using the fct_reorder() function. Let's create another variable, country.factor.reorder, for the reordered factor:

theme_set(theme_bw())

subset_change$`GEO/SEXreorder` <- fct_reorder(subset_change$`GEO/SEX.factor`, # factor variable to reorder
                                              subset_change$`2015_2011_change`) # variable to reorder it by

levels(subset_change$`GEO/SEXreorder`)

plot2 <- ggplot(subset_change, aes(`GEO/SEXreorder`, `2015_2011_change`,)) +
  geom_bar(stat = "identity", position = "dodge",  aes(fill = `2015_2011_change`>0),col = 'transparent') +
  scale_fill_brewer(palette = "Set1") + coord_flip() +
  geom_text(aes(label=`2015_2011_change` ),hjust=1, vjust=0.3, size=3.5) # fix it



plot2 + labs(title="Suicide Rate Overview Change by Country",
             subtitle="Years Period: 2011 to 2015. ",
             caption="Source: Eurostat , 
     Relevant Link : https://ec.europa.eu/eurostat/databrowser/view/tps00122/default/table?lang=en",
             x="Country Name ",
             y=" Precentage Change in Number of Suicides Between those two Years ") +  
  theme(title = element_text(face = "bold.italic", color = "black"),
        axis.title = element_text(face = "bold.italic", color = "gray24")) +
  scale_fill_discrete(guide = 'none') #+ 
geom_hline(yintercept=median(subset_change$`2015_2011_change`), alpha=1, color="black") 

# save it 
ggsave('Plot2_percent_change.png', plot2 + labs(title="Suicide Rate Overview Change by Country",
                                                subtitle="Years Period: 2011 to 2015.",
                                                caption="Source: Eurostat , 
     Relevant Link : https://ec.europa.eu/eurostat/databrowser/view/tps00122/default/table?lang=en",
                                                x="Country Name ",
                                                y=" Precentage Change in Number of Suicides Between those two Years ") +  
         theme(title = element_text(face = "bold.italic", color = "black"),
               axis.title = element_text(face = "bold.italic", color = "gray24")) +
         scale_fill_discrete(guide = 'none'))

# ---------------------------------------------- plot 4 ----------------------------


men_data <-  read.csv("C:\\Users\\gzigo\\Dropbox\\My PC (LAPTOP-LJMC8EBN)\\Downloads\\suicides\\Men_data.csv")
women_data<-  read.csv("C:\\Users\\gzigo\\Dropbox\\My PC (LAPTOP-LJMC8EBN)\\Downloads\\suicides\\Women_data.csv")

names(suicides)[names(suicides) == "ï..LOCATION"] <- "Country"
p <- ggplot(suicides, aes(x = TIME, y = Value, group = Country)) +
  geom_line(color = "steelblue",size = 1) +
  geom_point(color="steelblue") + 
  facet_wrap(~ Country)

p + labs(title="Suicide Rate per 100k Popylation Overview by Country",
         subtitle="Years. Period: 2004-2014.",
         caption="Source: OECD ",
         x="Year",
         y="Number of suicides") + theme(
           strip.background = element_rect(
             color="black", fill=NA, size=1.5, linetype="solid"
           ),
           strip.text.x = element_text(
             size = 12, color = "Blue", face = "bold.italic")
         )
# save it 
ggsave('facet_per_100k.png', p + labs(title="Suicide Rate per 100k Popylation Overview by Country",
                                      subtitle="Years. Period: 2004-2014.",
                                      caption="Source: OECD ",
                                      x="Year",
                                      y="Number of suicides") + theme(
                                        strip.background = element_rect(
                                          color="black", fill=NA, size=1.5, linetype="solid"
                                        ),
                                        strip.text.x = element_text(
                                          size = 12, color = "Blue", face = "bold.italic") ))

# all together useless
p <- ggplot(suicides, aes(x = TIME, y = Value, group = Country)) +
  geom_line(color = "steelblue",size = 1) +
  #geom_line(color = ,size = 1) +
  geom_point(color="steelblue") 
p

# ----------------------------------------- plot 6 men & women -------------------------

total_data <- rbind(men_data,women_data)
names(total_data)[names(total_data) == "ï..LOCATION"] <- "Country"


# Subsetting and reshaping the life expectancy data
subdata <- total_data  %>% filter(TIME == "2014") %>% 
  select(c(Country, SUBJECT, Value)) %>% spread(SUBJECT, Value)

# Taking a look at the first few rows
head(subdata)
# Plotting male and female SUICIDES 2014
ggplot(subdata, aes(x= MEN, y = WOMEN)) + geom_point()
# Adding an abline and changing the scale of axes of the previous plots
ggplot(subdata,  aes(x= MEN, y = WOMEN)) + 
  geom_point() +
  scale_x_continuous(limits=c(0,60)) +
  scale_y_continuous(limits=c(0,10)) 

# Subseting data to obtain countries of interest (top countries)
bottom <- subdata %>% arrange(MEN-WOMEN) %>% head(3)
top <- subdata %>% arrange(WOMEN-MEN) %>% head(3)


# Adding labels to previous plot
ggplot(subdata,  aes(x= MEN, y = WOMEN,label = Country)) + 
  geom_point(colour="white", fill="chartreuse3", shape=21, alpha=.55, size=5)+
  scale_x_continuous(limits=c(6,60),breaks = seq(from = 6, to = 58, by = 2)) +
  scale_y_continuous(limits=c(1,10),breaks = seq(from = 1, to = 10, by = 2)) +
  labs(title="Suicides per 100k population by Country",
       subtitle="Years. Period: 2014.",
       caption="Source: https://data.oecd.org/healthstat/suicide-rates.htm",
       x="Males",
       y="Females")+
  geom_text(data=bottom, size = 3)+
  geom_text(data = top, size=3) +
  annotate(geom="text", x=10, y=1.35, 
           label="Greece has 7.8 suicides for males,
           and 1.9 for females .") + 
  annotate(geom="point", x=7.8, y=1.9, size=10,colour="red", shape=23) +
  theme_bw()

# save it 
ggsave('Plotmenwomen2014_annotated.png',ggplot(subdata,  aes(x= MEN, y = WOMEN,label = Country)) + 
         geom_point(colour="white", fill="chartreuse3", shape=21, alpha=.55, size=5)+
         scale_x_continuous(limits=c(6,60),breaks = seq(from = 6, to = 58, by = 2)) +
         scale_y_continuous(limits=c(1,10),breaks = seq(from = 1, to = 10, by = 2)) +
         labs(title="Suicides per 100k population by Country",
              subtitle="Years. Period: 2014.",
              caption="Source: https://data.oecd.org/healthstat/suicide-rates.htm",
              x="Males",
              y="Females")+
         geom_text(data=bottom, size = 3)+
         geom_text(data = top, size=3) +
         annotate(geom="text", x=10, y=1.35, 
                  label="Greece has 7.8 suicides for males,
           and 1.9 for females .") + 
         annotate(geom="point", x=7.8, y=1.9, size=10,colour="red", shape=23) +
         theme_bw())

library("readxl")

library(ggalt)
library(tidyr)
library(dplyr)
library('tidyverse') # load the tidyverse package
#install.packages('ggalt')

# ----------------------------------------- Plot 1 -----------------------------------

my_data <- read_excel("C:/Users/gzigo/Dropbox/My PC (LAPTOP-LJMC8EBN)/Downloads/suicides/suicide1.xls" , skip = 2 )
suicide_only_countries <- my_data %>% filter(`GEO/SEX` != 'European Union (current composition)')
plotdata_wide <-suicide_only_countries %>% select(`GEO/SEX`,`2011`,`2015`)

# create dumbbell plot
ggplot(plotdata_wide, aes(y = `GEO/SEX`,
                          x = `2011`,
                          xend = `2015`)) +  
  geom_dumbbell()

# create dumbbell plot
ggplot(plotdata_wide, 
       aes(y = reorder(`GEO/SEX`, `2011`),
           x = `2011`,
           xend = `2015`)) +  
  geom_dumbbell(size = 1.2,
                size_x = 3, 
                size_xend = 3,
                colour = "grey", 
                colour_x = "blue", 
                colour_xend = "red",
                dot_guide=TRUE, dot_guide_size=0.25) +
  theme_minimal() + 
  labs(title = "Change in Number of suicides.",
       subtitle = "2011 to 2015",
       caption="Source: Eurostat , 
     Relevant Link : https://ec.europa.eu/eurostat/databrowser/view/tps00122/default/table?lang=en",
       x = "Suicides (count)",
       y = "")  + scale_color_manual(name = "", values = c("red", "blue") )




ggsave('dumbell.png' ,ggplot(plotdata_wide, 
                             aes(y = reorder(`GEO/SEX`, `2011`),
                                 x = `2011`,
                                 xend = `2015`)) +  
         geom_dumbbell(size = 1.2,
                       size_x = 3, 
                       size_xend = 3,
                       colour = "grey", 
                       colour_x = "blue", 
                       colour_xend = "red") +
         theme_minimal() + 
         labs(title = "Change in Number of suicides.",
              subtitle = "2011 to 2015",
              caption="Source: Eurostat , 
     Relevant Link : https://ec.europa.eu/eurostat/databrowser/view/tps00122/default/table?lang=en",
              x = "Suicides (count)",
              y = ""))

# ------------------------------------- plot 5 ---------------------------------
#install.packages('CGPfunctions')
library(CGPfunctions)
# long format
suicides2<-pivot_longer(my_data , cols = c('2011','2012','2013','2014','2015'), names_to = "year" , values_to = "suicides")


# Countries with population from 7 million to 11               

df <- suicides2 %>%
  filter(`GEO/SEX` %in% c("Greece", "Belgium", 
                          "Czech Republic", "Sweden", 
                          "Portugal", "Azerbaijan",
                          "Hungary","Belarus","Austria","Switzerland",
                          "Bulgaria","Serbia")) %>%
  mutate(year = factor(year),
         Suicides = round(suicides)) 

# create slope graph

newggslopegraph(df, year, Suicides, `GEO/SEX`) +
  labs(title="Suicides by Country", 
       subtitle="Countries with population from approximetely , 7 to 11 million people .", 
       caption="source: Suicide Data From Eurostat")   


whole_data <- rbind(suicides,men_data,women_data)
names(whole_data)[names(whole_data) == "ï..LOCATION"] <- "Country"

# ----------------------------------------------------- plot 7 --------------------
names(women_data)[names(women_data) == "ï..LOCATION"] <- "Country"
names(total_data)[names(total_data) == "ï..LOCATION"] <- "Country"
whole_data <- rbind(suicides,total_data,women_data)


whole_data_2014 <- whole_data %>% filter(TIME == 2014)

whole_data_2004 <- whole_data %>% filter(TIME == 2004)

g <- ggplot(whole_data_2014, 
            aes(y = Value,
                x=reorder(Country, Value, function(x){ sum(x) }) ,group = Country))+ 
  geom_point(aes(shape=SUBJECT,color=SUBJECT,size=SUBJECT)) +
  scale_shape_manual(values=c(10, 16, 17))+
  scale_size_manual(values=c(3,5,3))+ 
  scale_color_manual(values=c("red", "blue", "green"))+
  geom_hline(aes(yintercept = 1.9),linetype="dashed",color="green") +
  geom_hline(aes(yintercept = 7.8),linetype="longdash",color="red")


g + theme_bw() + 
  labs(title = "Suicide Rate by Country.",
       subtitle = "Total / Men / Women, Per 100 000 persons, 2014",
       caption="Source: OECD Data ",
       x = "",
       y = " Number of Suicides") +
  theme(legend.position="bottom",title = element_text(face = "bold.italic", color = "black"),
        panel.background = element_rect(fill = "lightblue2",
                                        size = 0.5, linetype = "solid"),
        axis.title = element_text(face = "bold.italic", color = "gray24")) +
  scale_y_continuous(limits = c(0, 56), # minimum and maximum for the x-axis 
                     breaks = seq(from = 0, to = 56, by = 5)) # x-axis labels


# --------------------------------------- Arrow ---------------------------------------------------
suicides2<-pivot_longer(my_data , cols = c('2011','2012','2013','2014','2015'), names_to = "year" , values_to = "suicides")

suicide_only_countries <- suicides2 %>% filter(`GEO/SEX` != 'European Union (current composition)')

suicide_only_countries_2 <- suicide_only_countries %>% filter (year %in% c(2011,2015))
year_colors = c("2011"="blue", "2015"="red")

p1 <- ggplot() +
  geom_point(data=suicide_only_countries_2, 
             aes(y=reorder(`GEO/SEX`, suicides, function(x){ diff(x) }), x=suicides, fill=year),
             size=4, shape=21, color="black") +
  geom_segment(data=plotdata_wide, 
               aes(y=`GEO/SEX`, yend=`GEO/SEX`, x=`2011`, xend=`2015`),
               size=1, color="black",
               lineend="butt", linejoin="mitre",
               arrow=arrow(length = unit(0.008, "npc"), type="closed")) +
  scale_fill_manual(values=year_colors)

p1+   labs(title = "Change in Number of suicides.",
           subtitle = "2011 to 2015",
           caption="Source: Eurostat , 
     Relevant Link : https://ec.europa.eu/eurostat/databrowser/view/tps00122/default/table?lang=en",
           x = "Suicides (count)",
           y = "")  + theme(legend.position="bottom",title = element_text(face = "bold.italic", color = "black"),
                            panel.background = element_rect(fill = "lightblue2",
                                                            size = 0.5, linetype = "solid"),
                            axis.title = element_text(face = "bold.italic", color = "gray24"))



# jitter

library(ggrepel)
my_data <- read_excel("C:/Users/gzigo/Dropbox/My PC (LAPTOP-LJMC8EBN)/Downloads/suicides/SuicideData.xlsx" , skip = 2 )

# gsub : to 0

my_data$`2017`<-gsub(":","",my_data$`2017`)
my_data$`2017`<- as.numeric(my_data$`2017`)
# long format
suicides2<-pivot_longer(my_data[,c(1,6,7,8)] , cols = c('2015','2016','2017'), names_to = "year" , values_to = "suicides")
suicides2<-pivot_longer(my_data , cols = c('2011','2012','2013','2014','2015','2016','2017'), names_to = "year" , values_to = "suicides")

suicides2$name <- suicides2$`GEO (Labels)`

suicides2$name[suicides2$`GEO (Labels)` != 'Greece'] <- ""

plot<-ggplot(suicides2,aes(year,suicides,color = name != "",label=name)) +
  geom_violin(alpha=0.8, color="white")+
  geom_jitter(alpha=0.5, aes(color=`GEO (Labels)`=='Greece'),position = position_jitter(width = .1))+
  scale_y_continuous(limits=c(0,37),breaks = seq(from = 0, to = 40, by = 2))+
  geom_label_repel()


library(ggthemes)

plot + theme_economist() + scale_color_brewer(palette="Set2")+
  labs(title="Suicide Rate per 100k Population .",
            subtitle="Distribution of Countries , 2011-2017.",
            caption="Source: Eurostat ",
            x="",
            y="Number of suicides",color = "Country :\n") + theme(
              strip.background = element_rect(
                color="black", fill=NA, size=1.8, linetype="solid"
              ),
              strip.text.x = element_text(
                size = 12, color = "Blue", face = "bold.italic") ) +
  scale_color_manual(labels = c("Other", "Greece"), values = c("blue", "red")) + 
  theme(legend.position="top",title = element_text(face = "bold.italic",size=15, color = "black"),
        axis.title = element_text(face = "bold.italic", color = "black")) +
  theme(axis.text.x = element_text(angle=45, vjust=0.6)) 


#---------------------------------------------------------

a <- suicides2 %>% group_by(`GEO (Labels)`)
a
a_new <- a %>% summarise(
  suicide = round(mean(suicides,na.rm=TRUE),2))

# find mean 
a_new$performance <- ifelse(a_new$suicide < mean(a_new$suicide), "below", "above")  # above / below avg flag

# test 
ggplot(a_new, aes(x=reorder(`GEO (Labels)`,suicide),y=suicide)) + 
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label=suicide),hjust=0.5, vjust=0.01, size=4.5) + coord_flip()
  

b<-ggplot(a_new, aes(x=reorder(`GEO (Labels)`,suicide),y=suicide,label=suicide)) + 
  geom_point(stat='identity',aes(col=performance), size=6.6)  +
  geom_segment(aes(x = `GEO (Labels)`, 
                   yend = suicide, 
                   xend = `GEO (Labels)`), 
               color = "black") +
  scale_color_manual(name="Suicides number :", 
                     labels = c("Above Average", "Below Average"), 
                     values = c("above"="#00ba38", "below"="#f8766d")) +
  geom_text(color="white", size=2) +
  labs(title="Diverging Lollipop Chart", 
       subtitle="Mean suicide rate for years : 2011-2017",
       caption="Source: Eurostat ",
       x="Country Name ",
       y="Suicides") + 
  coord_flip()

# -------------------- normalise -------------------------------------------

library(ggplot2)
theme_set(theme_bw())  

data2013_14 <- suicide_only_countries %>%
  filter(year %in% c(2013,2014))


# Data Prep
a_new$suicide_z <- round((a_new$suicide - mean(a_new$suicide,na.rm = TRUE))/sd(a_new$suicide), 2)  # compute normalized data
a_new$suicide_type <- ifelse(a_new$suicide_z < 0, "below", "above")  # above / below avg flag

# Diverging Barcharts
a <- ggplot(a_new, aes(x=reorder(`GEO (Labels)`,suicide_z),y=suicide_z,label=suicide_z)) + 
  geom_bar(stat='identity', aes(fill=suicide_type), width=.5)  +
  scale_fill_manual(name="Suicides number :", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised suicide data from 'Eurostat'", 
       title= "Diverging Bars",
       x="Country Name ",
       y="Suicides") + 
  coord_flip()

library(ggpubr)

ggarrange(a, b, 
          labels = c("A", "B"),
          ncol = 2, nrow = 1)

ggsave('combinedrplot.png', ggarrange(a, b, 
                                   labels = c("A", "B"),
                                   ncol = 2, nrow = 1))

# radarplot
library(ggplot2)
library(tibble)
# function provided by Erwan Le Pennec for the radar coord. 
coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

Data<- read_excel("C:/Users/gzigo/Dropbox/My PC (LAPTOP-LJMC8EBN)/Downloads/suicides/SuicideData.xlsx" , skip = 2 )
Data<-Data[-10,]

Data$`2017`<-as.numeric(Data$`2017`)
suicides2<-pivot_longer(Data , cols = c('2011','2012','2013','2014','2015','2016','2017'), names_to = "year" , values_to = "suicides")

# random data plotted in radar plot
ggplot(data = suicides2, aes(x  = year, y = suicides) ) +  theme_light() +
  geom_polygon(aes(group = `GEO (Labels)`, color = `GEO (Labels)`),
               fill  = NA, size = 0.8) +
  facet_wrap(~ `GEO (Labels)`) +
  theme(strip.text.x = element_text(size = rel(1.2)),
        axis.text.x  = element_text(size = rel(0.65)),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_blank()) +
  guides(color = "none")+
  xlab("") + ylab("") +
  coord_radar() +
  labs(title="Radarplot on suicide data by Country",
       subtitle="Years. Period: 2011-2017. ",
       caption="Source: Eurostat")

ggsave('radarplot.png', ggplot(data = suicides2, aes(x  = year, y = suicides) ) +  theme_light() +
         geom_polygon(aes(group = `GEO (Labels)`, color = `GEO (Labels)`),
                      fill  = NA, size = 0.8) +
         facet_wrap(~ `GEO (Labels)`) +
         theme(strip.text.x = element_text(size = rel(1.2)),
               axis.text.x  = element_text(size = rel(0.65)),
               axis.ticks.y = element_blank(),
               axis.text.y  = element_blank()) +
         guides(color = "none")+
         xlab("") + ylab("") +
         coord_radar() +
         labs(title="Radarplot on suicide data by Country",
              subtitle="Years. Period: 2011-2017. ",
              caption="Source: Eurostat"))

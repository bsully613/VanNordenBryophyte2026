
##### SET YOUR WORKING DIRECTORY ----------------------------------------------





##### LOAD PACKAGES -----------------------------------------------------------


library(dplyr)
library(ggplot2)
library(tidyr)
library(lme4)
library(lmerTest)
library(emmeans)
library(cowplot)
library(broom)
library(patchwork)
library(lubridate)
library(ggtext)



##### SOIL PHYSICAL CONDITION -----------------------------------------------------------


### Statistics ###


Data <- read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")


Data <- Data %>%
  mutate(Sample.ID = as.factor(Sample.ID),
         Date = factor(Date, levels = c("6/29/23", "7/6/23", "7/13/23", "7/19/23", "7/26/23", "8/10/23")),
         Block = as.factor(Block),
         Pair = interaction(Block, Collar.Pair, drop = TRUE)) %>%
  filter(Moss.Removal == "Moss Intact",
         Date %in% c("6/29/23", "7/6/23", "7/13/23", "7/19/23", "7/26/23", "8/10/23"))


## Soil Water Content ##


hist(Data$Soil.Water.Content_pct)
# Normally distributed

Soil.Water.Content_lmm <- lmer(Soil.Water.Content_pct ~ Date * Block + (1 | Pair),
                             data = Data)

anova(Soil.Water.Content_lmm)
# Type III Analysis of Variance Table with Satterthwaite's method
#            Sum Sq   Mean Sq   NumDF   DenDF   F value     Pr(>F)    
# Date       6639.4   1327.88   5       60      62.282      < 2.2e-16 ***
# Block      2022.6   1011.28   2       12      47.432      2.005e-06 ***
# Date:Block 7637.9   763.79    10      60      35.824      < 2.2e-16 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

plot(Soil.Water.Content_lmm); qqnorm(resid(Soil.Water.Content_lmm)); qqline(resid(Soil.Water.Content_lmm))

emmeans(Soil.Water.Content_lmm,
        pairwise ~ Date | Block,
        adjust = "tukey")

Data.Summary <- 
  Data %>%
  group_by(Block, Date) %>%
  summarise(
    min = min(Soil.Water.Content_pct),
    max = max(Soil.Water.Content_pct),
    mean = mean(Soil.Water.Content_pct),
    sd = sd(Soil.Water.Content_pct))



## Soil Temperature ##


hist(Data$Soil.Temperature_c)
# Normally distributed

Soil.Temperature_lmm <- lmer(Soil.Temperature_c ~ Date * Block + (1 | Pair),
                           data = Data)

anova(Soil.Temperature_lmm)
# Type III Analysis of Variance Table with Satterthwaite's method
#             Sum Sq    Mean Sq     NumDF   DenDF   F value     Pr(>F)    
# Date        423.81    84.763      5       60      12.5678     2.324e-08 ***
# Block       38.87     19.433      2       12      2.8814      0.095066 .  
# Date:Block  181.89    18.189      10      60      2.6970      0.008471 ** 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

emmeans(Soil.Temperature_lmm,
        pairwise ~ Date | Block,
        adjust = "tukey")

Data.Summary <- 
  Data %>%
  group_by(Block, Date) %>%
  summarise(
    min = min(Soil.Temperature_c),
    max = max(Soil.Temperature_c),
    mean = mean(Soil.Temperature_c),
    sd = sd(Soil.Temperature_c))



### Figure ###


Data<-read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")


Data<-Data %>%
  mutate(Block_Collar.Pair=interaction(Block,Collar.Pair,drop=TRUE),
         Date=as.Date(Date,format="%m/%d/%y"),
         Block=as.factor(Block),
         Collar.Pair=as.factor(Collar.Pair),
         Sample.ID=as.factor(Sample.ID)
  ) %>%
  filter(Moss.Removal=="Moss Intact",
         Date %in% as.Date(c("6/29/23","7/6/23","7/13/23","7/19/23","7/26/23","8/10/23"),
                           format="%m/%d/%y"))


Data<-Data %>%
  group_by(Date,Block) %>%
  summarise(
    Soil.Water.Content=mean(Soil.Water.Content_pct,na.rm=TRUE),
    Soil.Temperature=mean(Soil.Temperature_c,na.rm=TRUE),
    Soil.Water.Content_n=n_distinct(Sample.ID),
    Soil.Temperature_n=n_distinct(Sample.ID),
    Soil.Water.Content_sd=sd(Soil.Water.Content_pct,na.rm=TRUE),
    Soil.Temperature_sd=sd(Soil.Temperature_c,na.rm=TRUE),
    Soil.Water.Content_se=Soil.Water.Content_sd/sqrt(Soil.Water.Content_n),
    Soil.Temperature_se=Soil.Temperature_sd/sqrt(Soil.Temperature_n),
    .groups="drop"
  )


Dates<-as.Date(c("6/29/23","7/6/23","7/12/23","7/19/23","7/26/23","8/10/23"),
                 format="%m/%d/%y")
Date.Labels<-c("June 29","July 6","July 13","July 19","July 26","Aug 10")


Soil.Water.Content<-ggplot(Data,aes(x=Date,y=Soil.Water.Content,color=Block,group=Block))+
  geom_line(linewidth=1)+
  geom_errorbar(aes(ymin=Soil.Water.Content-Soil.Water.Content_sd,ymax=Soil.Water.Content+Soil.Water.Content_sd),color="black",linewidth=0.8,width=0.75)+
  geom_point(size=5)+
  scale_color_manual(values=c("#2F6F4E","#4F8FB8","#C49A45"))+
  scale_x_date(breaks=Dates,labels=Date.Labels)+
  labs(x="",y="\nSoil\nWater Content (%)\n",color="Block")+
  theme(axis.line=element_line(size=0.5),
        axis.ticks=element_line(size=0.5),
        axis.ticks.length=unit(0.25,"cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.title.y=element_text(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"),
        legend.position="none",
        legend.direction="horizontal",
        legend.background=element_rect(fill="transparent",color=NA),
        legend.title=element_text(size=12,color="black"),
        legend.text=element_text(size=12,color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        plot.background=element_rect(fill="transparent",color=NA))
Soil.Water.Content


Soil.Temperature<-ggplot(Data,aes(x=Date,y=Soil.Temperature,color=Block,group=Block))+
  geom_line(linewidth=1)+
  geom_errorbar(aes(ymin=Soil.Temperature-Soil.Temperature_sd,ymax=Soil.Temperature+Soil.Temperature_sd),color="black",linewidth=0.8,width=0.75)+
  geom_point(size=5)+
  scale_color_manual(values=c("#2F6F4E","#4F8FB8","#C49A45"))+
  scale_x_date(breaks=Dates,labels=Date.Labels)+
  labs(x="",y="\nSoil\nTemperature (°C)\n")+
  theme(axis.line=element_line(size=0.5),
        axis.ticks=element_line(size=0.5),
        axis.ticks.length=unit(0.25,"cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=12,color="black"),
        axis.title.y=element_text(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"),
        legend.position="none",
        legend.direction="horizontal",
        legend.background=element_rect(fill="transparent",color=NA),
        legend.title=element_text(size=12,color="black"),
        legend.text=element_text(size=12,color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        plot.background=element_rect(fill="transparent",color=NA))
Soil.Temperature


Soil.Water.Content_Soil.Temperature<-Soil.Water.Content/Soil.Temperature+
  plot_layout(guides="collect")+
  plot_annotation(tag_levels="a")&
  theme(legend.position="top",
        plot.tag=element_text(size=14,face="bold"))
Soil.Water.Content_Soil.Temperature


ggsave(filename="Soil Water Content & Soil Temperature.png",
       plot=Soil.Water.Content_Soil.Temperature,
       width=6.5,
       height=4,
       dpi=600)


##### NET CO2 UPTAKE ----------------------------------------------------------


### Statistics ###


## Net CO2 Uptake of Bryophyte & Vascular Plants ##


Data <- read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")


Data<-Data %>%
  filter(Data.Origin == "Measured") %>%
  mutate(Date = as.Date(Date,format="%m/%d/%y"),
         Date = as.factor(Date),
         Block = as.factor(Block),
         Pair = interaction(Block, Collar.Pair, drop = TRUE),
         Collar = as.factor(Sample.ID))

Data<-Data %>%
  mutate(Net.Carbon.Dioxide.Uptake_umolm2s = if_else(
    Net.Carbon.Dioxide.Uptake_umolm2s > 0,0, Net.Carbon.Dioxide.Uptake_umolm2s)) %>%
  mutate(Net.Carbon.Dioxide.Uptake_umolm2s = abs(Net.Carbon.Dioxide.Uptake_umolm2s))



hist(Data$Net.Carbon.Dioxide.Uptake_umolm2s)
# Right skewed; Clear outliers

Data_outliers <- Data %>%
  group_by(Date,Block,Moss.Removal) %>%
  filter(Net.Carbon.Dioxide.Uptake_umolm2s %in%
           boxplot.stats(Net.Carbon.Dioxide.Uptake_umolm2s)$out) %>%
  ungroup()

Data<-Data[-46,]
Data<-Data[-15,]

hist(Data$Net.Carbon.Dioxide.Uptake_umolm2s)
# Right skewed


Net.Carbon.Dioxide.Uptake_lmer <- lmer(Net.Carbon.Dioxide.Uptake_umolm2s ~ Moss.Removal * Date * Block + (1 | Pair/Collar), 
                                       data = Data)

anova(Net.Carbon.Dioxide.Uptake_lmer)
# Type III Analysis of Variance Table with Satterthwaite's method
#                           Sum Sq    Mean Sq   NumDF   DenDF     F value     Pr(>F)    
# Moss.Removal              44.40     44.400    1       11.920    8.6162      0.01256 *  
# Date                      471.30    94.259    5       118.337   18.2917     2.030e-13 ***
# Block                     65.58     32.792    2       12.026    6.3636      0.01303 *  
# Moss.Removal:Date         18.22     3.644     5       118.344   0.7072      0.61912    
# Moss.Removal:Block        32.32     16.158    2       11.918    3.1356      0.08051 .  
# Date:Block                210.70    21.070    10      118.328   4.0888      7.569e-05 ***
# Moss.Removal:Date:Block   36.65     3.665     10      118.334   0.7113      0.71238    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Data.Summary <- 
  Data %>%
  group_by(Moss.Removal) %>%
  summarise(
    min=min(Net.Carbon.Dioxide.Uptake_umolm2s),
    max=max(Net.Carbon.Dioxide.Uptake_umolm2s),
    mean=mean(Net.Carbon.Dioxide.Uptake_umolm2s),
    sd=sd(Net.Carbon.Dioxide.Uptake_umolm2s))



## Difference in Net CO2 Uptake between Bryophytes & Vascular Plants


Data<-read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")


Data<-Data %>%
  filter(Data.Origin == "Measured") %>%
  mutate(Date = as.Date(Date,format="%m/%d/%y"),
         Date = as.factor(Date),
         Block = as.factor(Block),
         Pair = interaction(Block, Collar.Pair, drop = TRUE),
         Collar = as.factor(Sample.ID))

Data<-Data %>%
  mutate(Net.Carbon.Dioxide.Uptake_umolm2s=if_else(
    Net.Carbon.Dioxide.Uptake_umolm2s>0,0,Net.Carbon.Dioxide.Uptake_umolm2s)) %>%
  mutate(Net.Carbon.Dioxide.Uptake_umolm2s=abs(Net.Carbon.Dioxide.Uptake_umolm2s))

Data<-Data[-46,]
Data<-Data[-15,]

Data<-Data %>%
  select(Date,
         Block,
         Collar.Pair,
         Moss.Removal,
         Net.Carbon.Dioxide.Uptake_umolm2s) %>%
  pivot_wider(
    names_from=Moss.Removal,
    values_from=Net.Carbon.Dioxide.Uptake_umolm2s) %>%
  mutate(Difference=.data[["Moss Intact"]]-.data[["Moss Removed"]])

hist(Data$Difference)
# Normally distributed; No obvious outliers

T.Test<-Data %>%
  group_by(Block,Date) %>%
  do(tidy(t.test(.$Difference,mu=0))) %>%
  ungroup() %>%
  select(Date,Block,p.value)
T.Test
# Date       Block p.value
# 2023-06-29 1     0.0336 
# 2023-07-06 1     0.141  
# 2023-07-13 1     0.639  
# 2023-07-19 1     0.941  
# 2023-07-26 1     0.0238 
# 2023-08-10 1     0.604  
# 2023-06-29 2     0.118  
# 2023-07-06 2     0.0153 
# 2023-07-13 2     0.0175 
# 2023-07-19 2     0.0331 
# 2023-07-26 2     0.215  
# 2023-08-10 2     0.162  
# 2023-06-29 3     0.384  
# 2023-07-06 3     0.0138 
# 2023-07-13 3     0.00272
# 2023-07-19 3     0.380  
# 2023-07-26 3     0.718  
# 2023-08-10 3     0.264 



### Figures ###


Data<-read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")


Data<-Data %>%
  filter(Data.Origin=="Measured") %>%
  mutate(Date=as.Date(Date,format="%m/%d/%y"),
         Block=as.factor(Block),
         Collar.Pair=as.factor(Collar.Pair),
         Sample.ID=as.factor(Sample.ID))

Data<-Data %>%
  mutate(Net.Carbon.Dioxide.Uptake_umolm2s=if_else(
    Net.Carbon.Dioxide.Uptake_umolm2s>0,0,Net.Carbon.Dioxide.Uptake_umolm2s)) %>%
  mutate(Net.Carbon.Dioxide.Uptake_umolm2s=abs(Net.Carbon.Dioxide.Uptake_umolm2s))

Data<-Data[-46,]
Data<-Data[-15,]


Data<-Data %>%
  group_by(Date,Block,Moss.Removal) %>%
  summarise(
    Net.Carbon.Dioxide.Uptake=mean(Net.Carbon.Dioxide.Uptake_umolm2s,na.rm=TRUE),
    Net.Carbon.Dioxide.Uptake_n=n_distinct(Sample.ID),
    Net.Carbon.Dioxide.Uptake_sd=sd(Net.Carbon.Dioxide.Uptake_umolm2s,na.rm=TRUE),
    Net.Carbon.Dioxide.Uptake_se=Net.Carbon.Dioxide.Uptake_sd/sqrt(Net.Carbon.Dioxide.Uptake_n),
    .groups="drop"
  )


Dates<-as.Date(c("6/29/23","7/6/23","7/12/23","7/19/23","7/26/23","8/10/23"),
               format="%m/%d/%y")
Date.Labels<-c("June\n29","July\n6","July\n13","July\n19","July\n26","Aug\n10")


Net.Carbon.Dioxide.Uptake_moss.removal<-ggplot(Data,aes(x=Date,y=Net.Carbon.Dioxide.Uptake,color=Block,group=Block))+
  geom_line(linewidth=1)+
  geom_errorbar(aes(ymin=Net.Carbon.Dioxide.Uptake-Net.Carbon.Dioxide.Uptake_sd,ymax=Net.Carbon.Dioxide.Uptake+Net.Carbon.Dioxide.Uptake_sd),color="black",linewidth=0.8,width=0.75)+
  geom_point(size=5)+
  scale_color_manual(values=c("#2F6F4E","#4F8FB8","#C49A45"))+
  scale_x_date(breaks=Dates,labels=Date.Labels)+
  scale_y_continuous(name = "Net Carbon Dioxide<br>Uptake<br>(µmol m<sup>-2</sup> s<sup>-1</sup>)<br>")+
  labs(x="",color="Block")+
  facet_wrap(~Moss.Removal,labeller=labeller(Moss.Removal=c("Moss Intact"="Bryophytes-Intact","Moss Removed"="Bryophytes-Removed")))+
  theme(axis.line=element_line(size=0.5),
        axis.ticks=element_line(size=0.5),
        axis.ticks.length=unit(0.25,"cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=12,color="black"),
        axis.title.y=element_markdown(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"),
        strip.text=element_text(size=12,color="black"),
        legend.position="top",
        legend.direction="horizontal",
        legend.background=element_rect(fill="transparent",color=NA),
        legend.title=element_text(size=12,color="black"),
        legend.text=element_text(size=12,color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        plot.background=element_rect(fill="transparent",color=NA))
Net.Carbon.Dioxide.Uptake_moss.removal



Data<-read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")


Data<-Data %>%
  filter(Data.Origin=="Measured") %>%
  mutate(Date=as.Date(Date,format="%m/%d/%y"),
         Block=as.factor(Block),
         Collar.Pair=as.factor(Collar.Pair),
         Sample.ID=as.factor(Sample.ID))

Data<-Data %>%
  mutate(Net.Carbon.Dioxide.Uptake_umolm2s=if_else(
    Net.Carbon.Dioxide.Uptake_umolm2s>0,0,Net.Carbon.Dioxide.Uptake_umolm2s)) %>%
  mutate(Net.Carbon.Dioxide.Uptake_umolm2s=abs(Net.Carbon.Dioxide.Uptake_umolm2s))

Data<-Data[-46,]
Data<-Data[-15,]

Data<-Data %>%
  select(Date,
         Block,
         Collar.Pair,
         Moss.Removal,
         Net.Carbon.Dioxide.Uptake_umolm2s) %>%
  pivot_wider(
    names_from=Moss.Removal,
    values_from=Net.Carbon.Dioxide.Uptake_umolm2s) %>%
  mutate(Difference=.data[["Moss Intact"]]-.data[["Moss Removed"]])

Data<-Data[-18,]

Asterisks<-data.frame(Date=c("2023-06-29","2023-07-26","2023-07-06","2023-07-13","2023-07-19","2023-07-06","2023-07-13"),
                      Block=c("1","1","2","2","2","3","3"),
                      label=c("*","*","*","*","*","*","*"),
                      y=c(8,-4.9,8,8,8,8,8))

Net.Carbon.Dioxide.Uptake_date<-ggplot(Data,aes(x=Block,y=Difference,fill=Block))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", linewidth = 1) +
  geom_boxplot()+
  scale_fill_manual(values=c("#2F6F4E","#4F8FB8","#C49A45"),guide="none")+
  scale_y_continuous(name = "∆ Net Carbon Dioxide<br>Uptake<br>(µmol m<sup>-2</sup> s<sup>-1</sup>)<br>")+
  labs(x="",color="Block")+
  facet_wrap(~Date,nrow=1,labeller=labeller(Date=c("2023-06-29"="June 29","2023-07-06"="July 6","2023-07-13"="July 13","2023-07-19"="July 19","2023-07-26"="July 26","2023-08-10"="Aug 10")))+
  geom_text(data=Asterisks,aes(x=Block,y=y,label=label),inherit.aes=FALSE,size=10)+
  theme(axis.line=element_line(size=0.5),
        axis.ticks=element_line(size=0.5),
        axis.ticks.length=unit(0.25,"cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=12,color="black"),
        axis.title.y=element_markdown(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"),
        strip.text=element_text(size=12,color="black"),
        legend.position="none",
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        plot.background=element_rect(fill="transparent",color=NA))
Net.Carbon.Dioxide.Uptake_date


Net.Carbon.Dioxide.Uptake<-Net.Carbon.Dioxide.Uptake_moss.removal/Net.Carbon.Dioxide.Uptake_date+
  plot_layout(guides="collect",heights=c(1,1.3))+
  plot_annotation(tag_levels="a")&
  theme(plot.tag=element_text(size=14,face="bold"),
        legend.position="top")
Net.Carbon.Dioxide.Uptake


ggsave(filename="Net Carbon Dioxide Uptake.png",
       plot=Net.Carbon.Dioxide.Uptake,
       width=6.5,
       height=5,
       dpi=600)



##### NET CO2 UPTAKE x BRYOPHYTE AREA ----------------------------------------------------------


### Statistics ###


Data<-read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")


Data<-Data %>%
  filter(Data.Origin=="Measured",Moss.Removal=="Moss Intact") %>%
  mutate(Date=as.Date(Date,format="%m/%d/%y"),
         Date=as.factor(Date),
         Block=as.factor(Block),
         Collar.Pair=as.factor(Collar.Pair),
         Sample.ID=as.factor(Sample.ID))

Data<-Data %>%
  mutate(Net.Carbon.Dioxide.Uptake_umolm2s=if_else(
    Net.Carbon.Dioxide.Uptake_umolm2s>0,0,Net.Carbon.Dioxide.Uptake_umolm2s)) %>%
  mutate(Net.Carbon.Dioxide.Uptake_umolm2s=abs(Net.Carbon.Dioxide.Uptake_umolm2s))

Data<-Data[-22,]


hist(Data$Moss.Cover_cm2)
# Right skewed
hist(log(Data$Moss.Cover_cm2))

hist(Data$Net.Carbon.Dioxide.Uptake_umolm2s)
# Normally distributed
hist(log(Data$Net.Carbon.Dioxide.Uptake_umolm2s))


Net.Carbon.Dioxide.Uptake_Moss.Cover_lm<-Data %>%
  group_by(Date) %>%
  group_modify(~{
    model<-lm(Net.Carbon.Dioxide.Uptake_umolm2s~Moss.Cover_cm2,data=.)
    tidy(model) %>%
      filter(term=="Moss.Cover_cm2") %>%
      select(estimate,p.value) %>%
      bind_cols(
        glance(model) %>%
          select(r.squared,adj.r.squared))}) %>%
  ungroup()
Net.Carbon.Dioxide.Uptake_Moss.Cover_lm
# Only June 29th has a significant relationship



### Figures ###


Data<-read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")


Data<-Data %>%
  filter(Data.Origin=="Measured",Moss.Removal=="Moss Intact") %>%
  mutate(Date=as.Date(Date,format="%m/%d/%y"),
         Date=as.factor(Date),
         Block=as.factor(Block),
         Collar.Pair=as.factor(Collar.Pair),
         Sample.ID=as.factor(Sample.ID))

Data<-Data %>%
  mutate(Net.Carbon.Dioxide.Uptake_umolm2s=if_else(
    Net.Carbon.Dioxide.Uptake_umolm2s>0,0,Net.Carbon.Dioxide.Uptake_umolm2s)) %>%
  mutate(Net.Carbon.Dioxide.Uptake_umolm2s=abs(Net.Carbon.Dioxide.Uptake_umolm2s))

Data<-Data[-22,]


Dates<-as.Date(c("6/29/23","7/6/23","7/12/23","7/19/23","7/26/23","8/10/23"),
               format="%m/%d/%y")
Date.Labels<-c("June\n29","July\n6","July\n13","July\n19","July\n26","Aug\n10")


Net.Carbon.Dioxide.Uptake_Moss.Cover<-ggplot(Data,aes(x=Moss.Cover_cm2,y=Net.Carbon.Dioxide.Uptake_umolm2s))+
  geom_point(aes(color=Block),size=4)+
  geom_smooth(data=Data %>%
                filter(Date=="2023-06-29"),
              method="lm",se=TRUE,color="black")+
  scale_color_manual(values=c("#2F6F4E","#4F8FB8","#C49A45"))+
  scale_x_continuous(name="\nBryophyte Cover (cm²)")+
  scale_y_continuous(name = "Net Carbon Dioxide Uptake<br>(µmol m<sup>-2</sup> s<sup>-1</sup>)<br>")+
  labs(color="Block")+
  facet_wrap(~Date,labeller=labeller(Date=c("2023-06-29"="June 29","2023-07-06"="July 6","2023-07-13"="July 13","2023-07-19"="July 19","2023-07-26"="July 26","2023-08-10"="Aug 10")))+
  theme(axis.line=element_line(size=0.5),
        axis.ticks=element_line(size=0.5),
        axis.ticks.length=unit(0.25,"cm"),
        axis.title.x=element_text(size=12,color="black"),
        axis.text.x=element_text(size=12,color="black"),
        axis.title.y=element_markdown(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"),
        strip.text=element_text(size=12,color="black"),
        legend.position="top",
        legend.direction="horizontal",
        legend.background=element_rect(fill="transparent",color=NA),
        legend.title=element_text(size=12,color="black"),
        legend.text=element_text(size=12,color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        plot.background=element_rect(fill="transparent",color=NA))
Net.Carbon.Dioxide.Uptake_Moss.Cover


ggsave(filename="Net Carbon Dioxide Uptake by Bryophyte Cover.png",
       plot=Net.Carbon.Dioxide.Uptake_Moss.Cover,
       width=6.5,
       height=4,
       dpi=500)



##### CO2 EFFLUX ----------------------------------------------------------


### Statistics ###


Data<-read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")


Data <- Data %>%
  filter(Data.Origin == "Measured") %>%
  mutate(Date = as.Date(Date,format="%m/%d/%y"),
         Date = as.factor(Date),
         Block = as.factor(Block),
         Pair = interaction(Block, Collar.Pair, drop = TRUE),
         Collar = as.factor(Sample.ID))


hist(Data$Carbon.Dioxide.Efflux_umolm2s)
# Normally distributed; Clear outlier
Data<-Data[-21,]
hist(Data$Carbon.Dioxide.Efflux_umolm2s)
# Normally distributed


Carbon.Dioxide.Efflux_lmer <- lmer(Carbon.Dioxide.Efflux_umolm2s ~ Moss.Removal * Date * Block + (1 | Pair/Collar), data=Data)
anova(Carbon.Dioxide.Efflux_lmer)
# Type III Analysis of Variance Table with Satterthwaite's method
#                           Sum Sq    Mean Sq     NumDF     DenDF       F value     Pr(>F)    
# Moss.Removal              8.57      8.573       1         12.107      8.7349      0.011920 *  
# Date                      1063.10   212.620     5         119.396     216.6459    < 2.2e-16 ***
# Block                     103.22    51.609      2         12.073      52.5861     1.097e-06 ***
# Moss.Removal:Date         11.72     2.344       5         119.396     2.3885      0.041932 *  
# Moss.Removal:Block        13.99     6.993       2         12.106      7.1250      0.009013 ** 
# Date:Block                318.23    31.823      10        119.388     32.4254     < 2.2e-16 ***
# Moss.Removal:Date:Block   44.80     4.480       10        119.388     4.5653      1.758e-05 ***
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


Data.Summary <-
  Data %>%
  group_by(Moss.Removal) %>%
  summarise(
    min=min(Carbon.Dioxide.Efflux_umolm2s),
    max=max(Carbon.Dioxide.Efflux_umolm2s),
    mean=mean(Carbon.Dioxide.Efflux_umolm2s),
    sd=sd(Carbon.Dioxide.Efflux_umolm2s))



## Difference in Net CO2 Uptake between Bryophytes & Vascular Plants


Data<-read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")


Data<-Data %>%
  filter(Data.Origin == "Measured") %>%
  mutate(Date = as.Date(Date,format="%m/%d/%y"),
         Date = as.factor(Date),
         Block = as.factor(Block),
         Pair = interaction(Block, Collar.Pair, drop = TRUE),
         Collar = as.factor(Sample.ID))


Data<-Data[-21,]


Data<-Data %>%
  select(Date,
         Block,
         Collar.Pair,
         Moss.Removal,
         Carbon.Dioxide.Efflux_umolm2s) %>%
  pivot_wider(
    names_from=Moss.Removal,
    values_from=Carbon.Dioxide.Efflux_umolm2s) %>%
  mutate(Difference=.data[["Moss Intact"]]-.data[["Moss Removed"]])

hist(Data$Difference)
# Normally distributed; No obvious outliers
Data<-Data[-66,]
Data<-Data[-39,]


T.Test<-Data %>%
  group_by(Block,Date) %>%
  do(tidy(t.test(.$Difference,mu=0))) %>%
  ungroup() %>%
  select(Date,Block,p.value)
T.Test
# July 19th x Block 1: P < 0.001
# July 26th x Block 1: P < 0.01
# August 10th x Block 1: P < 0.001
# June 29th x Block 2: P < 0.01
# July 19th x Block 2: P = 0.03
# August 10th x Block 2: P = 0.02
# July 6th x Block 3: P = 0.02
# July 13th x Block 3: P = 0.04
# July 19th x Block 3: P = 0.01
# July 26th x Block 3: P < 0.01


Data.Summary <-
  Data %>%
  group_by(Date, Block) %>%
  summarise(
    min=min(Difference),
    max=max(Difference),
    mean=mean(Difference),
    se=sd(Difference)/sqrt(length(Difference)))



### Figures ###


Data<-read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")


Data<-Data %>%
  filter(Data.Origin=="Measured") %>%
  mutate(Date=as.Date(Date,format="%m/%d/%y"),
         Block=as.factor(Block),
         Collar.Pair=as.factor(Collar.Pair),
         Sample.ID=as.factor(Sample.ID))


Data<-Data[-21,]


Data<-Data %>%
  group_by(Date,Block,Moss.Removal) %>%
  summarise(
    Soil.Respiration=mean(Carbon.Dioxide.Efflux_umolm2s,na.rm=TRUE),
    Soil.Respiration_n=n_distinct(Sample.ID),
    Soil.Respiration_sd=sd(Carbon.Dioxide.Efflux_umolm2s,na.rm=TRUE),
    Soil.Respiration_se=Soil.Respiration_sd/sqrt(Soil.Respiration_n),
    .groups="drop"
  )


Dates<-as.Date(c("6/29/23","7/6/23","7/12/23","7/19/23","7/26/23","8/10/23"),
               format="%m/%d/%y")
Date.Labels<-c("June\n29","July\n6","July\n13","July\n19","July\n26","Aug\n10")


Soil.Respiration<-ggplot(Data,aes(x=Date,y=Soil.Respiration,color=Block,group=Block))+
  geom_line(linewidth=1)+
  geom_errorbar(aes(ymin=Soil.Respiration-Soil.Respiration_sd,ymax=Soil.Respiration+Soil.Respiration_sd),color="black",linewidth=0.8,width=0.75)+
  geom_point(size=4)+
  scale_color_manual(values=c("#2F6F4E","#4F8FB8","#C49A45"))+
  scale_x_date(breaks=Dates,labels=Date.Labels)+
  scale_y_continuous(name = "Carbon Dioxide Efflux<br>(µmol m<sup>-2</sup> s<sup>-1</sup>)<br>")+
  labs(x="",color="Block")+
  facet_wrap(~Moss.Removal,labeller=labeller(Moss.Removal=c("Moss Intact"="Bryophytes-Intact","Moss Removed"="Bryophytes-Removed")))+
  theme(axis.line=element_line(size=0.5),
        axis.ticks=element_line(size=0.5),
        axis.ticks.length=unit(0.25,"cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=12,color="black"),
        axis.title.y=element_markdown(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"),
        strip.text=element_text(size=12,color="black"),
        legend.position="top",
        legend.direction="horizontal",
        legend.background=element_rect(fill="transparent",color=NA),
        legend.title=element_text(size=12,color="black"),
        legend.text=element_text(size=12,color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        plot.background=element_rect(fill="transparent",color=NA))
Soil.Respiration



Data<-read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")


Data<-Data %>%
  filter(Data.Origin=="Measured") %>%
  mutate(Date=as.Date(Date,format="%m/%d/%y"),
         Date=as.factor(Date),
         Block=as.factor(Block),
         Collar.Pair=as.factor(Collar.Pair),
         Sample.ID=as.factor(Sample.ID))


Data<-Data[-21,]


Data<-Data %>%
  select(Date,
         Block,
         Collar.Pair,
         Moss.Removal,
         Carbon.Dioxide.Efflux_umolm2s) %>%
  pivot_wider(
    names_from=Moss.Removal,
    values_from=Carbon.Dioxide.Efflux_umolm2s) %>%
  mutate(Difference=.data[["Moss Intact"]]-.data[["Moss Removed"]])

Data<-Data[-66,]
Data<-Data[-39,]


Asterisks<-data.frame(Date=c("2023-07-19","2023-07-26","2023-08-10",
                             "2023-06-29","2023-07-19","2023-08-10",
                             "2023-07-06","2023-07-13","2023-07-19","2023-07-26"),
                      Block=c("1","1","1",
                              "2","2","2",
                              "3","3","3","3"),
                      label=c("*","*","*",
                              "*","*","*",
                              "*","*","*","*"),
                      y=c(-3.5,-3.5,5,
                          5,5,5,
                          5,5,5,5))


Soil.Respiration_date<-ggplot(Data,aes(x=Block,y=Difference,fill=Block))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", linewidth = 1) +
  geom_boxplot()+
  scale_fill_manual(values=c("#2F6F4E","#4F8FB8","#C49A45"),guide="none")+
  scale_y_continuous(name = "∆ Carbon Dioxide Efflux<br>(µmol m<sup>-2</sup> s<sup>-1</sup>)<br>", breaks=seq(-4,5.5,2),limits=c(-4,5.5))+
  labs(x="",color="Block")+
  geom_text(data=Asterisks,aes(x=Block,y=y,label=label),inherit.aes=FALSE,size=10)+
  facet_wrap(~Date,nrow=1,labeller=labeller(Date=c("2023-06-29"="June 29","2023-07-06"="July 6","2023-07-13"="July 13","2023-07-19"="July 19","2023-07-26"="July 26","2023-08-10"="Aug 10")))+
  theme(axis.line=element_line(size=0.5),
        axis.ticks=element_line(size=0.5),
        axis.ticks.length=unit(0.25,"cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=12,color="black"),
        axis.title.y=element_markdown(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"),
        strip.text=element_text(size=12,color="black"),
        legend.position="none",
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        plot.background=element_rect(fill="transparent",color=NA))
Soil.Respiration_date



Soil.Respiration<-Soil.Respiration/Soil.Respiration_date+
  plot_layout(guides="collect",heights=c(1,1.3))+
  plot_annotation(tag_levels="a")&
  theme(plot.tag=element_text(size=14,face="bold"),
        legend.position="top")
Soil.Respiration


ggsave(filename="Carbon Dioxide Efflux.png",
       plot=Soil.Respiration,
       width=6.5,
       height=5,
       dpi=600)





##### NET ECOSYSTEM EXCHANGE ----------------------------------------------------------


### Statistics ###


Data<-read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")


Data<-Data %>%
  filter(Data.Origin == "Measured") %>%
  mutate(Date = as.Date(Date,format="%m/%d/%y"),
         Date = as.factor(Date),
         Block = as.factor(Block),
         Pair = interaction(Block, Collar.Pair, drop = TRUE),
         Collar = as.factor(Sample.ID))


hist(Data$NEE_gday)
# Normally distributed; Clear outlier

Data<-Data[-46,]


NEE_lmer <- lmer(NEE_gday ~ Moss.Removal * Date * Block + (1 | Pair/Collar), data = Data)
anova(NEE_lmer)
# Type III Analysis of Variance Table with Satterthwaite's method
#                           Sum Sq    Mean Sq     NumDF     DenDF     F value     Pr(>F)    
# Moss.Removal              42.578    42.578      1         131.008   10.7700     0.001322 ** 
# Date                      285.449   57.090      5         131.007   14.4407     2.940e-11 ***
# Block                     54.693    27.346      2         11.966    6.9171      0.010084 *  
# Moss.Removal:Date         6.674     1.335       5         131.007   0.3376      0.889336    
# Moss.Removal:Block        47.567    23.784      2         131.008   6.0160      0.003165 ** 
# Date:Block                233.132   23.313      10        131.005   5.8970      2.538e-07 ***
# Moss.Removal:Date:Block   31.532    3.153       10        131.005   0.7976      0.631173    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


Data.Summary <- 
  Data %>%
  group_by(Moss.Removal) %>%
  summarise(
    min=min(NEE_gday),
    max=max(NEE_gday),
    mean=mean(NEE_gday),
    sd=sd(NEE_gday))



## Difference in NEE between Bryophytes & Vascular Plants


Data<-read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")


Data<-Data %>%
  filter(Data.Origin=="Measured") %>%
  mutate(Date=as.Date(Date,format="%m/%d/%y"),
         Date=as.factor(Date),
         Block=as.factor(Block),
         Collar.Pair=as.factor(Collar.Pair),
         Sample.ID=as.factor(Sample.ID))


Data<-Data %>%
  select(Date,
         Block,
         Collar.Pair,
         Moss.Removal,
         NEE_gday) %>%
  pivot_wider(
    names_from=Moss.Removal,
    values_from=NEE_gday) %>%
  mutate(Difference=.data[["Moss Intact"]]-.data[["Moss Removed"]])

hist(Data$Difference)
# Normally distributed; Clear outlier
Data<-Data[-22,]


T.Test<-Data %>%
  group_by(Block,Date) %>%
  do(tidy(t.test(.$Difference,mu=0))) %>%
  ungroup() %>%
  select(Date,Block,p.value)
T.Test
# June 29th x Block 1: P < 0.01
# July 26th x Block 1: P = 0.04
# July 6th x Block 2: P = 0.02
# July 13th x Block 2: P = 0.04
# July 19th x Block 2: P < 0.01
# July 19th x Block 3: P = 0.02



### Figures ###


Data<-read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")


Data<-Data %>%
  filter(Data.Origin=="Measured") %>%
  mutate(Date=as.Date(Date,format="%m/%d/%y"),
         Block=as.factor(Block),
         Collar.Pair=as.factor(Collar.Pair),
         Sample.ID=as.factor(Sample.ID))

Data<-Data[-46,]


Data<-Data %>%
  group_by(Date,Block,Moss.Removal) %>%
  summarise(
    NEE=mean(NEE_gday,na.rm=TRUE),
    NEE_n=n_distinct(Sample.ID),
    NEE_sd=sd(NEE_gday,na.rm=TRUE),
    NEE_se=NEE_sd/sqrt(NEE_n),
    .groups="drop"
  )


Dates<-as.Date(c("6/29/23","7/6/23","7/12/23","7/19/23","7/26/23","8/10/23"),
               format="%m/%d/%y")
Date.Labels<-c("June\n29","July\n6","July\n13","July\n19","July\n26","Aug\n10")


NEE<-ggplot(Data,aes(x=Date,y=NEE,color=Block,group=Block))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", linewidth = 1) +
  geom_line(linewidth=1)+
  geom_errorbar(aes(ymin=NEE-NEE_sd,ymax=NEE+NEE_sd),color="black",linewidth=0.8,width=0.75)+
  geom_point(size=4)+
  scale_color_manual(values=c("#2F6F4E","#4F8FB8","#C49A45"))+
  scale_x_date(breaks=Dates,labels=Date.Labels)+
  scale_y_continuous(name = "Net Ecosystem Exchange<br>(g m<sup>-2</sup> d<sup>-1</sup>)<br>")+
  labs(x="",color="Block")+
  facet_wrap(~Moss.Removal,labeller=labeller(Moss.Removal=c("Moss Intact"="Bryophytes-Intact","Moss Removed"="Bryophytes-Removed")))+
  theme(axis.line=element_line(size=0.5),
        axis.ticks=element_line(size=0.5),
        axis.ticks.length=unit(0.25,"cm"),
        axis.title.x=element_blank(),
        axis.text.x=element_text(size=12,color="black"),
        axis.title.y=element_markdown(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"),
        strip.text=element_text(size=12,color="black"),
        legend.position="top",
        legend.direction="horizontal",
        legend.background=element_rect(fill="transparent",color=NA),
        legend.title=element_text(size=12,color="black"),
        legend.text=element_text(size=12,color="black"),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        plot.background=element_rect(fill="transparent",color=NA))
NEE



ggsave(filename="Net Ecosystem Exchange.png",
       plot=NEE,
       width=6.5,
       height=3.5,
       dpi=600)



##### NET ECOSYSTEM PRODUCTION ----------------------------------------------------------


### Statistics ###


Data<-read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")


Data<-Data %>%
  mutate(Date=as.Date(Date,format="%m/%d/%y"),
         Date=as.factor(Date),
         Block=as.factor(Block),
         Pair = interaction(Block, Collar.Pair, drop = TRUE),
         Collar = as.factor(Sample.ID))


hist(Data$NEE_gday)
# Normally distributed; Clear outlier

Data<-Data[-c(424:435),]


Data<-Data %>%
  group_by(Sample.ID,Moss.Removal,Block,Pair,Collar) %>%
  summarise(
    NEP=sum(NEE_gday)) %>%
  ungroup()

Data<-Data %>%
  mutate(NEP=-NEP)


NEP_aov <- aov(NEP ~ Moss.Removal * Block, data = Data)
summary(NEP_aov)
# Moss Removal: P = 0.03
# Block: P < 0.001
# Moss Removal * Block: P = 0.12
TukeyHSD(NEP_aov,"Block",conf.level=0.95)


Data.Summary <-
  Data %>%
  group_by(Moss.Removal) %>%
  summarise(
    min=min(NEP),
    max=max(NEP),
    mean=mean(NEP),
    sd=sd(NEP))



### Figures ###


Data<-read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")


Data<-Data %>%
  mutate(Date=as.Date(Date,format="%m/%d/%y"),
         Date=as.factor(Date),
         Block=as.factor(Block),
         Collar.Pair=as.factor(Collar.Pair),
         Sample.ID=as.factor(Sample.ID))

Data<-Data[-c(424:435),]


Data<-Data %>%
  group_by(Sample.ID,Moss.Removal,Block,Collar.Pair) %>%
  summarise(
    NEP=sum(NEE_gday)) %>%
  ungroup()

Data<-Data %>%
  mutate(NEP=-NEP)


NEP<-ggplot(Data,aes(x=Block,y=NEP,fill=Block))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", linewidth = 1) +
  geom_boxplot()+
  scale_fill_manual(values=c("#2F6F4E","#4F8FB8","#C49A45"),guide="none")+
  scale_y_continuous(name = "Net Ecosystem Production<br>(g m<sup>-2</sup>)<br>")+
  facet_wrap(~Moss.Removal,labeller=labeller(Moss.Removal=c("Moss Intact"="Bryophytes-Intact","Moss Removed"="Bryophytes-Removed")))+
  theme(axis.line=element_line(size=0.5),
        axis.ticks=element_line(size=0.5),
        axis.ticks.length=unit(0.25,"cm"),
        axis.title.x=element_text(size=12,color="black"),
        axis.text.x=element_text(size=12,color="black"),
        axis.title.y=element_markdown(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"),
        strip.text=element_text(size=12,color="black"),
        legend.position="none",
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        plot.background=element_rect(fill="transparent",color=NA))
NEP


ggsave(filename="Net Ecosystem Production.png",
       plot=NEP,
       width=6.5,
       height=3,
       dpi=600)




##### TRANSECT NET CO2 UPTAKE ------------------------------------



Experimental.Data <- read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")

Observational.Data<-read.csv("Van Norden Meadow - Moss Project_Observational Data.csv")


Experimental.Data <- Experimental.Data %>%
  filter(Data.Origin == "Measured") %>%
  filter(Moss.Removal == "Moss Removed") %>%
  mutate(Date = as.Date(Date,format="%m/%d/%y"),
         Date = as.factor(Date)) %>%
  group_by(Date) %>%
  summarise(
    Net.Carbon.Dioxide.Uptake = mean(Net.Carbon.Dioxide.Uptake_umolm2s))

Observational.Data <- Observational.Data %>%
  filter(Data.Origin == "Measured") %>%
  group_by(Date) %>%
  summarise(
    Net.Carbon.Dioxide.Uptake = mean(Net.Carbon.Uptake_umolm2s))


Experimental.Data <- Experimental.Data %>%
  mutate(Net.Carbon.Dioxide.Uptake = if_else(
    Net.Carbon.Dioxide.Uptake > 0,0, Net.Carbon.Dioxide.Uptake)) %>%
  mutate(Net.Carbon.Dioxide.Uptake = abs(Net.Carbon.Dioxide.Uptake))

Observational.Data <- Observational.Data %>%
  mutate(Net.Carbon.Dioxide.Uptake = if_else(
    Net.Carbon.Dioxide.Uptake > 0,0, Net.Carbon.Dioxide.Uptake)) %>%
  mutate(Net.Carbon.Dioxide.Uptake = abs(Net.Carbon.Dioxide.Uptake))


t.test <- t.test(Experimental.Data$Net.Carbon.Dioxide.Uptake, Observational.Data$Net.Carbon.Dioxide.Uptake)
print(t.test)
# P = 0.1444


Data <- 
  data.frame(Net.Carbon.Dioxide.Uptake = c(Experimental.Data$Net.Carbon.Dioxide.Uptake, Observational.Data$Net.Carbon.Dioxide.Uptake),
             Data.Source = rep(c("Bryophyte-Removed", "Reference"), times = c(length(Experimental.Data$Net.Carbon.Dioxide.Uptake), length(Observational.Data$Net.Carbon.Dioxide.Uptake))))


Reference <-
  ggplot(Data,aes(x = Data.Source, y = Net.Carbon.Dioxide.Uptake, fill = Data.Source)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#8B5A2B", "#C4A484"), guide="none")+
  scale_y_continuous(name = "Net Carbon Dioxide<br>Uptake<br>(µmol m<sup>-2</sup> s<sup>-1</sup>)<br>") +
  scale_x_discrete(name = "") +
  theme(axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(0.25,"cm"),
        axis.title.x = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.y = element_markdown(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 12, color = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA))
Reference


ggsave(filename = "Net Carbon Dioxide Uptake_Bryophyte-Removal vs Reference.png",
       plot = Reference,
       width=4,
       height=5,
       dpi=600)




##### TRANSECT CO2 EFFLUX ------------------------------------



Experimental.Data <- read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")

Observational.Data<-read.csv("Van Norden Meadow - Moss Project_Observational Data.csv")


Experimental.Data <- Experimental.Data %>%
  filter(Data.Origin == "Measured") %>%
  filter(Moss.Removal == "Moss Removed") %>%
  mutate(Date = as.Date(Date,format="%m/%d/%y"),
         Date = as.factor(Date)) %>%
  group_by(Date) %>%
  summarise(
    Carbon.Dioxide.Efflux = mean(Carbon.Dioxide.Efflux_umolm2s))

Observational.Data <- Observational.Data %>%
  filter(Data.Origin == "Measured") %>%
  group_by(Date) %>%
  summarise(
    Carbon.Dioxide.Efflux = mean(Soil.Respiration_umolm2s))


t.test <- t.test(Experimental.Data$Carbon.Dioxide.Efflux, Observational.Data$Carbon.Dioxide.Efflux)
print(t.test)
# P = 0.4928


Data <- 
  data.frame(Carbon.Dioxide.Efflux = c(Experimental.Data$Carbon.Dioxide.Efflux, Observational.Data$Carbon.Dioxide.Efflux),
             Data.Source = rep(c("Bryophyte-Removed", "Reference"), times = c(length(Experimental.Data$Carbon.Dioxide.Efflux), length(Observational.Data$Carbon.Dioxide.Efflux))))


Reference <-
  ggplot(Data,aes(x = Data.Source, y = Carbon.Dioxide.Efflux, fill = Data.Source)) +
  geom_boxplot() +
  scale_fill_manual(values = c("#8B5A2B", "#C4A484"), guide="none")+
  scale_y_continuous(name = "Carbon Dioxide Efflux<br>(µmol m<sup>-2</sup> s<sup>-1</sup>)<br>") +
  scale_x_discrete(name = "") +
  theme(axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(0.25,"cm"),
        axis.title.x = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.y = element_markdown(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 12, color = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA))
Reference


ggsave(filename = "Carbon Dioxide Efflux_Bryophyte-Removal vs Reference.png",
       plot = Reference,
       width=4,
       height=5,
       dpi=600)




##### TRANSECT NET ECOSYSTEM EXCHANGE ------------------------------------



Experimental.Data <- read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")

Observational.Data<-read.csv("Van Norden Meadow - Moss Project_Observational Data.csv")


Experimental.Data <- Experimental.Data %>%
  filter(Data.Origin == "Measured") %>%
  filter(Moss.Removal == "Moss Removed") %>%
  mutate(Date = as.Date(Date,format="%m/%d/%y"),
         Date = as.factor(Date)) %>%
  group_by(Date) %>%
  summarise(
    NEE = mean(NEE_gday))

Observational.Data <- Observational.Data %>%
  filter(Data.Origin == "Measured") %>%
  group_by(Date) %>%
  summarise(
    NEE = mean(NEE_gm2d))


t.test <- t.test(Experimental.Data$NEE, Observational.Data$NEE)
print(t.test)
# P = 0.07


Data <- 
  data.frame(NEE = c(Experimental.Data$NEE, Observational.Data$NEE),
             Data.Source = rep(c("Bryophyte-Removed", "Reference"), times = c(length(Experimental.Data$NEE), length(Observational.Data$NEE))))


Reference <-
  ggplot(Data,aes(x = Data.Source, y = NEE, fill = Data.Source)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", linewidth = 1) +
  geom_boxplot() +
  scale_fill_manual(values = c("#8B5A2B", "#C4A484"), guide="none")+
  scale_y_continuous(name = "Net Ecosystem Exchange<br>(g m<sup>-2</sup> d<sup>-1</sup>)<br>") +
  scale_x_discrete(name = "") +
  theme(axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(0.25,"cm"),
        axis.title.x = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.y = element_markdown(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 12, color = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA))
Reference


ggsave(filename = "Net Ecosystem Exchange_Bryophyte-Removal vs Reference.png",
       plot = Reference,
       width=4,
       height=5,
       dpi=600)




##### TRANSECT NET ECOSYSTEM PRODUCTION ------------------------------------



### Interpolating the Data ###


Data<-read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")
Observational.Data<-read.csv("Van Norden Meadow - Moss Project_Observational Data.csv")


Data<-Data %>%
  mutate(Date=as.Date(Date,format="%m/%d/%y")) %>%
  select(Date,Daylength_h)

Observational.Data<-Observational.Data %>%
  mutate(Date=as.Date(Date))

Data<-Data %>%
  select(Date,Daylength_h) %>%
  distinct(Date,.keep_all=TRUE)

Observational.Data<-left_join(Observational.Data,Data,by="Date")


Observational.Data$NEE_gm2d<-((Observational.Data$Net.Carbon.Uptake_umolm2s/10^6*12.01*60*60*Observational.Data$Daylength_h)+
                                (Observational.Data$Soil.Respiration_umolm2s/10^6*12.01*60*60*(24-Observational.Data$Daylength_h)))

Observational.Data<-Observational.Data %>%
  group_by(Sample.ID) %>%
  mutate(NEE_gm2d=approx(
    x=Date[!is.na(NEE_gm2d)],
    y=NEE_gm2d[!is.na(NEE_gm2d)],
    xout=Date,rule=2)$y) %>%
  ungroup()

write.csv(Observational.Data,"Van Norden Meadow - Moss Project_Observational Data.csv",row.names=FALSE)



### Summary ###


Observational.Data<-read.csv("Van Norden Meadow - Moss Project_Observational Data.csv")


Observational.Data<-Observational.Data %>%
  mutate(Date=as.Date(Date),
         Site=as.factor(Site),
         Collar=as.factor(Collar),
         Sample.ID=as.factor(Sample.ID))


hist(Observational.Data$NEE_gm2d)
# Normally distributed; There are clear outliers

Observational.Data<-Observational.Data %>%
  arrange(Sample.ID,Date)

Observational.Data<-Observational.Data %>%
  filter(Sample.ID!="NS6",Sample.ID!="NS5")

hist(Observational.Data$NEE_gm2d)


Observational.Data<-Observational.Data %>%
  group_by(Sample.ID,Site) %>%
  summarise(
    NEP=sum(NEE_gm2d)) %>%
  ungroup()

Observational.Data<-Observational.Data %>%
  mutate(NEP=-NEP)

Observational.Data.Summary<-Observational.Data %>%
  summarise(
    min=min(NEP),
    max=max(NEP),
    mean=mean(NEP),
    se=sd(NEP)/sqrt(length(NEP)))


### Figure ###


Observational.Data<-read.csv("Van Norden Meadow - Moss Project_Observational Data.csv")


Observational.Data<-Observational.Data %>%
  mutate(Date=as.Date(Date),
         Site=as.factor(Site),
         Collar=as.factor(Collar),
         Sample.ID=as.factor(Sample.ID))


Observational.Data<-Observational.Data %>%
  filter(Sample.ID!="NS6",Sample.ID!="NS5")


Observational.Data<-Observational.Data %>%
  group_by(Sample.ID,Site) %>%
  summarise(
    NEP=sum(NEE_gm2d)) %>%
  ungroup()

Observational.Data<-Observational.Data %>%
  mutate(NEP=-NEP)


NEP<-ggplot(Observational.Data,aes(x=Site,y=NEP,fill=Site))+
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", linewidth = 1) +
  geom_boxplot()+
  scale_fill_manual(values=c("#8B5A2B","#C4A484"),guide="none")+
  scale_y_continuous(name = "Net Ecosystem Production<br>(g m<sup>-2</sup>)<br>")+
  scale_x_discrete(name = "\nMeadow Side")+
  theme(axis.line=element_line(size=0.5),
        axis.ticks=element_line(size=0.5),
        axis.ticks.length=unit(0.25,"cm"),
        axis.title.x=element_text(size=12,color="black"),
        axis.text.x=element_text(size=12,color="black"),
        axis.title.y=element_markdown(size=12,color="black"),
        axis.text.y=element_text(size=12,color="black"),
        strip.text=element_text(size=12,color="black"),
        legend.position="none",
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        plot.background=element_rect(fill="transparent",color=NA))
NEP


ggsave(filename="Net Ecosystem Production_Meadow-Scale.png",
       plot=NEP,
       width=4,
       height=5,
       dpi=500)




##### TRANSECT NET ECOSYSTEM PRODUCTION ------------------------------------



Experimental.Data <- read.csv("Van Norden Meadow - Moss Project_Experimental Data.csv")

Observational.Data<-read.csv("Van Norden Meadow - Moss Project_Observational Data.csv")


Experimental.Data <- Experimental.Data %>%
  filter(Moss.Removal == "Moss Removed") %>%
  mutate(Date = as.Date(Date,format="%m/%d/%y"),
         Date = as.factor(Date)) %>%
  group_by(Date, Sample.ID) %>%
  summarise(
    NEE = mean(NEE_gday))

Observational.Data <- Observational.Data %>%
  group_by(Date,Sample>ID) %>%
  summarise(
    NEE = mean(NEE_gm2d))


Experimental.Data<-Experimental.Data %>%
  group_by(Sample.ID) %>%
  summarise(
    NEP=sum(NEE)) %>%
  ungroup()

Observational.Data<-Observational.Data %>%
  group_by(Sample.ID) %>%
  summarise(
    NEP=sum(NEE)) %>%
  ungroup()


Experimental.Data<-Experimental.Data %>%
  mutate(NEP=-NEP)

Observational.Data<-Observational.Data %>%
  mutate(NEP=-NEP)


Observational.Data.Summary<-Observational.Data %>%
  summarise(
    min=min(NEP),
    max=max(NEP),
    mean=mean(NEP),
    se=sd(NEP)/sqrt(length(NEP)))


t.test <- t.test(Experimental.Data$NEP, Observational.Data$NEP)
print(t.test)
# P = 0.07


Data <- 
  data.frame(NEP = c(Experimental.Data$NEP, Observational.Data$NEP),
             Data.Source = rep(c("Bryophyte-Removed", "Reference"), times = c(length(Experimental.Data$NEP), length(Observational.Data$NEP))))


Reference <-
  ggplot(Data,aes(x = Data.Source, y = NEP, fill = Data.Source)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "darkgrey", linewidth = 1) +
  geom_boxplot() +
  scale_fill_manual(values = c("#8B5A2B", "#C4A484"), guide="none")+
  scale_y_continuous(name = "Net Ecosystem Production<br>(g m<sup>-2</sup>)<br>") +
  scale_x_discrete(name = "") +
  theme(axis.line = element_line(size = 0.5),
        axis.ticks = element_line(size = 0.5),
        axis.ticks.length = unit(0.25,"cm"),
        axis.title.x = element_text(size = 12, color = "black"),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.y = element_markdown(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"),
        strip.text = element_text(size = 12, color = "black"),
        legend.position = "none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA))
Reference


ggsave(filename = "Net Ecosystem Production_Bryophyte-Removal vs Reference.png",
       plot = Reference,
       width=4,
       height=5,
       dpi=600)




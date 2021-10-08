library(readxl)
library(tidyverse)
library(dplyr)
library(reshape2)
library(gganimate)

#ReadingInput
read_excel("data.xlsx")->data1

data.frame(data1)->data1
glimpse(data1)

#CleaningData
data1[-5]->data1
data.frame(data1)->data1

data1$Legal.under.any..*100->data1$Legal.under.any..
data1$Legal.only.under.certain..*100->data1$Legal.only.under.certain..
data1$Illegal.in.all..*100->data1$Illegal.in.all..
data1
colnames(data1)<-c("Year","Legal under all circumstances","Legal under certain circumstances","Illegal under all circumstances")
data1%>%melt(id.vars="Year",measure.vars=c("Legal under all circumstances","Legal under certain circumstances","Illegal under all circumstances"),
             variable.name="Opinion",value.name="Value")->data

#Setting the theme
theme_set(theme_minimal(base_size = 12, base_family = "Open Sans"))
theme_update(
  axis.ticks = element_blank(),
  panel.grid.minor = element_blank(),
  plot.title = element_text(size = 18, hjust = .5, face = "bold", color = "white"),
  plot.subtitle = element_text(size = 11, color = "snow1", face="bold"),
  plot.caption = element_text(size = 10, margin = margin(t = 15), colour = "white")
)

#PlottingData
ggplot(data, aes(x=Year,y=Value,color=Opinion))+
  geom_line(lwd=0.8)+geom_point(aes(group=Year))+
  scale_color_manual(values=c("lightpink1", "coral","red1"))+
  labs(x="",y="Percentage of people (%)", subtitle="While 19% Americans think abortion should be illegal under all circumstances, almost twice as many think abortion should be legal in all scenarios", caption = "Data: Gallup via FiveThirtyEight | Design: Annapurani Vaidyanathan (@annapurani93)")+
  theme(axis.title.y = element_text(vjust = 2, size = 12, colour = "white"),
        axis.text = element_text(size=10, colour = "white"),
        legend.background = element_rect(fill = "transparent"),
        legend.title=element_text(size = 12, face="bold", colour = "white"),
        legend.key = element_rect(fill = "transparent"),
        legend.position = "top", legend.text = element_text(size=10,colour = "white") )+
  ggtitle("WHY TEXAS'S ABORTION LAW MAY GO TOO FAR FOR MOST AMERICANS")+
  theme(panel.background = element_rect(
    fill = "black", color = "black"))+
  theme(plot.background = element_rect(fill="black", color="black"))+
  theme(panel.grid = element_blank())+
  geom_text(data=subset(data, Year==2021),
            aes(label=Value),fontface = "bold",
            hjust = .10, vjust = -.25, size=4)->gh

#Animation
gh+transition_reveal(Year, keep_last = TRUE) +
  shadow_mark()->gh

#SavingAnimation
anim_save("gh.gif",gh, fps = 10, end_pause = 30, width = 950, height=450)






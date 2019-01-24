library(XML)
library(tidyverse)
library(RcppRoll)
library(ggrepel)

#get data for broad
url <-"http://stats.espncricinfo.com/ci/engine/player/10617.html?class=1;orderby=start;template=results;type=bowling;view=match"
tables <-readHTMLTable(url, stringsAsFactors = F)

broad <- tables$`Match by match list`

broad <- broad[,-c(8,12)]

broad %>%
  transmute(Overs=as.numeric(Overs),
            Runs=as.numeric(Runs),
            Wkts=as.numeric(Wkts),
            `Start Date`=as.Date(`Start Date`,format=c("%d %b %Y"))) %>%
  filter(!is.na(Overs)) %>%
  mutate(Roll_Runs=roll_sum(Runs,10,align="right",fill="NA"),
         Roll_Wkts=roll_sum(Wkts,10,align="right",fill="NA"),
         WholeOvers=floor(Overs),
         RemainBalls=10*(Overs-WholeOvers),
         Balls=(6*Overs)+RemainBalls,
         Roll_Balls=roll_sum(Balls,5,align="right",fill="NA"),
         Roll_Strike=Roll_Balls/Roll_Wkts,
         Roll_Avg = Roll_Runs/Roll_Wkts) -> broad

broad %>%
  filter(!is.na(Roll_Avg)) %>%
  ggplot() +
  geom_line(aes(x=`Start Date`,y=Roll_Avg))

broad %>%
  filter(!is.na(Roll_Strike)) %>%
  ggplot() +
  geom_line(aes(x=`Start Date`,y=Roll_Strike))




as.Date_origin <- function(x){
  as.Date(x, origin = '1970-01-01')
}

broad %>%
  filter(!is.na(Roll_Strike)) %>%
  ggplot() +
  geom_path(aes(x=Roll_Avg,y=Roll_Strike,colour=as.integer(`Start Date`))) +
  scale_color_viridis_c(labels=as.Date_origin)

#do 10 match rolling sums ...
#use these to calculate 10 match rolling average/strike rates


players <- c("Broad","Anderson","Curran","Stokes","Rashid","Ali","Woakes")
codes <- c(10617,8608,662973,311158,244497,8917,247235)

for (number in 1:length(players)){
  code <- codes[number]
  player <- players[number]
  url <- paste0("http://stats.espncricinfo.com/ci/engine/player/",code,".html?class=1;orderby=start;template=results;type=bowling;view=match")
  table <-readHTMLTable(url, stringsAsFactors = F)
  temp <- table$`Match by match list`
  temp <- temp[,-c(8,12)]
  
  temp %>%
    transmute(Overs=as.numeric(Overs),
              Runs=as.numeric(Runs),
              Wkts=as.numeric(Wkts),
              `Start Date`=as.Date(`Start Date`,format=c("%d %b %Y"))) %>%
    filter(!is.na(Overs)) %>%
    mutate(Roll_Runs=roll_sum(Runs,10,align="right",fill="NA"),
           Roll_Wkts=roll_sum(Wkts,10,align="right",fill="NA"),
           WholeOvers=floor(Overs),
           RemainBalls=10*(Overs-WholeOvers),
           Balls=(6*Overs)+RemainBalls,
           Roll_Balls=roll_sum(Balls,5,align="right",fill="NA"),
           `Rolling Strike Rate (10games)`=Roll_Balls/Roll_Wkts,
           `Rolling Bowling Average (10games)` = Roll_Runs/Roll_Wkts) -> temp
  
  temp$name <- player
  assign(player,temp)
}


all_players <- rbind(Broad,Anderson,Ali,Curran,Rashid,Stokes,Woakes)


all_players %>%
  filter(!is.na(`Rolling Strike Rate (10games)`)) %>%
  group_by(name) %>%
  filter(`Start Date`==max(`Start Date`)) %>%
  group_by(name,`Start Date`) %>%
  filter(!is.na(`Rolling Strike Rate (10games)`)) %>%
  select(`Rolling Strike Rate (10games)`,`Rolling Bowling Average (10games)`) %>%
  gather(key="measure",value="value",-c(name,`Start Date`))  -> player_labels

all_players %>%
  group_by(name,`Start Date`) %>%
  filter(!is.na(`Rolling Strike Rate (10games)`)) %>%
  select(`Rolling Strike Rate (10games)`,`Rolling Bowling Average (10games)`) %>%
  gather(key="measure",value="value",-c(name,`Start Date`)) %>%
  ggplot() +
  facet_grid(measure ~ .,switch="both") +
  geom_line(aes(x=`Start Date`,y=value,colour=name),show.legend = FALSE) +
  scale_x_date("",limits=(as.Date(c('1/1/2015', '24/1/2019'), format="%d/%m/%Y"))) +
  geom_text_repel(data=player_labels,aes(x=`Start Date`,y=value,colour=name,label=name),show.legend = FALSE) +
  scale_colour_brewer(palette="Dark2",type="qual") +
  scale_y_continuous("") + 
  theme_minimal() +
  theme(text=element_text(colour="grey50"),
        axis.text=element_text(colour="grey50"),
        panel.grid=element_blank(),
        strip.text=element_text(colour="grey50")) +
  labs(title="Broad is less good now than in 2015-2016, but he still looks competitive with other English bowlers",
       subtitle="Measures here are calculated based on total runs/wickets/balls across the most recent 10 games played",
       caption="Data source: ESPNcricinfo - Design by @stevejburr")

ggsave("lines.png",height=12,width=12)

#create a labelling dataset with max start date by player
#use ggrepel to show on a graph

#show facetted cumulative averages / strike rates as plots
#get broad/stokes/curran? others?

#gganimate of connected scatter of two variables?
#connected scatter of two variables (facet by player?)
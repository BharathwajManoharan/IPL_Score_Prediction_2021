library(dplyr)   
library(readr) 
library(data.table)
library(sqldf)
library(ggplot2) 
library(ggthemes)
library(treemap)
library(RColorBrewer)
library(tidyr) 

d <- read.csv(file.choose() , header = T , sep = ",") # Delivery Dataset 
m <- read.csv(file.choose() , header = T , sep = ",") # Matches Dataset

str(d) # structure of the Data set 

str(m) # structure of the Data set  

i1 <- sapply(d, class) %in% c('factor')
d[i1] <-  lapply(d[i1], function(x) {if(is.factor(x)) {
  levels(x)[levels(x)=='Rising Pune Supergiant'] <- 'RPS'
}
  x})

i1 <- sapply(d, class) %in% c('factor')
d[i1] <-  lapply(d[i1], function(x) {if(is.factor(x)) {
  levels(x)[levels(x)=='Rising Pune Supergiants'] <- 'RPS'
}
  x})

i1 <- sapply(m, class) %in% c('factor')
m[i1] <-  lapply(m[i1], function(x) {if(is.factor(x)) {
  levels(x)[levels(x)=='Rising Pune Supergiant'] <- 'RPS'
}
  x})

i1 <- sapply(m, class) %in% c('factor')
m[i1] <-  lapply(m[i1], function(x) {if(is.factor(x)) {
  levels(x)[levels(x)=='Rising Pune Supergiants'] <- 'RPS'
}
  x})

data.frame(var1 = unique(d$batting_team) , var2 = unique(m$team1)) 

rt <- d %>% 
  select(batting_team ,batsman_runs) %>%
  filter(batsman_runs %in% c(6,4,3,2,1))

s <- sqldf("select batting_team as Team , count(batsman_runs) as Sixes from rt 
      where batsman_runs = 6 group by batting_team")

f <- sqldf("select batting_team as Team , count(batsman_runs) as Fours from rt 
      where batsman_runs = 4 group by batting_team")

t <- sqldf("select batting_team as Team , count(batsman_runs) as Threes from rt 
      where batsman_runs = 3 group by batting_team")

tt <- sqldf("select batting_team as Team , count(batsman_runs) as Twos from rt 
      where batsman_runs = 2 group by batting_team")

o <- sqldf("select batting_team as Team , count(batsman_runs) as Ones from rt 
      where batsman_runs = 1 group by batting_team") 


Runs_Type <- sqldf("select s.Team ,
                           s.Sixes,
                           f.Fours,
                           t.Threes,
                           tt.Twos,
                           o.Ones from s
                           inner join f
                           on s.Team = f.Team
                           inner join t 
                           on s.Team = t.Team
                           inner join tt
                           on s.Team = tt.Team
                           inner join o 
                           on s.Team = o.Team ") 

print(Runs_Type) 

p <-   d %>%  
  group_by(match_id,batting_team) %>% 
  summarise(runs = sum(total_runs)) %>%
  filter(runs >= 200)

dd <- sqldf(" select batting_team as Team, count(runs) as Scored_200_Plus from p
        group by batting_team ")

p1 <-   d%>%  
  group_by(match_id,batting_team) %>% 
  summarise(runs = sum(total_runs)) %>%
  filter(runs <= 100)

a <- sqldf(" select batting_team as Team, count(runs) as Scored_Less_100 from p1
               group by batting_team ") 


p2 <- d %>%  
  group_by(match_id,batting_team) %>% 
  summarise(runs = sum(total_runs)) %>%
  filter(runs %in% c(100:150))



p2 <- d %>%  
  group_by(match_id,batting_team) %>% 
  summarise(runs = sum(total_runs)) %>%
  filter(runs <=150 & runs >=100)



b <- sqldf(" select batting_team as Team, count(runs) as Scored_bt_100_150 from p2
               group by batting_team ")  

p3 <- d %>%  
  group_by(match_id,batting_team) %>% 
  summarise(runs = sum(total_runs)) %>%
  filter(runs %in% c(150:200))

c <- sqldf(" select batting_team as Team, count(runs) as Scored_bt_150_200 from p3
               group by batting_team ")  


p4 <- d %>%  
  group_by(match_id,batting_team) %>% 
  summarise(runs = sum(total_runs)) %>%
  filter(runs >= 250)

e <- sqldf(" select batting_team as Team, count(runs) as Scored_250_plus from p4
               group by batting_team ")  


score_table<- sqldf("select b.Team,
              a.Scored_Less_100 ,
              b.Scored_bt_100_150 ,
              c.Scored_bt_150_200,
              dd.Scored_200_Plus,
              e.Scored_250_plus
              from b 
              left join a
              on a.Team = b.Team 
              left join c
              on b.Team = c.Team 
              left join dd
              on b.Team = dd.Team 
              left join e
              on b.Team = e.Team")

score_table[is.na(score_table)]  <- 0 #Replace NA Values with zero.
score_table 

Team_Bat <- merge(x = Runs_Type , y = score_table , by = "Team") 

setnames(Team_Bat ,c("Scored_Less_100","Scored_bt_100_150","Scored_bt_150_200","Scored_200_Plus","Scored_250_plus"),
         c("<100","100-150","150-200","200+","250+")) #Renaming Columns 

table(d$total_runs)
table(d$batsman_runs)

tos <- sqldf("select batting_team Team , count(*) Ones from d where batsman_runs = 1 group by batting_team")
tt1 <- sqldf("select batting_team Team, count(*) Twos from d where batsman_runs = 2 group by batting_team")
ttt <- sqldf("select batting_team Team, count(*) Threes from d where batsman_runs = 3 group by batting_team")
tf <- sqldf("select batting_team Team, count(*) Fours from d where batsman_runs = 4 group by batting_team")
ts <- sqldf("select batting_team Team, count(*) Sixes from d where batsman_runs = 6 group by batting_team")

# how many 30+ ,50+ ,100+ 150+ scored by each team

tp <- sqldf("select match_id,batting_team Team, batsman, sum(batsman_runs) Thirty_plus
            from d group by match_id,batting_team ,batsman having Thirty_plus >= 30.0") %>%
  select(Team ,Thirty_plus) %>%
  group_by(Team) %>%
  summarise(Thirty_plus_count = n())


# sqldf("select Team , count(Thirty_plus) from tp group by Team")


fp <- sqldf("select match_id,batting_team Team, batsman, sum(batsman_runs) Fifty_plus
            from d group by match_id,batting_team ,batsman having Fifty_plus >= 50.0") %>%
  select(Team ,Fifty_plus) %>%
  group_by(Team) %>%
  summarise(Fifty_plus_count = n())


hp <- sqldf("select match_id,batting_team Team, batsman, sum(batsman_runs) Hundred_plus
            from d group by match_id,batting_team ,batsman having Hundred_plus  >= 100.0") %>%
  select(Team ,Hundred_plus ) %>%
  group_by(Team) %>%
  summarise(Hundred_plus  = n())

ofp <- sqldf("select match_id,batting_team Team, batsman, sum(batsman_runs) One_Fifty_plus
             from d group by match_id,batting_team ,batsman having One_Fifty_plus  >= 150.0") %>%
  select(Team ,One_Fifty_plus ) %>%
  group_by(Team) %>%
  summarise(One_Fifty_plus_count  = n())  

#TOP Score by team

ths <- sqldf("select match_id,batting_team Team,sum(total_runs) Team_Highest_Score
             from d group by match_id,batting_team ") %>%
  select(Team,Team_Highest_Score) %>%
  group_by(Team) %>%
  summarise(Team_Highest_Score = max(Team_Highest_Score)) 

#Balls Played By Team

bpbt <- d %>%
  select(batting_team,wide_runs,noball_runs,batsman_runs) %>%
  filter(wide_runs == 0 & noball_runs == 0)
# %>% group_by(batting_team) %>%  tally

bpbt <- sqldf("select batting_team Team, count(batsman_runs) Balls_Played from bpbt group by batting_team ")

#Dot_Balls

dbt <- d %>%
  select(batting_team,wide_runs,noball_runs,batsman_runs) %>%
  filter(wide_runs == 0 & noball_runs == 0 & batsman_runs == 0)

dbt <- sqldf("select batting_team Team, count(batsman_runs) Dot_Balls from dbt group by batting_team ")  

#merge all data frame 

Team_Analysis <- sqldf("select tos.Team,
      tos.Ones,
      tt1.Twos,
      ttt.Threes,
      tf.fours,
      ts.Sixes,
      tp.Thirty_plus_count,
      fp.Fifty_plus_count,
      hp.Hundred_plus,
      ofp.One_Fifty_plus_count,
      ths.Team_Highest_Score,
      bpbt.Balls_Played,
      dbt.Dot_Balls
      from tos
      inner join tt1
      on tos.Team = tt1.Team
      inner join ttt
      on tos.Team = ttt.Team
      inner join tf
      on tos.Team = tf.Team
      inner join ts
      on tos.Team = ts.Team
      inner join tp
      on tos.Team = tp.Team
      inner join fp
      on tos.Team = fp.Team
      left join hp
      on tos.Team = hp.Team
      left join ofp
      on tos.Team = ofp.Team
      inner join ths
      on tos.Team = ths.Team
      inner join bpbt
      on tos.Team = bpbt.Team
      inner join dbt
      on tos.Team = dbt.Team
      ")

Team_Analysis 

Team_Analysis[is.na(Team_Analysis)] <- 0  #Replace NA values wtih Zero

Team_Analysis <- rename(Team_Analysis ,"30+" = "Thirty_plus_count" ) 
Team_Analysis <- rename(Team_Analysis ,"50+" = "Fifty_plus_count" ) 
Team_Analysis <- rename(Team_Analysis ,"100+" = "Hundred_plus" ) 
Team_Analysis <- rename(Team_Analysis ,"150+" = "One_Fifty_plus_count" ) #Renaming Columns 

Team_Analysis <- merge(x = Team_Analysis , y = Team_Bat , By = "Team") 

Team_Analysis <- rename(Team_Analysis ,"THS" = "Team_Highest_Score")

Team_Analysis

# Number of matches played by each team

mp <- as.data.frame(table(m$team1) + table(m$team2))

mp <- rename(mp , Team = Var1 , matches_played = Freq) 

#Number of Matches won by each teams  
mw <- as.data.frame(table(m$winner)) %>% filter(Freq != 3)

mw <- rename(mw , Team = Var1 , matches_won = Freq) 

mw %>% group_by(Team) %>% top_n(14) 

#join both data_frame mp and mw 

table <- merge(x = mp , y = mw , by = "Team") #inner join

table$win_per <- round((table$matches_won / table$matches_played),4) * 100   

print(table) 

Toss <- sqldf("select toss_winner as Team, count(*) as Toss_Won from m group by toss_winner") 

sqldf("select * from Toss order by Toss_Won desc") 

table <- merge( x = table , y = Toss , by = "Team")  

table

table$Toss_Win_Per = round((table$Toss_Won / table$matches_played ) * 100 , 2) #calculating Toss Win percentage 

Total_Runs <- sqldf("select batting_team as Team , sum(total_runs) as Total_Runs from d group by 
                    batting_team") 


table <- merge( x = table , y = Total_Runs , by = "Team")


table$Avg_Score <- round((table$Total_Runs  / table$matches_played),2) #Team Average Score

table

Team_BAT_Analysis <- merge(x = Team_Analysis , y = table ,By = "Team") 

Team_BAT_Analysis  

setnames(Team_BAT_Analysis , c("matches_played","matches_won","win_per","Toss_Won","Toss_Win_Per","Total_Runs",
                               "Avg_Score"),c("MP","MW","MWP","TW","TWP","TRs","AS"))

Team_BAT_Analysis #Final Table  

melt(Team_BAT_Analysis) %>% 
  filter(variable %in% c("MP","TW")) %>% 
  ggplot(aes(x =  Team , y = value,fill = variable))+
  geom_bar(stat ="identity",position="dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Team" , y = "Number of Matches", 
       title = "Matches_Played : Toss_Won")

melt(Team_BAT_Analysis) %>% 
  filter(variable %in% c("Balls_Played", "Dot_Balls")) %>% 
  ggplot(aes(x =  Team , y = value,fill = variable))+
  geom_bar(stat ="identity",position="dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Team" , y = "Number of Balls", 
       title = "Balls Played vs Dot_Balls by Team") 

ggplot(data = table , aes(reorder(Team,-Avg_Score) , y = Avg_Score ,fill = Team))+
  geom_bar(stat ="identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Team" , y = "Scores", title = "Avg Team Scores in an inning") +
  geom_text(aes(label = Avg_Score),color="black",fontface = "bold",size =2.5,vjust = 1.8)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  guides(fill=F) 

melt(Team_BAT_Analysis) %>% 
  filter(variable %in% c("<100","100-150","150-200","200+","250+"))%>% 
  ggplot(aes(Team,value,fill = variable))+
  geom_bar(stat ="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ggtitle("Team Scores Frequency") 

#Power Play analysis .

pd <- d %>% 
  filter(over %in% c(1,2,3,4,5,6)) 

p_f <- sqldf("select batting_team Team ,count(total_runs) Fours from pd 
          where total_runs = 4 group by batting_team order by Fours desc")


p_s <- sqldf("select batting_team Team ,count(total_runs) Sixes from pd 
      where total_runs = 6 group by batting_team order by Sixes desc")

#highest score in power play by team

p_THS <- sqldf("select match_id ,batting_team Team ,sum(total_runs) THS  from pd group by match_id ,batting_team") %>% 
  group_by(Team) %>% 
  filter(THS == max(THS)) %>% 
  arrange(-THS) %>% 
  distinct(Team,.keep_all= TRUE) 

p_THS # Team Highest Total in power play in an inning

p_rr <- sqldf("select batting_team Team , count(ball) Balls_Played,sum(total_runs) Total_Runs
        from pd group by batting_team") %>% 
  mutate(Strike_Rate = round((Total_Runs  / Balls_Played) *100, 2) ,
         Run_Rate = round((Total_Runs)/((Balls_Played)/6),2)) %>% 
  arrange(-Strike_Rate) # it makes sense as Strike_Rate of teams directly proportional to Run Rate 


p_db <- sqldf("select batting_team Team , count(total_runs) Dot_Balls from pd 
               where total_runs =0 group by batting_team") 



P_Play_BAT_Team  <- sqldf("select p_f.Team ,
                                  p_f.Fours,
                                  p_s.Sixes, 
                                  p_THS.THS,
                                  p_rr.Balls_Played,
                                  p_rr.Total_Runs,
                                  p_rr.Strike_Rate,
                                  p_rr.Run_Rate,
                                  p_db.Dot_Balls from p_f 
                                  inner join p_s 
                                  on p_f.Team = p_s.Team 
                                  inner join p_THS
                                   on p_f.Team = p_THS.Team
                                  inner join p_rr
                                  on p_f.Team = p_rr.Team 
                                  inner join p_db 
                                   on p_f.Team = p_db.Team")


setnames(P_Play_BAT_Team , c("Fours","Sixes","THS","Balls_Played","Total_Runs","Strike_Rate","Run_Rate","Dot_Balls"),
         c("P_Fours","P_Sixes","P_THS","P_Balls_Played","P_Total_Runs","P_Strike_Rate","P_Run_Rate","P_Dot_Balls"))

P_Play_BAT_Team #how well team batted in power play 

#Death over Analysis : overs 16 to 20 

dd <- d %>% 
  filter(over %in% c(16,17,18,19,20)) 

d_f <- sqldf("select batting_team Team ,count(total_runs) Fours from dd 
          where total_runs = 4 group by batting_team order by Fours desc")


d_s <- sqldf("select batting_team Team ,count(total_runs) Sixes from dd 
      where total_runs = 6 group by batting_team order by Sixes desc")



d_THS <- sqldf("select match_id ,batting_team Team ,sum(total_runs) THS  from dd group by match_id ,batting_team") %>% 
  group_by(Team) %>% 
  filter(THS == max(THS)) %>% 
  arrange(-THS) %>% 
  distinct(Team,.keep_all= TRUE) 

d_THS

d_rr <- sqldf("select batting_team Team , count(ball) Balls_Played,sum(total_runs) Total_Runs
        from dd group by batting_team") %>% 
  mutate(Strike_Rate = round((Total_Runs  / Balls_Played) *100, 2) ,
         Run_Rate = round((Total_Runs)/((Balls_Played)/6),2)) %>% 
  arrange(-Strike_Rate) # it makes sense as Strike_Rate of teams directly proportional to Run Rate  


d_db <- sqldf("select batting_team Team , count(total_runs) Dot_Balls from dd 
               where total_runs =0 group by batting_team") 

D_Over_BAT_Team  <- sqldf("select d_f.Team ,
                                  d_f.Fours,
                                  d_s.Sixes, 
                                  d_THS.THS,
                                  d_rr.Balls_Played,
                                  d_rr.Total_Runs,
                                  d_rr.Strike_Rate,
                                  d_rr.Run_Rate,
                                  d_db.Dot_Balls from d_f 
                                  inner join d_s 
                                  on d_f.Team = d_s.Team 
                                  inner join d_THS
                                   on d_f.Team = d_THS.Team
                                  inner join d_rr
                                  on d_f.Team = d_rr.Team 
                                  inner join d_db 
                                   on d_f.Team = d_db.Team")

setnames(D_Over_BAT_Team, c("Fours","Sixes","THS","Balls_Played","Total_Runs","Strike_Rate","Run_Rate","Dot_Balls"),
         c("D_Fours","D_Sixes","D_THS","D_Balls_Played","D_Total_Runs","D_Strike_Rate","D_Run_Rate","D_Dot_Balls"))

D_Over_BAT_Team #How well team batted in last Five Overs 

PD_BAT_Team <- merge(x = P_Play_BAT_Team , y = D_Over_BAT_Team , by = "Team") 

PD_BAT_Team

#Boundaries and Sixes hit by team in (Power Play  Vs Death Overs) 
melt(PD_BAT_Team) %>% 
  filter(variable %in% c("P_Fours","D_Fours")) %>% 
  ggplot(aes(x =  Team , y = value,fill = variable))+
  geom_bar(stat ="identity",position="dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#Highest Score by team in (Power Play  Vs Death Overs) 
melt(PD_BAT_Team) %>% 
  filter(variable %in% c("P_THS","D_THS")) %>% 
  ggplot(aes(x =  Team , y = value,fill = variable))+
  geom_bar(stat ="identity",position="dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

melt(PD_BAT_Team) %>% 
  filter(variable %in% c("P_Total_Runs","D_Total_Runs")) %>% 
  ggplot(aes(x =  Team , y = value,fill = variable))+
  geom_bar(stat ="identity",position="dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

melt(PD_BAT_Team) %>% 
  filter(variable %in% c("P_Run_Rate","D_Run_Rate")) %>% 
  ggplot(aes(x =  Team , y = value,fill = variable))+
  geom_bar(stat ="identity",position="dodge") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

csk1 <- m %>% 
  select(team1,team2,winner) %>% 
  filter(team1 != 'Chennai Super Kings' & winner == 'Chennai Super Kings' ) %>% 
  group_by(team1) %>% 
  summarise(c1 = n()) 

csk2 <- m %>% 
  select(team1,team2,winner) %>% 
  filter(team2 != 'Chennai Super Kings' & winner == 'Chennai Super Kings' ) %>% 
  group_by(team2) %>% 
  summarise(c2 = n()) 

CSK <- merge(x = csk1 , y = csk2 ,by.x = 'team1' , by.y = 'team2',all = T ) ;
CSK[is.na(CSK)] <- 0
CSK <- rename(CSK , Team = team1)

CSK <- CSK %>% 
  mutate (CSK = c1 + c2 ) %>% 
  select(Team ,CSK) 

CSK

treemap(CSK ,                           #  Data Frame
        index = "Team" ,                 #  variables  
        vSize = "CSK",                   
        type = "index" ,                 
        palette = "Set1",                # color selection
        title = "CSK wins against Team", # title of the graph
        fontsize.title=08,               # font size of the title
        fontface.labels="bold",
        bg.labels=rep("transparent", nrow(CSK)),    
        border.col=c("white"),           # border color 
        border.lwds=c(1,1),              # width of the border
        fontsize.labels=c(8,0),          # font size of names appeared in box
        fontcolor.labels=c("black"),
        aspRatio= 0.8,
        fontfamily.title = "serif") 
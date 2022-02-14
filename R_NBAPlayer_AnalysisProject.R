install.packages("reshape2")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggstatsplot")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("ggpubr")
library(ggstatsplot)
library(reshape2)

library(ggplot2)
library(ggplot)
library(tidyverse)

library(tidyr)
library(dplyr)
library(ggpubr)

nba2122 = read.csv("C:\\Users\\rauna\\Downloads\\2021_2022_NBA_Player_Stats.csv")
summary(nba2122)
colnames(nba2122)= c("Num","Player","Pos", "Age","Team","GPlay","GStart","MPlay","FGM","FGA",
                     "FGPer","3PM","3PA","3PPer","2PM","2PA","2P","eFGPer","FTA","FTM",
                     "FTPer","ORB","DRB","TRB","AST","STL","BLK","TOS","PF","PTS")

nba2122[30]

sapply(nba2122,sd)
##nba2122_cleaned contains data for players who have played atleast 10 games and average atleast 12 mins a game.

nba2122_cleaned = dplyr::filter(nba2122, nba2122$GPlay > 13 & nba2122$MPlay > 15)

nba2122_cleaned

plot(nba2122_cleaned$MPlay,nba2122_cleaned$Age,main ="Relationship between Minutes Played and Age",xlab="Minutes Played",ylab="Age")
abline(lm(nba2122$Age~nba2122$MPlay), col="red") # regression line (y~x

r2agemplay = lm(nba2122$Age~nba2122$MPlay)
summary(r2agemplay)
?abline
?lm()
## added an efficieny column

nba2122_cleaned %>% mutate(Eff = NA)


nba2122_cleaned$Eff = ((nba2122_cleaned$PTS + nba2122_cleaned$TRB + nba2122_cleaned$AST +nba2122_cleaned$STL - nba2122_cleaned$TOS + nba2122_cleaned$BLK - (nba2122_cleaned$FGA - nba2122_cleaned$FGM)- (nba2122_cleaned$FTA - nba2122_cleaned$FTM)))  

nba2122_cleaned$Eff


nba2122_cleaned



ggplot(data = nba2122_cleaned, aes(x = Eff, y = Age)) +
  geom_point()+ geom_smooth(method = "lm") + ggtitle("Efficiency vs Age")+xlab("Efficiency")+ ylab("Age")+ geom_text(
    label=rownames(nba2122_cleaned), 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T) + stat_regline_equation(label.y = 52, aes(label = ..eq.label..))+stat_regline_equation(label.y =47 , aes(label = ..rr.label..))

nba2122_cleaned[2]

ggplot(data = nba2122_cleaned, aes(x = Eff, y = GStart)) +
  geom_point()+ geom_smooth(method = "lm") + ggtitle("Efficiency vs Games Started")+xlab("Efficiency")+ ylab("Games Started") +
  geom_text(
    label=rownames(nba2122_cleaned), 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T)+
  stat_regline_equation(label.y = 80, aes(label = ..eq.label..)) +stat_regline_equation(label.y =75 , aes(label = ..rr.label..))


ggplot(data = nba2122_cleaned, aes(x = Eff, y = MPlay)) +
  geom_point()+ geom_smooth(method = "lm") +  ggtitle("Efficiency vs Minutes Played")+xlab("Efficiency")+ ylab("Minutes Played")+
  geom_text(
    label=rownames(nba2122_cleaned), 
    nudge_x = 0.25, nudge_y = 0.25, 
    check_overlap = T)+
  stat_regline_equation(label.y = 55, aes(label = ..eq.label..)) +stat_regline_equation(label.y =50 , aes(label = ..rr.label..))



#removing rows with non numeric values 
cormatprep = na.omit(nba2122_cleaned[-c(1:3,5)])
cormatprep

#created a correlation matrix
nba_cormat = round(cor(cormatprep),2)
nba_cormat

#created 2 vectors Var 1 and Var2 using melt function to assist in building the matrix
nba_melt_cormat = melt(nba_cormat)

nba_melt_cormat


#create the matrix
ggplot(data = nba_melt_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

get_upper_tri <- function(nba_cormat){
  nba_cormat[lower.tri(nba_cormat)]<- NA
  return(nba_cormat)
}

upper_tri = get_upper_tri(nba_cormat)

upper_tri

nba_melt_cormat2 <- melt(upper_tri, na.rm = TRUE)

nba_melt_cormat2  

ggplot(data = nba_melt_cormat2, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 70, vjust = 1, 
                                   size = 8, hjust = 1))+
  coord_fixed()

##re order matrix for better visualization


## Group nba data by teams and return total eff to predict championship winner
## Calculated mean efficiency rating per team for later use

team_eff = nba2122_cleaned %>% group_by(Team) %>% summarise(total = sum(Eff))
team_eff

ggplot(data = team_eff,aes(x = total, y = Team))+
  geom_point() +  ggtitle("Total Efficiency Per Team")+xlab("Total Team Efficiency")+ ylab("Team Name")
  



meaneff = sapply(team_eff,mean)
meaneff
## 153.73 = mean Eff for all teams

split_by_teams



## get player efficiency for top 8 players with most minutes per team 

nba_sortedbymins =nba2122_cleaned %>% arrange(desc(nba2122_cleaned$MPlay)) 
nba_sortedbymins


nba_splitbyteams = split(nba_sortedbymins, f = nba_sortedbymins$Team, drop = FALSE)
nba_splitbyteams
nba_splitbyteams[[1]][31]

listofteameff = list(
    "ATL" = nba_splitbyteams[[1]][31],
    "BOS" = nba_splitbyteams[[2]][31],
    "BRK" = nba_splitbyteams[[3]][31],
    "CHI" = nba_splitbyteams[[4]][31],
    "CHO" = nba_splitbyteams[[5]][31],
    "CLE" = nba_splitbyteams[[6]][31],
    "DAL" = nba_splitbyteams[[7]][31],
    "DEN" = nba_splitbyteams[[8]][31],
    "DET" = nba_splitbyteams[[9]][31],
    "GSW" = nba_splitbyteams[[10]][31],
    "HOU" = nba_splitbyteams[[11]][31],
    "IND" = nba_splitbyteams[[12]][31],
    "LAC" = nba_splitbyteams[[13]][31],
    "LAL" = nba_splitbyteams[[14]][31],
    "MEM" = nba_splitbyteams[[15]][31],
    "MIA" = nba_splitbyteams[[16]][31],
    "MIL" = nba_splitbyteams[[17]][31],
    "MIN" = nba_splitbyteams[[18]][31],
    "NOP" = nba_splitbyteams[[19]][31],
    "NYK" = nba_splitbyteams[[20]][31],
    "OKC" = nba_splitbyteams[[21]][31],
    "ORL" = nba_splitbyteams[[22]][31],
    "PHI" = nba_splitbyteams[[23]][31],
    "PHX" = nba_splitbyteams[[24]][31],
    "POR" = nba_splitbyteams[[25]][31],
    "SAC" = nba_splitbyteams[[26]][31],
    "SAS" = nba_splitbyteams[[27]][31],
    "TOR" = nba_splitbyteams[[28]][31],
    "UTA" = nba_splitbyteams[[29]][31],
    "WAS" = nba_splitbyteams[[30]][31]
)
nba_starter_eff = rep(0,30)

for(i in seq(from = 1, to = 30, by = 1)){nba_starter_eff[i]=sum(nba_splitbyteams[[i]][1:8,c(31)])}
nba_trueStarterEff = nba_starter_eff[1:30]

nba_trueStarterEff

nba_starter_eff

for (i in seq(from = 1, to = length(listofteams), by = 1)){
  nba_starter_eff[i] = sum (listofteams[i]$Eff[1:8])
}
row1
row1= c("ATL",
        "BOS",
        "BRK",
        "CHI",
        "CHO",
        "CLE",
        "DAL",
        "DEN",
        "DET",
        "GSW",
        "HOU",
        "IND",
        "LAC",
        "LAL",
        "MEM",
        "MIA",
        "MIL",
        "MIN",
        "NOP",
        "NYK",
        "OKC",
        "ORL",
        "PHI",
        "PHX",
        "POR",
        "SAC",
        "SAS",
        "TOR",
        "UTA",
        "WAS"
)

nba_starter_eff
nba_starters = matrix(data = row1,nrow= 30,ncol = 1)
nba_starters

nba_startersF = rbind(row1,nba_starter_eff)
nba_startersF

nba_EFFStarters = data.frame(row1,nba_starter_eff)
nba_EFFStarters
ggplot(data = nba_EFFStarters,aes(x = nba_starter_eff, y = row1))+
  geom_point()+  ggtitle("Eight Player Per Team")+xlab("Eight Player Efficiecy")+ ylab("Team Name")


## Use a statistical test to see if deep subs could make a potential difference 
## team_eff = total efficiency for an NBA team including subs
## nba_EFFStarters = total efficiency for an NBA team with starters and 3 subs with most minutes

## Null Hypothesis: u(team_eff) = u(nba_EFFStarters)
## Alternate Hypothesis : u(team_eff)!= u(nba_EFFStarters)


summary(team_eff)
summary(nba_EFFStarters)
sapply(team_eff,sd)
sapply(nba_EFFStarters,sd)


set.seed(15)
stat_team_eff = rnorm(30,153.7,13.72)

stat_nba_EFFStarters = rnorm(30,130.4,10.00)


ggplot() + 
  geom_density(data = stat_team_eff, aes(x = x), 
               fill = "#E69F00", color = "black", alpha = 0.7) + 
  geom_density(data = stat_nba_EFFStarters, aes(x = y),
               fill = "#56B4E9", color = "black", alpha = 0.7)+ ggtitle("Density Plot of 8 Player Eff and All Players Eff")+xlab("Total Efficiecy")+ ylab("Density")


plot(density(stat_team_eff), xlab = "Total Team Eff.",
     main = "Sample Distribution of Total Efficiency", col = "red")
axis(2,cex.axis=2)
lines(density(stat_nba_EFFStarters), col = "blue")
legend("topright", c("Total Team Efficiency", "Starter Efficiency"), lwd = 1, col = c("red", "blue"))

boxplot(stat_team_eff, stat_nba_EFFStarters, main = "Sample Distribution of Total Efficiency",
        col = c("red", "blue"), names = c("Total Team Efficiency", "Starter Efficiency"))

t.stat = (mean(stat_team_eff) - mean(stat_nba_EFFStarters))/sqrt(var(stat_team_eff))/100
t.stat                                                                 

t.test(stat_team_eff, stat_nba_EFFStarters, alternative = "two.sided")

f.stat = var(stat_team_eff)/var(stat_nba_EFFStarters) #Manually calculating the F-statistic.
f.stat

var.test(stat_team_eff, stat_nba_EFFStarters, alternative = "two.sided")

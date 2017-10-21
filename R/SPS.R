library(dplyr)
library(reshape2)
library(lubridate)

sps <- read.csv("SPS_CSV_OCT302015.csv")
streams <- read.csv("rivers_ordered.csv")
label_short <- function(variable, value){
  snames <- c ("Casc E. Slp Trib","U. Col./E Slp Casc.","Yakima","Imnaha","Gr.Ronde/Imnaha","Stanley","U Salmon","M Fk Salmon","S Fk Salmon","Grande Ronde","Snake River","L. Snake","Umatilla/Walla Walla","John Day")
  return(snames[as.numeric(value)])
}
sps$Stream <- factor(sps$Stream,levels=streams$name)
sps$Major.Population.Group <- factor(sps$Major.Population.Group,levels=c("Cascades Eastern Slope Tributaries","Upper Columbia / East Slope Cascades","Yakima River Group","Imnaha River","Grande Ronde / Imnaha","Stanley Basin","Upper Salmon River","Middle Fork Salmon River","South Fork Salmon River","Grande Ronde River","Snake River","Lower Snake","Umatilla And Walla Walla River","John Day River"))
sps.agg <- summarise(group_by(sps,Year,Major.Population.Group,Species),count=sum(Spawners))

#generate dot plot showing independently scaled spawner counts
sps$SR <- as.factor(paste(as.character(sps$Species),as.character(sps$Run)))
sps1 <- merge(sps,summarise(group_by(sps,Stream,SR),smax=max(Spawners)))
ggplot(sps1,aes(Year,Stream,size=Spawners/smax,colour=SR)) + geom_point()

sps.long <- melt(sps,id.vars=c("Major.Population.Group","Species","Run","Stream","Year","FracWild","Spawners"),measure.var=c(15,16,17,18,19,20,21),variable.name="Age.Group",value.name="Age.Fraction")
sps.long <- filter(sps.long,Age.Fraction > 0, FracWild > 0, Spawners>0)
sps.long$Count <- sps.long$FracWild*sps.long$Age.Fraction*sps.long$Spawners
sps.long$Brood <- sps.long$Year-as.numeric(sps.long$Age.Group)
sps.long.agg <- summarise(group_by(sps.long,Species,Major.Population.Group,Year,Age.Group),Count=sum(Count))
ggplot(filter(sps.long.agg,Species=="Chinook"),aes(Year,Count,fill=Age.Group)) + geom_bar(stat="identity") + facet_grid(Major.Population.Group ~ .,scale="free",labeller=label_short)  + ggtitle("Chinook Spawners By Age Group") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) + xlab("Spawn Year") + ylab("Spawner Count")
sps.long.agg2 <- summarise(group_by(sps.long,Species,Major.Population.Group,Brood,Age.Group),Count=sum(Count))

ggplot(filter(sps.long.agg2,Species=="Chinook"),aes(Brood,Count,fill=Age.Group,order=Age.Group)) + geom_bar(stat="identity") + facet_grid(Major.Population.Group ~ .,scale="free",labeller=label_short) + ggtitle("Chinook Recruits by Age at Return") + 
  theme(plot.title = element_text(lineheight=.8, face="bold"))+ xlab("Brood Year") + ylab("Recruit Count")
#get dataframe of spawner counts
spawners <- summarise(group_by(sps.long,Species,Major.Population.Group,Year),Spawners=sum(Count))
names(spawners) <- c("Species","Major.Population.Group","Brood","Spawners")
sps.long.agg2a <- merge(sps.long.agg2,spawners)
sps.long.agg2a$Productivity <- sps.long.agg2a$Count/sps.long.agg2a$Spawners

ggplot(filter(sps.long.agg2a,Species=="Chinook"),aes(Brood,Productivity,fill=Age.Group,order=Age.Group)) + geom_bar(stat="identity") + facet_grid(Major.Population.Group ~ .,scale="free",labeller=label_short) + ggtitle("Chinook Productivity") + 
  theme(plot.title = element_text(lineheight=.8, face="bold")) + xlab("Brood Year")



ggplot(filter(sps.agg,Species == "Chinook"),aes(Year,count)) + geom_area() + facet_grid(Major.Population.Group ~ .,scale="free")
sps$SR <- as.factor(paste(as.character(sps$Species),as.character(sps$Run)))

sps1 <- data.frame(cpop = as.numeric(sps$Common.Population.Name),nwrpop = as.numeric(sps$NWR.Population.Name),mpop = as.numeric(sps$Major.Population.Group),esu = as.numeric(sps$ESU),species=as.numeric(sps$Species),year=sps$Year,spawners=sps$Spawners,fracwild=sps$FracWild,ecatch=sps$Effective.Catch,returns1=sps$Age.1.Returns,returns2=sps$Age.2.Returns,returns3=sps$Age.3.Returns,returns4=sps$Age.4.Returns,returns5=sps$Age.5.Returns)
#View(data.frame(levels(sps$Common.Population.Name)))
sps1$returns1 <- ifelse(sps1$returns1==99,0,sps1$returns1)
sps1$returns2 <- ifelse(sps1$returns2==99,0,sps1$returns2)
sps1$returns3 <- ifelse(sps1$returns3==99,0,sps1$returns3)
sps1$returns4 <- ifelse(sps1$returns4==99,0,sps1$returns4)
sps1$returns5 <- ifelse(sps1$returns5==99,0,sps1$returns5)
write.csv(sps1,"SPS_data.csv",row.names=FALSE)







ggplot(filter(sps,FracWild >=0),aes(Year,Stream,size=Spawners,colour=SR)) + geom_point()
ggplot(filter(sps,Stream != "Snake River"),aes(Year,Stream,size=Spawners,colour=SR)) + geom_point()
ggplot(filter(sps,Species == "Chinook",Stream != "Snake River"),aes(Year,Stream,size=Spawners,colour=SR)) + geom_point()
sps1 <- merge(sps,summarise(group_by(sps,Stream,SR),smax=max(Spawners)))
ggplot(sps1,aes(Year,Stream,size=Spawners/smax,colour=SR)) + geom_point()

getRecruits <- function(df){
  df[is.na(df)] <- -99
  recruits <- data.frame()
  for(i in 1:nrow(df)){
    if(df$Spawners[i] & df$FracWild[i] > 0) {
      spawners <- df$FracWild[i]*df$Spawners[i]
      if(df$Age.1.Returns[i] >0)recruits <- rbind(recruits,data.frame(Species=df$Species[i],Run=df$Run[i],Stream=df$Stream[i],Year=df$Year[i]-1,Ryear=df$Year[i],count=df$Age.1.Returns[i]*spawners))
      if(df$Age.2.Returns[i] >0)recruits <- rbind(recruits,data.frame(Species=df$Species[i],Run=df$Run[i],Stream=df$Stream[i],Year=df$Year[i]-2,Ryear=df$Year[i],count=df$Age.2.Returns[i]*spawners))
      if(df$Age.3.Returns[i] >0)recruits <- rbind(recruits,data.frame(Species=df$Species[i],Run=df$Run[i],Stream=df$Stream[i],Year=df$Year[i]-3,Ryear=df$Year[i],count=df$Age.3.Returns[i]*spawners))
      if(df$Age.4.Returns[i] >0)recruits <- rbind(recruits,data.frame(Species=df$Species[i],Run=df$Run[i],Stream=df$Stream[i],Year=df$Year[i]-4,Ryear=df$Year[i],count=df$Age.4.Returns[i]*spawners))
      if(df$Age.5.Returns[i] >0)recruits <- rbind(recruits,data.frame(Species=df$Species[i],Run=df$Run[i],Stream=df$Stream[i],Year=df$Year[i]-5,Ryear=df$Year[i],count=df$Age.5.Returns[i]*spawners))
      if(df$Age.6.Returns[i] >0)recruits <- rbind(recruits,data.frame(Species=df$Species[i],Run=df$Run[i],Stream=df$Stream[i],Year=df$Year[i]-6,Ryear=df$Year[i],count=df$Age.6.Returns[i]*spawners))
      if(df$Age.7.Returns[i] >0)recruits <- rbind(recruits,data.frame(Species=df$Species[i],Run=df$Run[i],Stream=df$Stream[i],Year=df$Year[i]-7,Ryear=df$Year[i],count=df$Age.7.Returns[i]*spawners))
    }
  }
  return(recruits)
}

recruits1 <- summarise(group_by(recruits,Species,Run,Stream,Year),Recruits=round(sum(count)))
sps2 <- merge(sps1,recruits1)
sps2$Recruits.Spawner <- sps2$Recruits/sps2$Spawner

#Generate csv file for web tool
sps <- read.csv("SPS_CSV_OCT302015.csv")
sps1 <- select(sps,4,6,7,8,11,12,13,14,17,18,19)
names(sps1) <- c("Pop","Species","Run","Stream","Year","Spawners","FracWild","Catch","Age3","Age4","Age5")
sps1 <- filter(sps1,Spawners >= 0)
sps1 <- filter(sps1,FracWild != -99, !is.na(Catch))
sps1$Age3 <- ifelse(sps1$Age3==-99,0,sps1$Age3)
sps1$Age4 <- ifelse(sps1$Age4==-99,0,sps1$Age4)
sps1$Age5 <- ifelse(sps1$Age5==-99,0,sps1$Age5)
sps1$Run <- ifelse(sps1$Run=="","Summer-run",as.character(sps1$Run))
sps1$Catch <- ifelse(sps1$Catch==-99,0,sps1$Catch)
write.csv(sps1,"SPSa.csv",row.names=FALSE)

sps1a <- sps1
sps1a$Ret3 <- sps1a$Spawners*sps1a$FracWild*sps1a$Age3
Ret3 <- cbind(select(sps1,1,2,3,4),Ret3)
Ret4 <- data.frame(Year=sps1$Year-4,Ret4=sps1$Spawners*sps1$FracWild*sps1$Age4)
Ret4 <- cbind(select(sps1,1,2,3,4),Ret4)
Ret5 <- data.frame(Year=sps1$Year-5,Ret5=sps1$Spawners*sps1$FracWild*sps1$Age5)
Ret5 <- cbind(select(sps1,1,2,3,4),Ret5)
sps1a <- merge(sps1,Ret3,all.x=TRUE)
sps1a <- merge(sps1a,Ret4,all.x=TRUE)
sps1a <- merge(sps1a,Ret5,all.x=TRUE)
write.csv(sps1a,"SPSb.csv",row.names=FALSE)
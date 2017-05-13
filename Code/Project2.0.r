library(tidyverse)
library(corrplot)
library(ggplot2)
library(ggthemes)


worldcup <-read.csv("~/R materials/WorldCupGoalsProject/Data/Worldcupdata.csv", header = FALSE)
header <-c("Team","Year",	"Goals",	"Temperature",	"Humidity",	"ShotAccuracy",	"ShortRate",	"LongRate",	"OpponentYellow",	"OpponentRed", "Fouled",	"BallPossession", "Opponent", "TotalShots", "MediumRate", "PossessionInOpponent3rd", "CornerKicks")
names(worldcup)<-header
attach(worldcup)

correlationplot <- worldcup %>% select( -Team, -Year, -Opponent) %>% cor %>% corrplot(method = "number")


Model.1a<-lm(Goals~Temperature+Humidity+TotalShots+ShotAccuracy+CornerKicks+ShortRate+MediumRate+LongRate+Fouled+BallPossession+PossessionInOpponent3rd+OpponentYellow+OpponentRed)
summary(Model.1a)

library(MASS)
# Forward:
forwardmod <- stepAIC(lm(Goals ~ Temperature + Humidity + TotalShots + ShotAccuracy + CornerKicks+
             ShortRate + MediumRate + LongRate + Fouled + BallPossession + 
             PossessionInOpponent3rd+OpponentYellow+OpponentRed), direction = "forward")
summary(forwardmod)

# Backward:
backwardmod <- stepAIC(lm(Goals ~ Temperature + Humidity + TotalShots + ShotAccuracy + CornerKicks+
                           ShortRate + MediumRate + LongRate + Fouled + BallPossession + 
                           PossessionInOpponent3rd+OpponentYellow+OpponentRed), direction = "backward")
summary(backwardmod)

# Stepwise:
stepwisemod <- stepAIC(lm(Goals ~ Temperature + Humidity + TotalShots + ShotAccuracy + CornerKicks+
                           ShortRate + MediumRate + LongRate + Fouled + BallPossession + 
                           PossessionInOpponent3rd+OpponentYellow+OpponentRed), direction = "both")

summary(stepwisemod)

# Extra created variables:
worldcup %>% mutate(AvgPassRate = (ShortRate + MediumRate + LongRate)/3)

worldcup %>% mutate(Cards = (OpponentYellow + OpponentRed))

worldcup %>% mutate(ShotsSqrd = (TotalShots^2))

worldcup %>% mutate(FouledSqrd = (Fouled^2))

worldcup %>% mutate(CornerKickSqrd = (CornerKicks^2))




Result1aa<-lm(Goals~TotalShots+ShotAccuracy+CornerKicks+ShortRate+MediumRate+LongRate+Fouled+BallPossession+PossessionInOpponent3rd+OpponentYellow+OpponentRed)
summary(Result1aa)

aov(Result1a)
aov(Result1aa)
cor(Temperature, ShortRate)
cor(Temperature, MediumRate)
cor(Temperature, LongRate)
cor(Temperature,Humidity)
cor(ShortRate,MediumRate)
cor(MediumRate,LongRate)
cor(ShortRate,LongRate)

Result1ab<-lm(Goals~Temperature+Humidity+TotalShots+ShotAccuracy+BallPossession+PossessionInOpponent3rd)
summary(Result1ab)

Result1ac<-lm(Goals~Temperature+Humidity+TotalShots+ShotAccuracy+CornerKicks+ShortRate+LongRate+Fouled+BallPossession)
summary(Result1ac)

cor(BallPossession,PossessionInOpponent3rd)
cor(PossessionInOpponent3rd, Humidity)
cor(PossessionInOpponent3rd, Temperature)
cor(PossessionInOpponent3rd, TotalShots)
cor(PossessionInOpponent3rd,ShotAccuracy)
cor(PossessionInOpponent3rd, CornerKicks)
cor(PossessionInOpponent3rd, OpponentYellow)
cor(PossessionInOpponent3rd,OpponentRed)
cor(PossessionInOpponent3rd, Fouled)
cor(PossessionInOpponent3rd,ShortRate)
cor(PossessionInOpponent3rd,MediumRate)
cor(PossessionInOpponent3rd,LongRate)

Cards<-OpponentYellow+OpponentRed
#
Result1ad<-lm(Goals~Temperature+Humidity+TotalShots+ShotAccuracy+CornerKicks+ShortRate+MediumRate+LongRate+Fouled+BallPossession+PossessionInOpponent3rd+Cards)
summary(Result1ad)
cor(Cards, Temperature)

AvgPassingRate <-(ShortRate+MediumRate+LongRate)/3
#
Result1ae<-lm(Goals~Temperature+Humidity+TotalShots+ShotAccuracy+CornerKicks+AvgPassingRate+Fouled+BallPossession+PossessionInOpponent3rd+OpponentYellow+OpponentRed)
summary(Result1ae)

Result1af<-lm(Goals~Temperature+Humidity+TotalShots+ShotAccuracy+CornerKicks+AvgPassingRate+Fouled+BallPossession+PossessionInOpponent3rd+Cards)
summary(Result1af)

cor(Cards,AvgPassingRate)
cor(AvgPassingRate,Temperature)
cor(Cards,Temperature)
#
TotalShotssq<-TotalShots^2
CornerKickssq<-CornerKicks^2
Fouledsq<-Fouled^2
#
Result1ag<-lm(Goals~Temperature+Humidity+TotalShots+TotalShotssq+ShotAccuracy+CornerKicks+CornerKickssq+AvgPassingRate+Fouled+Fouledsq+BallPossession+PossessionInOpponent3rd+Cards)
summary(Result1ag)

Result1ag<-lm(Goals~Temperature+TotalShots+ShotAccuracy+PossessionInOpponent3rd)
summary(Result1ag)

mydata1subset2010<-subset(mydata1, Year ==2010, select = x)
detach(mydata1)
attach(mydata1subset2010)
Year

Result1ba<-lm(Goals~Temperature+Humidity+TotalShots+ShotAccuracy+CornerKicks+ShortRate+MediumRate+LongRate+Fouled+BallPossession+PossessionInOpponent3rd+OpponentYellow+OpponentRed)
summary(Result1ba)

Result1bb<-lm(Goals~Temperature+Humidity+TotalShots+ShotAccuracy+CornerKicks+AvgPassingRate+Fouled+BallPossession+PossessionInOpponent3rd+Cards)
summary(Result1bb)

Result1bc<-lm(Goals~Temperature+TotalShots+ShotAccuracy+PossessionInOpponent3rd)
summary(Result1bc)

mydata1subset2014<-subset(mydata1, Year ==2014, select = x)
detach(mydata1)
attach(mydata1subset2014)
Year

Result1ca<-lm(Goals~Temperature+Humidity+TotalShots+ShotAccuracy+CornerKicks+ShortRate+MediumRate+LongRate+Fouled+BallPossession+PossessionInOpponent3rd+OpponentYellow+OpponentRed)
summary(Result1ca)

Result1cb<-lm(Goals~Temperature+Humidity+TotalShots+ShotAccuracy+CornerKicks+AvgPassingRate+Fouled+BallPossession+PossessionInOpponent3rd+Cards)
summary(Result1cb)

Result1cc<-lm(Goals~Temperature+TotalShots+ShotAccuracy+PossessionInOpponent3rd)
summary(Result1cc)

cor(Temperature,AvgPassingRate)
cor(Temperature, Cards)
cor(Temperature,TotalShots)

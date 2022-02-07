#input births/deaths/population datasets
b <- read.csv(file.choose())
d <- read.csv(file.choose())
p <- read.csv(file.choose())

#Removing the predictions from this dataset so we can use actual data
births <- b[1:72,]
births$year <- c(1950:2021)
births <- births[,-1]

deaths <- d[1:72,]
deaths$year <- c(1950:2021)
deaths <- deaths[,-1]

population <- p[1:72,-1]
population$year <- c(1950:2021)

currYears <- c(1950:2021)
futureYears <- c(2022:2122)

#############################
# Autoregression for deaths #
#############################
library(dynlm)

deathparam <- deaths$Deaths.per.1000.People

#Some code cause i dont beleive in the function but it was actually correct
#So i should have more faith in the internet
  #testo <- deaths$Deaths.per.1000.People[c(-1,-2)]
  #test1l <- deaths$Deaths.per.1000.People[c(-1,-72)]
  #test2l <- deaths$Deaths.per.1000.People[c(-71,-72)]
  #testreg <- lm(testo ~ test1l + test2l)
  #summary(testreg) #this is equal to the dynlm

#Use the BIC function to find out how many lags to include 
order <- 1:6
dBICs <- sapply(order, function(x) 
  "AR" = BIC(dynlm(ts(deathparam) ~ L(ts(deathparam), 1:x)))) 
dBICs # AR(2) is has the lowest BIC and AIC

#Fit an AR(2) model on deathparam (deaths per 1000 people)
deathreg <- dynlm(ts(deathparam)~L(ts(deathparam))+L(ts(deathparam),2))
summary(deathreg)
plot(deathreg)

#Equation is y = dintercept + dlag1 + dlag2, 
#i.e deathIn2020 = intercept + deathIn2019 + deathIn2018, with coefficcents
dintercept <- deathreg$coefficients[1]
dlag1 <- deathreg$coefficients[2]
dlag2 <- deathreg$coefficients[3]

ddf <- data.frame(year=integer(),dp1000=double())
pred2022 <- dintercept + dlag1*(deathparam[72]) + dlag2*(deathparam[71])
ddf[nrow(ddf) + 1,] = c(2022,pred2022)
pred2023 <- dintercept + dlag1*(ddf$dp1000[1]) + dlag2*(deathparam[72])
ddf[nrow(ddf) + 1,] = c(2023,pred2023)

for(x in 3:101){
  year <- x+2021
  prediction <- dintercept + dlag1*(ddf$dp1000[x-1]) + dlag2*(ddf$dp1000[x-2])
  ddf[nrow(ddf) + 1,] = c(year,prediction)
}
ddf



#############################
# Autoregression for births #
#############################
birthparam <- births$Births.per.1000.People

bBICs <- sapply(order, function(x) 
  "AR" = BIC(dynlm(ts(birthparam) ~ L(ts(birthparam), 1:x)))) 
bBICs # AR(2) is has the lowest BIC and AIC

#Fit an AR(2) model on birthparam (births per 1000 people)
birthreg <- dynlm(ts(birthparam)~L(ts(birthparam))+L(ts(birthparam),2))
summary(birthreg)
plot(birthreg)


bintercept <- birthreg$coefficients[1]
blag1 <- birthreg$coefficients[2]
blag2 <- birthreg$coefficients[3]

bdf <- data.frame(year=integer(),bp1000=double())
bpred2022 <- bintercept + blag1*(birthparam[72]) + blag2*(birthparam[71])
bdf[nrow(bdf) + 1,] = c(2022,bpred2022)
bpred2023 <- bintercept + blag1*(bdf$bp1000[1]) + blag2*(birthparam[72])
bdf[nrow(bdf) + 1,] = c(2023,bpred2023)

for(x in 3:101){
  year <- x+2021
  prediction <- bintercept + blag1*(bdf$bp1000[x-1]) + blag2*(bdf$bp1000[x-2])
  bdf[nrow(bdf) + 1,] = c(year,prediction)
}
bdf
births$Births.per.1000.People

#######
#Plots#
#######
library(ggplot2)
combinedbdf <- data.frame(Year = c(1950:2122), BP1000 = c(births$Births.per.1000.People,bdf$bp1000))

ggplot(combinedbdf,aes(Year,BP1000)) + geom_line() + 
  geom_vline(xintercept=2022,color="red") +  
  geom_text(aes(x=2022,label="2022 Onward", y=20),hjust=-.5,vjust=-5, color="red") +
  ggtitle("Births Per 1000 People Given Year") +
  theme(plot.title = element_text(hjust = 0.5))

combinedddf <- data.frame(Year = c(1950:2122), DP1000 = c(deaths$Deaths.per.1000.People,ddf$dp1000))

ggplot(combinedddf,aes(Year,DP1000)) + geom_line() + 
  geom_vline(xintercept=2022,color="red") +  
  geom_text(aes(x=2022,label="2022 Onward", y=20),hjust=-.5,vjust=-5, color="red") +
  ggtitle("Deaths Per 1000 People Given Year") +
  theme(plot.title = element_text(hjust = 0.5))

#########
#ANSWERS#
#########
pop2021 <- population$Population[72]
popdf <- data.frame(year=integer(),population=integer()) 
pop2022 <- round((bdf$bp1000[1]-ddf$dp1000[1])*pop2021/1000) + pop2021
popdf[1,] = c(2022,pop2022)
for(x in 2:101){
  change <- round((bdf$bp1000[x]-ddf$dp1000[x])*popdf$population[x-1]/1000)
  totalpop <- change + popdf$population[x-1]
  popdf[nrow(popdf) + 1,] = c(popdf$year[x-1]+1,totalpop)
}
popdf

combinedpdf<- data.frame(Year = c(1950:2122),Population = c(population$Population,popdf$population))
ggplot(combinedpdf,aes(Year,Population)) + geom_line() + 
  geom_vline(xintercept=2022,color="red") +  
  geom_text(aes(x=2022,label="2022 Onward", y=20),hjust=-.5,vjust=-5, color="red") +
  ggtitle("Total Population Given Year") +
  theme(plot.title = element_text(hjust = 0.5))


# deckbrew - json verison of cards, current price data from TCGPlayer
# blacklotusproject - csv version of cards, historic price data
# mtgjson - json version of cards
# mtgapi - nothing? used something to get rankings
# mtggoldfish - uses supernova bot for prices b4 2013, mtgotraders for prices after 2013
# contains BOTH online AND paper prices (from MTGOtraders & TCGPlayer, respectively)
# https://github.com/kihashi/mtg-irc uses irc bot phenny to pull mtgo and physical card prices

install.packages("car")
library(httr)
library(Hmisc) # for correlation
library(ggplot2)
library(stringr)
library(car)
require(jsonlite)
library(plyr)

## Declare functions ##

clean <- function(dat){
  #   dat$editions[[3]]$price$low
  #   dat[89,]$editions[[1]]$price
  #   prices <- lapply(dat$editions, "[[", "price")   # lapply?
  #   prices[which(prices==NULL)] <- NA
  #   prices[sapply(prices, is.null)] <- NA  # if no entry exists
  #   pricesLow <- sapply(prices, "[[", "low")
  
  str(dat$editions[6:8])
  # duplicate entry for each value in editions list
  dat$editions[]
  
  dat$nText <- str_length(dat$text)
  dat$nName <- str_length(dat$name)  
  dat$colors2 <- ifelse(lapply(dat$colors, length) > 1, c("multicolor"), dat$colors)
  dat$nColors <- unlist(lapply(dat$colors, length))
  
  # need to eliminate '0' values in price field, e.g., all[all$name=="Black Lotus",]$editions
  dat$priceLow <- matrix(lapply(lapply(lapply(dat$editions,"[[", "price"), "[[", "low"), mean, na.rm = TRUE, warn=FALSE))  # NULL converts to NA, triggers warning
  dat$priceMedian <- matrix(lapply(lapply(lapply(dat$editions,"[[", "price"), "[[", "median"), mean, na.rm = TRUE))  # NULL converts to NA
  dat$priceHigh <- matrix(lapply(lapply(lapply(dat$editions,"[[", "price"), "[[", "high"), mean, na.rm = TRUE))  # NULL converts to NA
  
  # scrape null values
  dat$colors2 <- unlist(ifelse(sapply(dat$colors2, is.null), "none", dat$colors2))
  dat$nColors <- unlist(ifelse(sapply(dat$nColors, is.null), NA, dat$nColors))
  dat$priceLow <- unlist(ifelse(sapply(dat$priceLow, is.null), NA, dat$priceLow))
  dat$priceMedian <- unlist(ifelse(sapply(dat$priceMedian, is.null), NA, dat$priceMedian))
  dat$priceHigh <- unlist(ifelse(sapply(dat$priceHigh, is.null), NA, dat$priceHigh))
  
  unique(unlist(dat$types))
  dat$creature <- sapply(sapply(dat$types, "%in%", c("creature","summon")), "any")
  dat$artifact <- sapply(sapply(dat$types, "%in%", "artifact"), "any")
  dat$land <- sapply(sapply(dat$types, "%in%", "land"), "any")
  dat$enchant <- sapply(sapply(dat$types, "%in%", "enchantment"), "any")
  dat$sorcery <- sapply(sapply(dat$types, "%in%", "sorcery"), "any")
  dat$planeswalker <- sapply(sapply(dat$types, "%in%", "planeswalker"), "any")
  dat$tribal <- sapply(sapply(dat$types, "%in%", "tribal"), "any")
  dat$instant <- sapply(sapply(dat$types, "%in%", c("instant","interrupt")), "any")
  dat$type <- sapply(dat$types, paste, collapse=" ")
  
  return(dat)
}

is.not.null = function(x)!is.null(x)



load("test.RData")

# Grab data from URL "https://api.deckbrew.com/mtg/cards?page=[x]"
# deckbrew="http://api.deckbrew.com/mtg/cards"
# for (pg in c(0:105,107:170)){  #170   #106 gives error
#   url <- paste(deckbrew,pg,sep="?page=")
#   rd  <- fromJSON(readLines(url, warn="F"))
#   
#   rd$formats <- as.matrix(rd$formats)  #flatten formats df
#   if(pg==0){ all <- rd}
#   else {all <- rbind.fill(all,rd)}
# }

### card functionality ###
d <- clean(all)

# resulting data structure of 1 card (all editions) in the dataframe
str(d[2,])

attach(d)
# na.omit(cbind(colors2,cmc))

# summary statistics

# card type
op <- par(mar = c(5,9,4,2) + 0.1)
barplot(prop.table(table(d$type)), beside = T, legend = colnames(d$types), horiz = T, xlim = c(0, .5),
        las=1, main="card types")
axis(1, at = pretty(c(0, 1.1*max(table(d$type))))/length(d$type),
     label = pretty(c(0, 1.1*max(table(d$type)))), col='brown', col.ticks="brown", col.axis="brown",
     line=2.2)
mtext("prop",1,line=1,at=-0.02,col="black")
mtext("count",1,line=2.2,at=-0.02,col="brown")
par(op)

# card color
op <- par(mar = c(5,6,4,2) + 0.1)
barplot(prop.table(table(d$colors2)), beside = T, legend = colnames(d$colors), horiz = T,
        las=1, main="card colors")
axis(1, at = pretty(c(0, 1.1*max(table(d$colors2))))/length(d$colors2),
     label = pretty(c(0, 1.1*max(table(d$colors2)))), col='brown', col.ticks="brown", col.axis="brown",
     line=2.2)
mtext("prop",1,line=1,at=-0.008,col="black")
mtext("count",1,line=2.2,at=-0.008,col="brown")
par(op)

# price
scatterplotMatrix(~I(log10(priceLow+1)) + I(log10(priceMedian+1)) + I(log10(priceHigh+1)), data=d, main="Price variables")
library(car)

# 100 most expensive cards
d[order(-d$priceMedian,d$name),c("priceMedian","name")][1:100,]
d[11620,]$editions[[1]]$image_url

# images do not work as below; needs new site #sryfall image url format https://img.scryfall.com/cards/large/en/pcel/1.jpg
install.packages("jpeg")
library("jpeg")
img <- readJPEG(system.file("img", d[11620,]$editions[[1]]$image_url, package="jpeg"))

# price by color and type
# test
fit <- aov(I(log10(priceMedian+1)) ~ colors2*type, data=d)
summary(fit)

pairwise.t.test(log10(priceMedian+1), type, p.adj = "bonf")
lm1 <- lm(I(log10(priceMedian+1)) ~ factor(type) * factor(colors2))
summary(lm1)

layout(matrix(c(1,2,3,4),2,2))
plot(fit)
coef(fit)
confint(fit)
par(op)
#print(model.tables(fit,"means"),digits=3)
all$planeswalker <- sapply(sapply(all$types, "%in%", "planeswalker"), "any")
lm1 <-lm(priceMedian ~ planeswalker + creature + artifact + land + enchant + sorcery + instant + tribal, data=d)

contrasts(hsb2$race.f) <- contr.treatment(4, base = 2)
# dorsn't account for multiple types
# summary(lm(priceMedian ~ factor(type)))


summary(lm1)
coef(lm1)
confint(lm1)
table(lm1)


# price by type
op <- par(mar = c(5,6,4,2) + 0.1)
boxplot( I(log10(priceMedian)) ~ factor(type), las=1, xaxt="n",log="x", cex.axis=.55, xlab="cost($)", ylab="", main="card cost by type", horizontal=T)
axis(1, at = pretty(c(0, 1.1*max(log10(priceMedian), na.rm=T))),
     label = c(".01",".1","1","10","100","1000", "10000", "1000000"), col='brown', col.ticks="brown", col.axis="brown",
     line=1)
par(op)
detach(d)

# subset on common types
d1 <- subset(d, type=="artifact"|type=="artifact creature"|type=="creature"|type=="creature enchantment"|type=="enchantment"|type=="instant"|type=="land"|type=="planeswalker"|type=="sorcery",
             select=c(name,priceMedian,type))
attach(d1)
op <- par(mar = c(5,6,4,2) + 0.1)
boxplot( I(log10(priceMedian)) ~ factor(type), las=1, xaxt="n",log="x", cex.axis=.55, xlab="cost($)", ylab="", main="card cost by type", horizontal=T)
axis(1, at = pretty(c(0, 1.1*max(log10(priceMedian), na.rm=T))),
     label = c(".01",".1","1","10","100","1000", "10000", "1000000"), col='brown', col.ticks="brown", col.axis="brown",
     line=0)
par(op)
detach(d1)
attach(d)

# price by type (refined)
str(subset(d, type=="artifact", select=c(name,priceMedian,type)))

# price by color
op <- par(mar = c(5,6,4,2) + 0.1)
boxplot( I(log10(priceMedian)) ~ factor(colors2), las=1,log="x", xaxt="n", cex.axis=.55, xlab="cost($)", ylab="", main="card cost by color", horizontal=T)
axis(1, at = pretty(c(0, 1.1*max(log10(priceMedian), na.rm=T))),
     label = c(".01",".1","1","10","100","1000", "10000", "1000000"), col='brown', col.ticks="brown", col.axis="brown",
     line=0)
par(op)

# cmc by type
boxplot( cmc[-4947] ~ factor(type[-4947]), horizontal=T, las=1)

# Boxplot( I(log10(priceMedian+1)) ~ factor(type),las=1, cex.axis=.55, log="y", main="price by type", labels=d$name)
# scatterplotMatrix(~I(log10(priceMedian)) + nText + nName + cmc|colors2, data=d, main="Card use")
# text(y = seq(0, 100, by=20), par("usr")[1], labels = lablist.y, srt = 45, pos = 2, xpd = TRUE)

library(gplots)
plotmeans(I(log10(priceMedian+1)) ~ factor(type),xlab="type",
          ylab="log price", main="price by card type", cey.axis=.5,horizontal=T)

# price by color
# Boxplot( I(log10(priceMedian+1)) ~ factor(colors2), main="price by color",
#          horizontal=T, labels.omit=T)

# price by card type
summary(lm(priceMedian ~ scale(nName, center = TRUE, scale = FALSE) +
             scale(nText, center = TRUE, scale = FALSE) +
             scale(nColors, center = TRUE, scale = FALSE), data=d))

layout(matrix(c(1,2,3,4),2,2)) # optional layout 
plot(fit) # diagnostic plots

detach(d)

# testing
setwd("C:/Users/USER/SkyDrive/magic")
d0 <- fromJSON(readLines("https://api.deckbrew.com/mtg/cards?page=0", warn="F"))
d1 <- fromJSON(readLines("https://api.deckbrew.com/mtg/cards?page=1", warn="F"))
d0$formats <- as.matrix(d0$formats)
d1$formats <- as.matrix(d1$formats)
test <-rbind.fill(subset(d0,select= -formats), subset(d1,select= -formats))

### card rarity, set, year data ###
#http://www.r-bloggers.com/concatenating-a-list-of-data-frames/
  
### online card price component ###
  
# http://www.mtggoldfish.com/price-download/online/Elvish+Mystic+%5BM15%5D
# http://www.mtggoldfish.com/price-download/online/Mantis+Rider+%5BKTK%5D+%28F%29

var = 1
url = paste("http://stats.grok.se/json/en/",var,"/web_scraping",sep="")


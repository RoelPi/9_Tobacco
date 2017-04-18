rm(list=ls())
options(scipen=999)
require(data.table)
require(ggplot2)
require(forecast)
require(zoo)
require(scales)
require(RColorBrewer)

# Load data
# dtSigaretten <- data.table(read.csv2("dataSigaretten.csv",stringsAsFactors=F))
# dtRokers <- data.table(read.csv2("dataRokers.csv",stringsAsFactors=F))
# dtBevolking <- data.table(read.csv2("dataBevolking.csv",stringsAsFactors=F))
# dtAccijnzen <- data.table(read.csv2("dataAccijnzen.csv",stringsAsFactors=F))
# dtInkomsten <- data.table(read.csv2("dataInkomsten.csv",stringsAsFactors=F))
# dtIndex <- data.table(read.csv2("dataIndex.csv",stringsAsFactors=F))
# dtIndexF <- data.table(read.csv2("dataIndexFull.csv",stringsAsFactors=F))
dtIndexMaand <- data.table(read.csv2("dataIndexMaand.csv",stringsAsFactors=F))
dtAccijnzenMaand <- data.table(read.csv2("dataAccijnzenMaand.csv",stringsAsFactors=F))

# set price of pack of cigs today
prijs <- 6
maandNu <- '2017-03-01'
btw <- 0.21

# Fix columns
dtAccijnzenMaand$maand <- as.Date(dtAccijnzenMaand$maand,format="%d/%m/%Y")
dtIndexMaand$maand <- as.Date(dtIndexMaand$maand,format="%d/%m/%Y")

dt <- data.table(merge(dtAccijnzenMaand,dtIndexMaand,by='maand',all.x=T))
dt <- data.table(dt[,.(maand,adValoremSig,bijzonderAdValoremSig,specifiekeAccijnsSig,bijzonderSpecifiekeAccijnsSig,minimumSig,indexSigaretten)])


# Bereken minimum kleinhandelsprijs & basisprijs op basis van minimumlasten uit wetgeving
dt[,minPrijs:=(minimumSig-specifiekeAccijnsSig-bijzonderSpecifiekeAccijnsSig)/1000*19/(adValoremSig+bijzonderAdValoremSig)]


# Bereken gemiddelde kleinhandelsprijs van pak sigaretten op basis van index
indexNu <- dt[maand == maandNu]$indexSigaretten
dt[,avgPrijs:=indexSigaretten/indexNu*prijs]

# Basisprijs berekenen op basis van geëxtrapoleerde huidige prijs - lasten, dit is nodig om voorspelling te maken
dt[,baseAvgPrijs:=(1-adValoremSig-bijzonderAdValoremSig)*avgPrijs/1.21-(specifiekeAccijnsSig+bijzonderSpecifiekeAccijnsSig)/1000*19]

# Voorspelling over toekomstige prijs op basis van lineaire regressie
fclm <- with(dt[maand > '2012-01-01'],lm(baseAvgPrijs~maand))
toPredict <- dt[maand > '2017-03-01']
dt <- dt[maand < '2017-03-02']
toPredict$baseAvgPrijs <- predict(fclm,toPredict)
predictionIntervals <- predict(fclm,toPredict,interval="predict")
toPredict <- toPredict[,avgPrijs:=(baseAvgPrijs+(specifiekeAccijnsSig+bijzonderSpecifiekeAccijnsSig)/1000*19)*1.21/(1-adValoremSig-bijzonderAdValoremSig)]

# Samenvoegen waargenomen data en voorspelling
dt <- data.table(rbind(dt,toPredict))

# Bereken prijsonafhankelijke prijscomponenten
dt[,c('sa','bsa'):=list(specifiekeAccijnsSig/1000*19,bijzonderSpecifiekeAccijnsSig/1000*19)]

# Bereken prijscomponenten voor gemiddelde prijs
# Base prijs reeds berekend
dt[,c('avAvgPrijs','bavAvgPrijs','btwAvgPrijs'):=list(avgPrijs*adValoremSig,avgPrijs*bijzonderAdValoremSig,(baseAvgPrijs+sa+bsa)*0.21)]

# Bereken prijscomponenten voor minimumprijs
dt[,baseMinPrijs:=(1-adValoremSig-bijzonderAdValoremSig)*minPrijs/1.21-(specifiekeAccijnsSig+bijzonderSpecifiekeAccijnsSig)/1000*19]
dt[,c('avMinPrijs','bavMinPrijs','btwMinPrijs'):=list(minPrijs*adValoremSig,minPrijs*bijzonderAdValoremSig,(baseMinPrijs+sa+bsa)*0.21)]


timeSeriesPlot <- function(dt,start,vars) {
    dt <- melt(dt,id.vars=c('maand'),measure.vars=vars)
    dt <- dt[maand > start]
    g <- ggplot(dt,aes(x=maand,y=value,fill=variable)) + 
        geom_area(stat="identity") + 
        geom_vline(xintercept = as.numeric(as.Date(maandNu))) +
        ylab('Prijs') +
        theme(axis.title.x = element_blank(),legend.position='bottom',legend.title = element_blank()) +
        scale_fill_brewer(palette='RdGy',guide=guide_legend(direction='horizontal'),labels=c('Basisprijs','Ad Valorem','Bijzonder Ad Valorem','Specifieke Accijns','Bijzonder Specifieke Accijns','BTW')) +
        scale_y_continuous(labels=function(x) sprintf("€ %.2f", x))  
}

overviewPlot <- function(dt,vars) {
    dt <- melt(dt,id.vars=c('maand'),measure.vars=vars)
    dt <- dt[maand == as.Date(maandNu,"%Y-%m-%d")]
    
    g <- ggplot(dt,aes(x=maand,y=value,fill=variable,label=round(value,2))) + 
        geom_bar(stat="identity",width=0.2) + 
        geom_text(position='stack') +
        coord_flip() +
        ylab('Prijs') +
        theme(axis.title.y = element_blank(),axis.text.y = element_blank(),legend.position='bottom') +
        scale_fill_brewer(palette='RdGy',guide=guide_legend(direction='horizontal'),labels=c('Basisprijs','Ad Valorem','Bijzonder Ad Valorem','Specifieke Accijns','Bijzonder Specifieke Accijns','BTW')) +
        scale_y_continuous(labels=function(x) sprintf("€ %.2f", x)) 
}

# Generate plot
gMin <- timeSeriesPlot(dt,'2012-01-01',c('baseMinPrijs','avMinPrijs','bavMinPrijs','sa','bsa','btwMinPrijs'))
gAvg <- timeSeriesPlot(dt,'2006-01-01',c('baseAvgPrijs','avAvgPrijs','bavAvgPrijs','sa','bsa','btwAvgPrijs'))
hMin <- overviewPlot(dt,c('baseMinPrijs','avMinPrijs','bavMinPrijs','sa','bsa','btwMinPrijs'))
hAvg <- overviewPlot(dt,c('baseAvgPrijs','avAvgPrijs','bavAvgPrijs','sa','bsa','btwAvgPrijs'))
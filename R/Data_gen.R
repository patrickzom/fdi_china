library(WDI)
library(tidyr)
library(dplyr)
library(readxl)
library(rgdal)
library(spdep)
library(GISTools)
library(countrycode)
library(data.table)
library(mice)
library(splm)


##-----------------------------------------------functions


full_excel_read<-function(fpath,v=TRUE, skip=FALSE){
  
  sheetnames <- excel_sheets(fpath)
  workbook <- sapply(sheetnames,function (x){readxl::read_excel(fpath,sheet = x, skip = skip)})
}


##-----------------------------------------------FDI data+ Shapefile

data2<-readxl::read_xlsx("./data/FDI_China_Outflows_africa_2015_unctad.xlsx", sheet = 2)
data<-gather(data2,"country", "value",2:57)



shp <- readOGR(dsn = "./data/Eurostat_RG_2016_4326_M01", layer ="CNTR_RG_01M_2016_4326", encoding = "UTF-8")


data1<-data %>%spread(Year, value)
data1<-na.omit(data1)
africa<-data1$country

remove(data1)
#filter african countries
shp <- shp[shp$NAME_ENGL %in% africa, ]


##-----------------------------------------------Covariates--------------------------------------------------------
#------------------------------------------------read in political data



pol<-full_excel_read("./data/3BResearchersDataset2018.xls", v=FALSE, skip = 6)
pol<-pol[c(2:13)]

POL <- rbindlist(pol, idcol = TRUE)
POL$iso2c<-countrycode(POL$Country, "country.name", "iso2c")

x<-shp@data
x<-as.character(x$CNTR_ID)

POL <- POL[POL$iso2c %in% x, ]

POL<-POL[,-(3:21)]#keine NAs in pol



A<-gather(POL,"year", "pol",3:15)
A<-A%>%group_by(Country, iso2c, year)%>%summarise(pol=sum(pol))#calculate sum of all political risk variables

ind<-gather(POL,"year", "pol",3:15)
ind<-ind%>%group_by(Country, iso2c, year, .id)%>%summarise(pol=sum(pol))#calculate sum of all political risk variables

remove(POL)
remove(pol)

#start ordering
ind<-filter(ind, iso2c!="LY")#to get Libya out
A<-filter(A, iso2c!="LY")
ind<-filter(ind, iso2c!="ZA")#to get South Africa out
A<-filter(A, iso2c!="ZA")

x<-levels(as.factor(ind$iso2c))
shp <- shp[shp$CNTR_ID %in% x, ]

x<-shp@data
x<-as.character(x$CNTR_ID)

ind<-ind %>% spread(.id, pol)

ind<-ind[order(ind$year,match(ind$iso2c,x)),]#for ordering ind in the same way as our weights matrix
A<- A[order(A$year,match(A$iso2c,x)),]#for ordering A in the same way as our weights matrix

sum(1:442==which(ind$iso2c==x))

#------------------------------------------------read in financial risk data-----------------


fin<-full_excel_read("./data/4BDataset2018.xls", v=FALSE, skip = 6)
fin<-fin[c(2:6)]
FIN <- rbindlist(fin, idcol = TRUE)


FIN$iso2c<-countrycode(FIN$Country, "country.name", "iso2c")
FIN <- FIN[FIN$iso2c %in% x, ]

FIN<-FIN[,-(3:21)]#keine NAs in Fin

B<-gather(FIN,"year", "fin",3:15)
B<-B%>%group_by(Country, year)%>%summarise(fin=sum(fin))

fin<-gather(FIN,"year", "fin",3:15)
fin<-fin%>%group_by(Country, year, .id)%>%summarise(fin=sum(fin))

fin<-fin %>% spread(.id, fin)

ind<-left_join(ind,fin, by=c("Country", "year"))#keine NAs
indices <- left_join(A,B, by=c("Country", "year"))

remove(FIN)
remove(fin)

#------------------------------------------------read in economic risk data----------------


eco<-full_excel_read("./data/5BDataset2018.xls", v=FALSE, skip = 6)
eco<-eco[c(2:6)]
ECO <- rbindlist(eco, idcol = TRUE)

ECO$iso2c<-countrycode(ECO$Country, "country.name", "iso2c")
ECO <- ECO[ECO$iso2c %in% x, ]

ECO<-ECO[,-(3:21)]#keine NAS

C<-gather(ECO,"year", "eco",3:15)
C<-C%>%group_by(Country, year)%>%summarise(eco=sum(eco))

eco<-gather(ECO,"year", "eco",3:15)
eco<-eco%>%group_by(Country, year, .id)%>%summarise(eco=sum(eco))

eco<-eco %>% spread(.id, eco)

ind<-left_join(ind,eco, by=c("Country", "year"))#keine NAs
indices <- left_join(indices,C, by=c("Country", "year"))

anyNA(ind)
remove(ECO)
remove(eco)
remove(A,B,C)

###-------------------------------------------WDI DATA----------------------------------------------------
###------------------------------------------------------------------------------------------------------



#---------------------------------------GDP per Capita and population----------------------------------------
# get the variables gdp per capita and population 
data_WDI<-WDI(country = "all", indicator = c("SP.POP.TOTL","NY.GDP.MKTP.KD"), start=2003, end=2015)# search population postition 497 #search gdp per capita postition 9 
data_WDI <- data_WDI[data_WDI$iso2c %in% x, ]
anyNA(data_WDI)



#---------------------------------------Trade Costs----------------------------------------
#search the variables we want
trade<-WDI(country = "all", indicator = c("NE.TRD.GNFS.ZS"), start=2003, end=2015)#exports: "NE.EXP.GNFS.ZS"# imports: NE.IMP.GNFS.ZS# trade->exports+imports "NE.TRD.GNFS.ZS"

trade <- trade[trade$iso2c %in% x, ]

sum(is.na(trade))


eth<-WDI(country = "all", indicator = c("NY.GDP.MKTP.CD","BX.GSR.TOTL.CD","BM.GSR.TOTL.CD"), start=2003, end=2015)
#NY.GDP.MKTP.CD#gdp
#BX.GSR.TOTL.CD#exports primary income income
#BM.GSR.TOTL.CD#imports primary income

eth <- eth[eth$country %in% "Ethiopia", ]# für die fehlenden Werte von Ethiopien
eth<-eth[order(-eth$year),]#um auch decreasend wie in trade
z<-(eth$BX.GSR.TOTL.CD+eth$BM.GSR.TOTL.CD)/eth$NY.GDP.MKTP.CD*100
z

# 
# k<-filter(trade, country=="Ethiopia")$NE.TRD.GNFS.ZS
# k


trade[118:130,3]<-z

data_WDI<-left_join(data_WDI,trade, by=c("iso2c","country", "year"))
names(data_WDI)[names(data_WDI) %in% c("SP.POP.TOTL","NY.GDP.MKTP.KD","NE.TRD.GNFS.ZS")] = c("population","gdp", "tradec")

data_WDI$tradec<-1/data_WDI$tradec*100#too calculate the trade costs

#order Wdi data as our shapefile
data_WDI<-data_WDI[order(data_WDI$year,match(data_WDI$iso2c,x)),]

sum(1:442==which(data_WDI$iso2c==x))# gleiche reihenfolge wie shapefile

remove(trade)
remove(eth)
remove(z)
#-------------------------------------------#Total natural resources rents (% of GDP-----------------------------------

reso<-WDI(country = "all", indicator = c("NY.GDP.TOTL.RT.ZS"), start=2003, end=2015)#Total natural resources rents (% of GDP

reso <- reso[reso$iso2c %in% x, ]


sum(is.na(reso))#in libya fehlen die letzten vier Jahre, wegen Bürgerkrieg


#auch werte für 2012, 2013 auf https://tradingeconomics.com/libya/total-natural-resources-rents-percent-of-gdp-wb-data.html
#2013#42
#2012#59
# 
# reso$NY.GDP.TOTL.RT.ZS
# libya<-reso[209:221,]
# libya<-libya[order(libya$year),]
# libya[10,3]<-59
# libya[11,3]<-42
# 
# libya1<-predict(arima(libya$NY.GDP.TOTL.RT.ZS,order=c(1,1,0)), n.ahead=2 )$`pred`#einfach mit AR
# libya[12:13,3]<-libya1
# libya<-libya[order(-libya$year),]
# 
# reso[209:221,"NY.GDP.TOTL.RT.ZS"]<-libya$NY.GDP.TOTL.RT.ZS
# 
data_WDI<-left_join(data_WDI,reso, by=c("iso2c","country", "year"))

names(data_WDI)[names(data_WDI) %in% c("NY.GDP.TOTL.RT.ZS")] = c("reso")
# 
# 
# remove(libya1)
# remove(libya)
remove(reso)



#-------------------------------------------distance china host country-----------------------------------

china <- readOGR(dsn = "./data/Eurostat_RG_2016_4326_M01", layer ="CNTR_RG_01M_2016_4326", encoding = "UTF-8")
ch<-c(x,"CN")
china<- china[china$CNTR_ID %in% ch, ]

coordsch <-coordinates(china)
distw.tot <- dnearneigh(coordsch,0,Inf, row.names = china$CNTR_ID)
dnbdist.tot <- nbdists(distw.tot, coordsch) 


dist<-dnbdist.tot[[which(china@data$CNTR_ID=="CN")]]#eight for china
dist<-data.frame(x,dist)


data_WDI<-left_join(data_WDI,dist, by=c("iso2c"="x"))



remove(china)
remove(ch)
remove(distw.tot)
remove(dnbdist.tot)
remove(coordsch)
remove(dist)

#-------------------------------------------Sourounding market potential
# in the paper they use the same W for the calculation of it as they use for the spatial dependence 
# I think they also use the total gdp and not the gdp per head. 

#Create W matrix
coords <-coordinates(shp)
distw.tot <- dnearneigh(coords,0,Inf, row.names = shp$CNTR_ID)
dnbdist.tot <- nbdists(distw.tot, coords)
gl.tot <- lapply(dnbdist.tot, function(x) 1/x^2) #calculating inverse distances and take x^2 because we want to put more weight on closer neighbours
W.list.inv <- nb2listw(distw.tot, glist=gl.tot, zero.policy=FALSE, style = "W")
W.dis<-listw2mat(W.list.inv)


#use Gdp 
gdp<-WDI(country = "all", indicator = c("NY.GDP.MKTP.KD"), start=2003, end=2015)#GDP (constant 2010 US$) 
gdp <- gdp[gdp$iso2c %in% x, ]
gdp1<-gdp%>%spread(year, NY.GDP.MKTP.KD)

#magic bring gdp1 into same order as W.dis
gdp1<-gdp1[order(match(gdp1[,1],rownames(W.dis))),]

#check if gdp1 and W.dis have same order
sum(1:34==which(rownames(W.dis)==gdp1$iso2c))




#-----------calculate potential gdp
gdp1[,3:15]<-W.dis%*%as.matrix(gdp1[,3:15])

gdp1<-gather(gdp1,"year", "GDP_po", 3:15)#sollt so passen hoff ich
gdp1$year<-as.numeric(gdp1$year)

data_WDI<-left_join(data_WDI,gdp1, by=c("iso2c","country", "year"))



remove(gdp1)
remove(gdp)
#---------------------------------------------BITs-----------------------------------------
chinatable<- read.csv("./data/china_treaties.csv")

chinatable$x<- countrycode(chinatable$CountryB, "country.name", "iso2c")
chinatable<-chinatable[chinatable$x %in% x, ]


dat<-transform(chinatable, from = Year, to=2015)
dat<-data.table(dat)

dat<-dat[, list(x, year = seq(from, to, by = 1)), by = 1:nrow(dat)]

dat$nrow<-1

names(dat)[names(dat) %in% "nrow"]= "bits"





data_WDI<-left_join(data_WDI, dat, by=c("iso2c"="x","year"))

data_WDI$bits[is.na(data_WDI$bits)]<-0


remove(dat)
remove(chinatable)

# ------------------------------------------------------Zusammenführung
ind<-ind[,-1]
ind$year<-as.numeric(ind$year)


data_exp<-left_join(data_WDI, ind, by=c("year", "iso2c"))

#change order
#ind<-ind[order(ind$year,match(ind$iso2c,x)),]


#--------------------------------------------literacy ----------------------------------------------

#cross-country-literacy-rates https://ourworldindata.org/literacy

litt<-read.csv("./data/cross-country-literacy-rates.csv")

litt$Code<-countrycode(litt$Code,"iso3c","iso2c")
litt <- litt[litt$Code%in% x, ]

unique(litt$Entity)#für alle Länder eine observation
#wie interpolieren wir am besten.

litt<-litt[,2:ncol(litt)]
anyNA(data_exp)
data_exp<-left_join(data_exp, litt, by=c("iso2c"="Code", "year"="Year"))

names(data_exp)[names(data_exp) %in% c("Literacy.rates..World.Bank..CIA.World.Factbook..and.other.sources.....")] = c("lit")

data_exp<-subset(data_exp,select=-c(country))
litt<-subset(data_exp,select=c(iso2c, year,lit))

library(plm)
litt<-pdata.frame(litt, c("iso2c", "year"))

zaza<-litt %>% group_by(iso2c)%>% mice(m=10,method = "pmm")
litt<-mice::complete(zaza)#um unseren finalen datensatz zu bekommen
litt<-as.data.frame(litt)

data_exp<-subset(data_exp,select=-c(lit))

litt$year<-as.numeric(rep.int(2003:2015,33))# auch ändern wenn Lybien raus

data_exp<-left_join(data_exp, litt, by=c("iso2c", "year"))

#geht wsl nur mit matrix


#Literacy http://sdg4monitoring.uis.unesco.org/data_tcg.php
#education_table-youth-and-adult-literacy-rate-updated-oct.-2015 https://data.unicef.org/topic/education/literacy/
# 
# lit<-readxl::read_xlsx("./data/Literacy.xlsx")
# bla<-ress$country
# lit <- lit[lit$`Country or territory` %in% bla, ]
# 
# 
# j<-levels(as.factor(lit$`Country or territory`))
# 
# x<-levels(as.factor(bla))
# 
# setdiff(x,j)
#-601865091 https://tellmaps.com/uis/literacy/#!/tellmap/-601865091


#--------------------------------------------------join FDI DAta-------------------------------------------------------

data$country<-countrycode(data$country, "country.name", "iso2c")

data<-data[data$country %in% x, ]
data<-na.omit(data)

data_exp<-left_join(data_exp, data, by=c("iso2c"="country", "year"="Year"))


anyNA(data_exp)


names(data_exp)[names(data_exp) %in% c("A-Government Stability","B-Socioeconomic Conditions","C-Investment Profile","D-Internal Conflict","E-External Conflict","F-Corruption","G-Military in Politics","H-Religious Tensions","I-Law and Order","J-Ethnic Tensions","K-Democratic Accountability","L-Bureaucracy Quality")] = c("Government Stability","Socioeconomic Conditions","Investment Profile","Internal Conflict","External Conflict","Corruption","Military in Politics","Religious Tensions","Law and Order","Ethnic Tensions","Democratic Accountability","Bureaucracy Quality")


indices <- subset(indices, select=-Country)
indices$year <- as.numeric(indices$year)

data_full <- data_exp %>% left_join(indices, by=c("iso2c","year")) %>% rename.vars(from="GDP_po", to="gdp_po") %>% mutate(invcost=1/(pol+fin+eco))

remove(ind,indices, litt, zaza, data2)

#------------------------------------------------------Save Data-------------------------------------------------------------

data_exp<-data_exp[order(data_exp$year,match(data_exp$iso2c,rownames(W.dis))),]
data_exp<-data_exp[order(match(data_exp$iso2c,x), data_exp$year),]

data_full<-data_full[order(data_full$year,match(data_full$iso2c,rownames(W.dis))),]
data_full<-data_full[order(match(data_full$iso2c,x), data_full$year),]

#saveRDS(data_full, "./data/data_full.RData")
#saveRDS(data_exp, "FDI_data_large.rds")
#saveRDS(W.list.inv, "W.list.inv.rds")
#saveRDS(W.dis, "W.dis.rds")





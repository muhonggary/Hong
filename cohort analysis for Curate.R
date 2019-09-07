library(readxl)
library(MASS)
library(partykit)
library(grid)
library(libcoin)
library(mvtnorm)
#library(randomForest)
#library(sandwich)
#library(party)
library(caret)
#library(tidyverse)
library(rms)

#####################################
# Notes for future developers:
# 
# First I want to say sorry for this messy code,
# but due to limited time and my ability this is
# the best I can do. This head of this code is
# definition of variables and the creation
# of some new variables. For regressions
# and table generations, please run the code
# paragraph by paragraph instead of running the
# whole piece altogether.
# 
# I should make an important note on the
# conceptualization of the life cycles.
# I interpret each level of the life cycle 
# as a subset of its father level (for example,
# customer is a subset of opportunity), because
# it appears to me that all children levels are
# at the same time parts of their father levels (
# for example, all customers are former opportunities).
# 
# You may want to pay attention to the "mtos" vectors
# because I modified directly on variables they depend.
# I was lazy to create separate variables and I truly
# confess for that. So a good idea is to run the
# "mqlyes" vectors everytime before you proceed with
# "mtos" vectors, which stand for whether the conversion
# is TRUE or FALSE.
# 
# Finally, this is the work of Youming Liu as an
# undergraduate student at Washington University,
# if you have any difficulty to undertand my grosteque 
# coding, you may reach me via youmingliu@wustl.edu or
# youmingliu@aestas.cn as the last resort. I will help
# you out. This is the karma for my laziness today.
#####################################

data<-read_excel("C:\\Users\\13361\\Downloads\\test9.xls")
tr<-unlist(data["total revenue"])
nsales<-unlist(data["number of sales activities"])
dcust<-unlist(data['date became customer'])
persona<-unlist(data['persona'])
orisource<-unlist(data['original source'])
sc<-as.numeric(unlist(data['sales cycle']))
avgp<-as.numeric(unlist(data['average website pages viewed']))
country<-unlist(data['country'])
state<-unlist(data['state'])
closescore<-as.numeric(unlist(data['likelihood to close']))
life<-unlist(data['life cycle stage'])
avge<-unlist(data['average events per year'])
avgrpe<-unlist(data['average event revenue'])
chan<-unlist(data['channel name'])
cam<-unlist(data['campaign name'])
tr<-as.numeric(gsub("[$,]", "", tr))
afford<-unlist(data["afford"])
want<-unlist(data["want"])
need<-unlist(data["need"])
contact<-unlist(data['contact'])
nsales<-as.numeric(nsales)
rpersale<-tr/nsales
thank<-unlist(data['source to thank'])
x<-(length(persona):1)
#email related stuffs
mecli<-as.Date(unlist(data['last sales email clicked']),format="%m/%d/%Y")
days<-as.numeric(as.Date("04/04/2019",format="%m/%d/%Y")-mecli)
dayo<-as.Date(unlist(data['date became opportunity']),format="%m/%d/%Y")
dayc<-as.Date(unlist(data['date became customer']),format="%m/%d/%Y")
dayotonow<-as.numeric(as.Date('04/08/2019',format="%m/%d/%Y")-dayo)
dayftonow<-as.numeric(as.Date('04/08/2019',format="%m/%d/%Y")-as.Date(unlist(data['date of conversion']),format="%m/%d/%Y"))
dayftoo<-as.numeric(dayo-as.Date(unlist(data['date of conversion']),format="%m/%d/%Y"))
dayotoc<-as.numeric(dayc-dayo)
ed<-as.numeric(unlist(data["sales email delivered"]))
ec<-as.numeric(unlist(data["sales email clicked"]))
eo<-as.numeric(unlist(data["sales email opened"]))
otod<-eo/ed
ctod<-ec/ed
ctoo<-ec/eo

pure=c("0 - 25 Events Per Year",'50 - 100 Events Per Year','25 - 50 Events Per Year','100+ Events Per Year')
avge[!avge %in% pure]<-NA

dt<-as.Date(unlist(date),format="%m/%d/%Y")
ytf<-format(dt,"%Y") %in% c("2018","2019")
mtf<-format(dt,"%m") %in% c("10","11","12","1","2","3","4")
tf<-ytf & mtf
arr<-unlist(arr[tf,])
ql<-unlist(ql[tf,])
consults<-unlist(consults[tf,])
consultc<-unlist(consultc[tf,])
products<-unlist(products[tf,])
lta<-unlist(lta[tf,])
persona<-unlist(persona[tf,])
ltac<-unlist(ltac[tf,])


#outlier treatment
outk <- function(df,number, replace = NA, digits = 2) {
  b<-boxplot(number)
  lowerwhisker<-b$stats[1]
  upperwhisker<-b$stats[5]
  df<-df[df$number>lowerwhisker & df$number<upperwhisker,]

  return(df) #if values == TRUE, return z score for each value

  
}


rolling<-data.frame(subyes,leadyes,mqlyes,sqlyes,oppyes,customeryes)
lcnum<-colSums((rolling==TRUE))
converlist<-c(lcnum[2]/lcnum[1],lcnum[3]/lcnum[2],lcnum[4]/lcnum[3],lcnum[5]/lcnum[4],lcnum[6]/lcnum[5])
ConversionRate<-round(unname(converlist),4)
#names(converlist)<-c('Subscriber to Lead','Lead to MQL','MQL to SQL','SQL to Opportunity','Opportunity to Customer')
Lifecycle<-c('Subscriber to Lead','Lead to MQL','MQL to SQL','SQL to Opportunity','Opportunity to Customer')
#ConversionRate<-factor(ConversionRate,levels=ConversionRate)
Lifecycle<-factor(Lifecycle,levels=Lifecycle)
dfbar<-data.frame(ConversionRate,Lifecycle)
ggplot(data=dfbar, aes(x=Lifecycle, y=ConversionRate)) + 
  geom_bar(position = 'dodge', stat='identity',fill='light blue') +
  geom_text(aes(label=ConversionRate), position=position_dodge(width=0.9), vjust=-0.25)+theme(text = element_text(size=20))


#email related logistic
edf<-data.frame(otod,mtosyes)
edf<-na.exclude(edf)
emodel<-glm(lm(mtosyes~otod,data=edf,family=binomial(link='logit')))
summary(emodel)
#mean total revenue by persona
trialdf<-data.frame(tr,persona)
trialdf<-na.exclude(trialdf)
avgtr=aggregate(trialdf$tr,by=list(trialdf$persona),FUN=mean)


#a list of us states
us<-data.frame(country,state)
us<-us[us$country=='united states',]
us<-na.exclude(us)
usstates<-factor(us$state)

#a list of us states which have customers
bstate<-data.frame(state,customeryes,country)
bstate<-bstate[(bstate$country=='united states'& bstate$customeryes==TRUE),]
bstate<-na.exclude(bstate)
buystate<-factor(bstate$state,levels=levels(usstates))

#conversion rate table
crtable<-sort(round(table(buystate)/table(usstates),3),decreasing=TRUE)
summary(lm(unlist(crtable)~factor(unique(usstates))))
barplot(crtable[crtable>0],las=1,horiz=TRUE,xpd=FALSE,col=heat.colors(47),cex.names=1.3)
#double check the barplot
dfs<-data.frame(country,state)
dfs<-na.exclude(dfs[dfs$country=="united states",])
dfs<-droplevels(dfs)
table(dfs$state)

table(buystate)

#priority table accordign to conversion rate and current proportion
ptable<-sort(crtable/prop.table(table(usstates)),decreasing=TRUE)

#conversiontable according to orisource


rnori<-data.frame(orisource,customeryes)
rnori<-rnori[rnori$customeryes==TRUE,]
rnori<-na.exclude(rnori)
buysource<-factor(rnori$orisource,levels=levels(as.factor(orisource)))
rnoritable<-sort(round(table(buysource)/table(orisource),3),decreasing=TRUE)
rnoritable

#conversiontable by persona
rnp<-data.frame(persona,customeryes)
rnp<-rnp[rnp$customeryes==TRUE,]
rnp<-na.exclude(rnp)
buyp<-factor(rnp$persona,levels=levels(as.factor(persona)))
unorderp<-round(table(buyp)/table(persona),3)
rnptable<-sort(round(table(buyp)/table(persona),3),decreasing=TRUE)
rnptable

#par(mar=c(5.1, 13 ,4.1 ,2.1))
barplot(rnoritable[rnoritable>0],las=1,horiz=TRUE,xpd=FALSE,col=heat.colors(9),cex.names=1.3)
#same, but according to channel
rchan<-data.frame(chan,customeryes)
rchan<-rchan[rchan$customeryes==TRUE,]
rchan<-na.exclude(rchan)
buysource2<-factor(rchan$chan,levels=levels(as.factor(chan)))
rchantable<-sort(round(table(buysource2)/table(chan),3),decreasing=TRUE)
rchantable

#priority table accordign to orisource
# sort(rnoritable/prop.table(table(orisource)),decreasing=TRUE)

#frequency click to delivered
dfcli<-data.frame(ctod,orisource)
dfcli<-na.exclude(dfcli)
clilist<-aggregate(dfcli$ctod, by=list(dfcli$orisource), FUN=mean)
unorder3<-clilist
clilist<-clilist[order(clilist$x,decreasing = TRUE),]
barplot(clilist$x,names=clilist$Group.1,horiz=TRUE,xpd=FALSE,col=heat.colors(9),cex.names=1.3,las=1)
#same, by channel
dfcli2<-data.frame(ctod,chan)
dfcli2<-na.exclude(dfcli2)
clilist2<-aggregate(dfcli2$ctod, by=list(dfcli2$chan), FUN=mean)
clilist2<-clilist2[order(clilist2$x,decreasing = TRUE),]
barplot(clilist2$x,names=clilist2$Group.1,horiz=TRUE,xpd=FALSE,col=heat.colors(22),cex.names=1.3,las=1)
clilist2
#otod
dfcli3<-data.frame(otod,orisource)
dfcli3<-na.exclude(dfcli3)
clilist3<-aggregate(dfcli3$otod, by=list(dfcli3$orisource), FUN=mean)
clilist3<-clilist3[order(clilist3$x,decreasing = TRUE),]
barplot(clilist3$x,names=clilist3$Group.1,horiz=TRUE,xpd=FALSE,col=heat.colors(9),cex.names=1.3,las=1)

#ctoo
dfcli4<-data.frame(ctoo,orisource)
dfcli4<-na.exclude(dfcli4)
clilist4<-aggregate(dfcli4$ctoo, by=list(dfcli4$orisource), FUN=mean)
clilist4<-clilist4[order(clilist4$x,decreasing = TRUE),]
barplot(clilist4$x,names=clilist4$Group.1,horiz=TRUE,xpd=FALSE,col=heat.colors(9),cex.names=1.3,las=1)

#email and persona
dfcli5<-data.frame(otod,persona)
dfcli5<-na.exclude(dfcli5)
clilist5<-aggregate(dfcli5$otod, by=list(dfcli5$persona), FUN=mean)
clilist5<-clilist5[order(clilist5$x,decreasing = TRUE),]
barplot(clilist5$x,names=clilist5$Group.1,xpd=FALSE,col=heat.colors(3),cex.names=1.3)

dfcli6<-data.frame(ctod,persona)
dfcli6<-na.exclude(dfcli6)
clilist6<-aggregate(dfcli6$ctod, by=list(dfcli6$persona), FUN=mean)
unorderp2<-clilist6
clilist6<-clilist6[order(clilist6$x,decreasing = TRUE),]
barplot(clilist6$x,names=clilist6$Group.1,xpd=FALSE,col=heat.colors(3),cex.names=1.3)

dfcli7<-data.frame(ctoo,persona)
dfcli7<-na.exclude(dfcli7)
clilist7<-aggregate(dfcli7$ctoo, by=list(dfcli7$persona), FUN=mean)
clilist7<-clilist7[order(clilist7$x,decreasing = TRUE),]
barplot(clilist7$x,names=clilist7$Group.1,xpd=FALSE,col=heat.colors(3),cex.names=1.3)

buyyes<-data.frame(orisource,customeryes)
buyyes<-na.exclude(buyyes)
buyyes<-buyyes[buyyes$customeryes=="TRUE",]
buy<-factor(buyyes$orisource,levels=levels(as.factor(orisource)))
buytable<-sort(round(table(buy)/table(orisource),3),decreasing=TRUE)
buytable
#
# df<-data.frame(orisource,rpersale)
# df<-na.exclude(df)
# 
# df<-data.frame(country,state,rpersale)
# df<-df[df$country=="united states",]
# df<-na.exclude(df)
#round(table(df$state)/table(usbuy),3)

#close score to state
likeli<-data.frame(closescore,state,country)
likeli<-likeli[likeli$country=='united states',]
likeli<-na.exclude(likeli)
likeli2<-data.frame(closescore,orisource)
likeli2<-na.exclude(likeli2)

#texas,arizona,kansas,kentucky,louisana,minnesota,north carolina,south carolina
summary(lm(closescore~state,data=likeli))
#close score by state
clist2<-aggregate(likeli2$closescore, by=list(likeli2$orisource), FUN=mean)
clist2<-clist2[order(clist2$x,decreasing = TRUE),]
clist<-aggregate(likeli$closescore, by=list(likeli$state), FUN=mean)
clist<-clist[order(clist$x,decreasing = TRUE),]

#page seen and orisource
ps<-data.frame(avgp,orisource)
ps<-na.exclude(ps)
pslist<-aggregate(ps$avgp,by=list(ps$orisource),FUN=mean)
unorder<-pslist
pslist<-pslist[order(pslist$x,decreasing=TRUE),]
pslist

#page by persona
ps2<-data.frame(avgp,persona)
ps2<-na.exclude(ps2)
pslist2<-aggregate(ps2$avgp,by=list(ps2$persona),FUN=mean)
unorder2<-pslist2
pslist2<-pslist2[order(pslist2$x,decreasing=TRUE),]
pslist2

#synopsis
ctable<-round(table(buysource)/table(orisource),3)
conversionrate<-as.numeric(unname(ctable))
originalsource<-names(ctable)
#conversionrate<-c(0.134,0.085,0.059,0.056,0.041,0.036,0.035,0.034,0.032)
#Originalsource<-c("Direct traffic","Paid search","Organic search","Offline Sources","Other campaigns","Referrals","Paid social","Email marketing","Social media")
dfs<-data.frame(originalsource,conversionrate,unorder$x,unorder3$x)
names(dfs)[3]<-paste("average pages seen")
names(dfs)[4]<-paste("clicked-to-delivered rate")
dfs2<-dfs[,-1]
levels(dfs2)<-dfs$originalsource
barplot(t(as.matrix(dfs2)),beside = TRUE,names.arg = dfs$originalsource,col=rainbow(3))
legend('topright',legend=c("Conversion Rate", "Average Pages Seen","Clicked-to-delivered rate"),fill=rainbow(3),  cex=0.8)

dfrank<-data.frame(rank(dfs2$conversionrate),rank(dfs2$`average pages seen`),rank(dfs2$`clicked-to-delivered rate`))
par(mar=c(3, 15, 3, 1))
barplot(t(as.matrix(dfrank)),beside = TRUE,names.arg = dfs$originalsource,col=heat.colors(3),main = "Rank Plot",cex.names=1.2,horiz = TRUE,las=1)
legend('topright',legend=c("Conversion Rate", "Average Pages Seen","Clicked-to-delivered rate"),fill=heat.colors(3),  cex=0.8)


#synopsis, by persona
pnames<-names(unorderp)
dfsp<-data.frame(pnames,unorderp,unorder2$x,unorderp2$x)
names(dfsp)[3]<-paste("average pages seen")
names(dfsp)[4]<-paste("clicked-to-delivered rate")
dfsp2<-dfsp[,-1]
levels(dfsp2)<-dfsp2$pnames
dfprank<-data.frame(rank(dfsp2$unorderp),rank(dfsp2$`average pages seen`),rank(dfsp2$`clicked-to-delivered rate`))

barplot(t(as.matrix(dfprank)),beside = TRUE,names.arg = pnames,col=heat.colors(3),main = "Rank Plot",cex.names=1.2,horiz = TRUE,las=1)
legend('topright',legend=c("Conversion Rate", "Average Pages Seen","Clicked-to-delivered rate"),fill=heat.colors(3),  cex=0.8)

#rpersale to channel
rtochan<-data.frame(chan,rpersale)
b<-boxplot(rpersale)
lowerwhisker<-b$stats[1]
upperwhisker<-b$stats[5]
rtochan<-rtochan[rtochan$rpersale>lowerwhisker & rtochan$rpersale<upperwhisker,]

rtochan<-na.exclude(rtochan)
summary(lm(rpersale~factor(chan),data=rtochan))


#tr to channel
trtochan<-data.frame(chan,tr)
b<-boxplot(tr)
lowerwhisker<-b$stats[1]
upperwhisker<-b$stats[5]
trtochan<-trtochan[trtochan$tr>lowerwhisker & trtochan$tr<upperwhisker,]

trtochan<-na.exclude(trtochan)
summary(lm(tr~factor(chan),data=trtochan))

trtochan<-data.frame(chan,customeryes)
trtochan<-na.exclude(trtochan)
chanmodel<-glm(customeryes~chan,data=trtochan,family=binomial(link='logit'))
summary(chanmodel)
barplot(c(exp(coef(chanmodel))-1)[-1])

#tr to source
trtosource<-data.frame(orisource,tr)
trtosource<-na.exclude(trtosource)
summary(lm(tr~factor(orisource),data=trtosource))

#rpersale to source
rtosource<-data.frame(orisource,rpersale)
rtosource<-na.exclude(rtosource)
summary(lm(rpersale~factor(orisource),data=rtosource))

#rpersale to state
rtostate<-data.frame(country,state,rpersale)
rtostate<-rtostate[rtostate$state %in% usstates,]
b<-boxplot(rpersale)
lowerwhisker<-b$stats[1]
upperwhisker<-b$stats[5]
rtostate<-rtostate[rtostate$rpersale>lowerwhisker & rtostate$rpersale<upperwhisker,]
rtostate<-na.exclude(rtostate)
summary(lm(rpersale~factor(state),data=rtostate))

trtostate<-data.frame(country,state,tr)
trtostate<-trtostate[trtostate$state %in% usstates,]
trtostate<-na.exclude(trtostate)
summary(lm(tr~factor(state),data=trtostate))

#rpersale to persona
rtop<-data.frame(rpersale,persona)
rtop<-na.exclude(rtop)
summary(lm(rpersale~factor(persona),data=rtop))

#rpersale to sales cycle
rtosc<-data.frame(rpersale,sc)
rtosc<-na.exclude(rtosc)
summary(lm(rpersale~sc,data=rtosc))

#tr to sales cycle
trtosc<-data.frame(tr,sc)
trtosc<-na.exclude(trtosc)
summary(lm(tr~sc,data=trtosc))

#tr to sc+persona
trtoscp<-data.frame(tr,persona,sc)
trtoscp<-na.exclude(trtoscp)
summary(lm(tr~sc+factor(persona)+sc:factor(persona),data=trtoscp))

#rpersale to sc+persona

rtoscp<-data.frame(rpersale,persona,sc)
rtoscp<-na.exclude(rtoscp)
summary(lm(rpersale~sc+factor(persona)+sc:factor(persona),data=rtoscp))






# arr<-unlist(arr)
# ql<-unlist(ql)
# consults<-unlist(consults)
# consultc<-unlist(consultc)
# products<-unlist(products
# lta<-unlist(lta)
# persona<-unlist(persona)
# ltac<-unlist(ltac)

#father regression


#logistic regression
paid<-tr!=FALSE
paid[is.na(paid)]<-FALSE
df3<-na.exclude(data.frame(paid,avgp,orisource,ctod))
#df3<-df3[df3$state %in% buystate,]
logimodel<-glm(paid~.,data=df3,family=binomial(link="logit"))
anova(glm(paid~.,data=df3,family=binomial(link="logit")),test="Chisq")
summary(glm(paid~.,data=df3,family=binomial(link="logit")))
exp(cbind(coef(logimodel),confint(logimodel)))

df4<-na.exclude(data.frame(paid,nsales))
summary(glm(paid~nsales,data=df4,family = binomial))

paid<-tr!=FALSE
paid[is.na(paid)]<-FALSE
df3<-na.exclude(data.frame(paid,avgp,orisource,ctod))
#df3<-df3[df3$state %in% buystate,]

#customer? logistic regression
custdf<-na.exclude(data.frame(customeryno,orisource,avgp,ctod))
#custdf<-custdf[custdf$state %in% buystate,]

custmodel<-glm(customerno~.,data=custdf,family=binomial(link='logit'))
anova(custmodel,test="Chisq")
summary(custmodel)
exp(cbind(OR=coef(custmodel),confint(custmodel)))

##barplot of how many times a contact is more likely not to be converted to a customer, compared to Direct Traffic
barplot(c(exp(coef(custmodel))-1)[-1],names=c('Email marketing','Offline sources','Organic search','Other campaigns','Paid search','Paid social','Referrals','Social media'))[1]####

dfx<-data.frame(paid,avgp,avge,avgrpe)
dfx<-na.exclude(dfx)
modelx<-glm(paid~.,data=dfx,family = binomial(link="logit"))
anova(modelx,test="Chisq")

#partitioning data
intrain<- createDataPartition(churn$Churn,p=0.7,list=FALSE)
set.seed(2017)
training<- churn[intrain,]
testing<- churn[-intrain,]


#multilinear regression

dfml<-data.frame(ctod,otod,persona,orisource,rpersale,avgp,country)
b<-boxplot(rpersale)
lowerwhisker<-b$stats[1]
upperwhisker<-b$stats[5]
dfml<-dfml[dfml$rpersale>lowerwhisker & dfml$rpersale<upperwhisker,]
dfml<-dfml[dfml$country=='united states',]
dfml<-na.exclude(dfml)
modelml<-lm(rpersale~.,data=dfml)
stepAIC(modelml)
modelml0<-lm(formula = rpersale ~ ctod+otod+persona+orisource+avgp, data = dfml)
summary(modelml0)

#on tr
dfml1<-data.frame(ctod,ctoo,persona,orisource,tr,avgp)
b<-boxplot(tr)
lowerwhisker<-b$stats[1]
upperwhisker<-b$stats[5]
dfml1<-dfml1[dfml1$tr>lowerwhisker & dfml1$tr<upperwhisker,]
dfml1<-na.exclude(dfml1)
modelml1<-lm(tr~.,data=dfml1)
stepAIC(modelml1)
modelml01<-lm(formula = tr ~ ., data = dfml1)
summary(modelml01)

#logistic regression
master<-data.frame(customeryes,orisource,persona,avgp,avgrpe)
#restrict to us customers
master<-master[master$country=="united states",]
master<-na.exclude(master)
mastertrial<-glm(customeryes~.,data=master,family = binomial(link='logit'))
anova(mastertrial,test="Chisq")
mastermodel<-glm(customeryes~orisource+persona+ctoo+avgp,data=master,family = binomial(link='logit'))
summary(mastertrial)



  
  
dfml<-data.frame(ctod,otod,persona,orisource,rpersale,avgp,closescore)
b<-boxplot(rpersale)
lowerwhisker<-b$stats[1]
upperwhisker<-b$stats[5]
dfml<-dfml[dfml$rpersale>lowerwhisker & dfml$rpersale<upperwhisker,]
dfml<-na.exclude(dfml)
modelml<-lm(rpersale~.,data=dfml)
stepAIC(modelml)
modelml0<-lm(formula = rpersale ~ avgp, data = dfml)
summary(modelml0)



#drop off analysis
dftest<-data.frame(orisource,sub,lead,mql,sql,opp,cust)
subx<-dftest[dftest$sub==TRUE,]
dfsub<-data.frame(t(as.vector(table(subx[1]))))
colnames(dfsub)<-names(table(subx[1]))

leadx<-dftest[dftest$lead==TRUE,]
dflead<-data.frame(t(as.vector(table(leadx[1]))))
colnames(dflead)<-names(table(leadx[1]))

mqlx<-dftest[dftest$mql==TRUE,]
dfmql<-data.frame(t(as.vector(table(mqlx[1]))))
colnames(dfmql)<-names(table(mqlx[1]))

sqlx<-dftest[dftest$sql==TRUE,]
dfsql<-data.frame(t(as.vector(table(sqlx[1]))))
colnames(dfsql)<-names(table(sqlx[1]))

oppx<-dftest[dftest$opp==TRUE,]
dfopp<-data.frame(t(as.vector(table(oppx[1]))))
colnames(dfopp)<-names(table(oppx[1]))

custx<-dftest[dftest$cust==TRUE,]
dfcust<-data.frame(t(as.vector(table(custx[1]))))
colnames(dfcust)<-names(table(custx[1]))

dfplot<-rbind(dfsub,dflead,dfmql,dfsql,dfopp,dfcust)
rownames(dfplot)<-c('Subscriber','Lead','MQL','SQL','Opportunity','Customer')
barplot(as.matrix(dfplot),col=rainbow(8),horiz = TRUE,las=1)
legend("topright",legend=c('Subscriber','Lead','MQL','SQL','Opportunity','Customer'),fill=rainbow(8),  cex=1)

#percentage
ptable<-dfplot/rowSums(dfplot)
barplot(as.matrix(t(ptable)),col=rainbow(8),horiz = TRUE,las=1,cex.names = 1.2)
par(mar=c(3,10,3,3))
barplot(as.matrix(t(ptable)),col=rainbow(8),horiz = TRUE,las=1,cex.names = 1.2,ylim=range(0,14))

legend(0.85,14,legend=c('Subscriber','Lead','MQL','SQL','Opportunity','Customer'),fill=rainbow(8),  cex=0.8)


#time related analysis
trt<-data.frame(tr,x)
b<-boxplot(tr)
lowerwhisker<-b$stats[1]
upperwhisker<-b$stats[5]
trt<-trt[trt$tr>lowerwhisker & trt$tr<upperwhisker,]
trt<-na.exclude(trt)
plot(trt$x,trt$tr)






#sql to opp

stoodf<-na.exclude(data.frame(sql,cam))



#mql to sql logistic
mqlonly[is.na(mqlonly)==T]<-2
mqlonly[mqlonly==T]<-1
sqlonly[is.na(sqlonly)==T]<-2
sqlonly[sqlonly==T]<-1
ms<-sqlonly+mqlonly
ms[ms==4]<-NA
ms[ms==3]<-F
ms[ms==2]<-T
mtos<-ms & sqlyes
# mqlonly<-mqlyes
# mqlonly[mqlonly==FALSE]<-NA
# mtosno<-mqlonly==TRUE & sqlyes==FALSE
# mtosyes<-!mtosno
mtosdf<-data.frame(mtos,avgrpe,avgp,orisource,ed)

#mtosdf<-mtosdf[mtosdf$state %in% usstates,]
mtosdf<-na.exclude(mtosdf)
mtosmodel<-glm(mtos~.,data=mtosdf,family=binomial(link='logit'))
anova(mtosmodel,test='Chisq')
lrm(mtosmodel)
summary(mtosmodel)

tree1<-ctree(factor(mtos)~.,data=mtosdf)
plot(tree1, ep_args = list(justmin = 15), gp = gpar(fontsize = 10),margin=c(3,15,3,8))

png("mqltosql.png", res=80, height=800, width=2500) 
  plot(tree1, ep_args = list(justmin = 15), gp = gpar(fontsize = 14),margin=c(3,15,3,5))
dev.off()
#data partitioning
intrain<- createDataPartition(mtosdf$mtos,p=0.7,list=FALSE)
set.seed(2017)
training<- mtosdf[intrain,]
testing<- mtosdf[-intrain,]
LogModel <- glm(mtos ~ .,family=binomial(link="logit"),data=training)
print(summary(LogModel))
anova(LogModel,test="Chisq")

testing$mtos <- as.character(testing$mtos)
testing$mtos[testing$mtos==FALSE] <- "0"
testing$mtos[testing$mtos==TRUE] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$mtos)
print(paste('Logistic Regression Accuracy',1-misClasificError))


#dummyvariable logistic regression
customerno<-!(life=='Customer')

sub<-life=='Subscriber'
subonly<-subyes
subonly[subonly==FALSE]<-NA
lead<-life=="Lead"
leadonly<-leadyes
leadonly[leadonly==FALSE]<-NA
mql<-life=='Marketing qualified lead'
mqlonly<-mqlyes
mqlonly[mqlonly==FALSE]<-NA
sql<-life=="Sales qualified lead"
sqlonly<-sqlyes
sqlonly[sqlonly==FALSE]<-NA
opp<-life=="Opportunity"
opponly<-oppyes
opponly[opponly==FALSE]<-NA
cust<-life=='Customer'

custonly<-customeryes
custonly[custonly==FALSE]<-NA

subyes<-(life=='Subscriber'|life=='Customer'|life=="Lead"|life=='Marketing qualified lead'|life=="Sales qualified lead"|life=="Opportunity")
leadyes<-(life=="Lead"|life=='Marketing qualified lead'|life=="Sales qualified lead"|life=="Opportunity"|life=='Customer')
mqlyes<-(life=='Marketing qualified lead'|life=="Sales qualified lead"|life=="Opportunity"|life=='Customer')
sqlyes<-(life=="Sales qualified lead"|life=="Opportunity"|life=='Customer')
oppyes<-(life=="Opportunity"|life=='Customer')
customeryes<-life=='Customer'
otheryes<-(life=='Other'|life=='Evangelist')
#lead to mql logistic


# ltomno<-leadonly==TRUE & mqlyes==FALSE
# ltomyes<-!ltomno
leadonly[is.na(leadonly)==T]<-2
leadonly[leadonly==T]<-1
mqlonly[is.na(mqlonly)==T]<-2
mqlonly[mqlonly==T]<-1
leadm<-leadonly+mqlonly
leadm[leadm==4]<-NA
leadm[leadm==3]<-F
leadm[leadm==2]<-T

ltom<-leadm & mqlyes
ltomdf<-data.frame(ltom,orisource,avgrpe,avge,ed)
#ltomdf<-ltomdf[ltomdf$state %in% usstates,]
ltomdf<-na.exclude(ltomdf)
ltommodel<-glm(ltom~.,data=ltomdf,family=binomial(link='logit'))
lrm(ltommodel)

anova(ltommodel,test='Chisq')
summary(ltommodel)
format(round(exp(coef(ltosmodel))-1,3),scientifict=F)
#decision tree
tree2<-ctree(factor(ltom)~.,data=ltomdf)
plot(tree2, ep_args = list(justmin = 15), gp = gpar(fontsize = 15))
png("leadtomql.png", res=80, height=800, width=1600) 
  plot(tree2, ep_args = list(justmin = 15), gp = gpar(fontsize = 15))
dev.off()
# for lead to mql
intrain<- createDataPartition(ltomdf$ltom,p=0.7,list=FALSE)
set.seed(2017)
training<- ltomdf[intrain,]
testing<- ltomdf[-intrain,]
LogModel <- glm(ltom ~ .,family=binomial(link="logit"),data=training)
treex<-ctree(factor(ltom)~.,data=training)
plot(treex, ep_args = list(justmin = 15), gp = gpar(fontsize = 13))
print(summary(LogModel))
anova(LogModel,test="Chisq")

testing$ltom <- as.character(testing$ltom)
testing$ltom[testing$ltom==FALSE] <- "0"
testing$ltom[testing$ltom==TRUE] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$ltom)
print(paste('Logistic Regression Accuracy',1-misClasificError))

#opp to cust
opponly[is.na(opponly)==T]<-2
opponly[opponly==T]<-1
custonly[is.na(custonly)==T]<-2
custonly[custonly==T]<-1
oppcust<-opponly+custonly
oppcust[oppcust==4]<-NA
oppcust[oppcust==3]<-F
oppcust[oppcust==2]<-T

otoc<-oppcust & custonly
otocdf<-na.exclude(data.frame(otoc,orisource,avgp,ed))
otocmodel<-glm(otoc~.,data=otocdf,family=binomial(link='logit'))
lrm(otocmodel)
anova(otocmodel,test='Chisq')  
summary(otocmodel)

# for opp to cust
intrain<- createDataPartition(otocdf$otoc,p=0.7,list=FALSE)
set.seed(1024)
training<- otocdf[intrain,]
testing<- otocdf[-intrain,]
LogModel <- glm(otoc ~ .,family=binomial(link="logit"),data=training)
print(summary(LogModel))
anova(LogModel,test="Chisq")

testing$otoc <- as.character(testing$otoc)
testing$otoc[testing$otoc==FALSE] <- "0"
testing$otoc[testing$otoc==TRUE] <- "1"
fitted.results <- predict(LogModel,newdata=testing,type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)
misClasificError <- mean(fitted.results != testing$otoc)
print(paste('Logistic Regression Accuracy',1-misClasificError))
tree3<-ctree(factor(otoc)~.,data=otocdf)
plot(tree3, ep_args = list(justmin = 15), gp = gpar(fontsize = 16),margin=c(3,17,3,8))

png("opptocust.png", res=80, height=800, width=1600) 
  plot(tree3, ep_args = list(justmin = 15), gp = gpar(fontsize = 15),margin=c(3,17,3,8))
dev.off()

#
dftest<-na.exclude(data.frame(otoc,dayftonow,avgp,orisource))
testmodel<-glm(otoc~.,data=dftest,family=binomial(link = 'logit'))
anova(testmodel,test='Chisq')
lrm(testmodel)
summary(testmodel)

plot(ctree(factor(mtos)~.,data=dftest),ep_args = list(justmin = 15))



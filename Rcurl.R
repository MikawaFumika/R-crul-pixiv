library(RCurl)
library(XML)
myHttpheader <- c(
  
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  
  "Accept-Language"="en-us",
  
  "Connection"="keep-alive",
  
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
  
)

Date and time
start<-as.Date("2017-01-01")    
end<-as.Date("2017-11-29") 
datetime=seq(from=start,to=end,by=1)

cycling
tagsday=list(0)
for(i in 1:length(datetime))
{
date=paste0(unlist(strsplit(as.character(datetime[i]),split='-'))[1],unlist(strsplit(as.character(datetime[i]),split='-'))[2],unlist(strsplit(as.character(datetime[i]),split='-'))[3],sep='')
urlpart1='https://www.pixiv.net/ranking.php?mode=daily&date='
url=paste0(urlpart1,as.character(date))
webpage <- getURL(url,httpheader=myHttpheader)
webhtml=unlist(strsplit(webpage,split = "\n"))
tagcontainer=grep(pattern='ranking-items-container',webhtml,value=T)
v=unlist(strsplit(tagcontainer,split = "<section"))
tagsdaymeta=character(0)
for(j in 2:length(v))
{tagsdaymeta[j-1]=substr(unlist(strsplit(unlist(strsplit(v[j],split = "data-tags")),split='data-user-id'))[2],3,nchar(unlist(strsplit(unlist(strsplit(v[j],split = "data-tags")),split='data-user-id'))[2])-1)}

tagsday[[i]]<-tagsdaymeta
}
tagsall=strsplit(unlist(tagsday),split=' ')

keyword='Fate/Apocrypha'
stamonthly=numeric(0)
for(i in 1:9)
{stamonthly[i]=length(grep(pattern=keyword,unlist(tagsday[which(substr(as.character(datetime),6,7)==paste0('0',as.character(i)))]),value=F))/length(unlist(tagsday[which(substr(as.character(datetime),6,7)==paste0('0',as.character(i)))]))}
for(i in 10:11)
{stamonthly[i]=length(grep(pattern=keyword,unlist(tagsday[which(substr(as.character(datetime),6,7)==as.character(i))]),value=F))/length(unlist(tagsday[which(substr(as.character(datetime),6,7)==as.character(i))]))}
month=c('一月','二月','三月','四月','五月','六月','七月','八月','九月','十月','十一月')
par(family='STKaiti')
barplot(stamonthly,main=keyword,names.arg=month)

sumtagsall=summary(as.factor(unlist(tagsall)))


par(family='STKaiti',mar=c(8,3,2,2))
barplot(sumtagsall[1:20],cex.names=0.7,las=2)

a=length(grep(pattern='パンスト',unlist(tagsall),value=F))/length(unlist(tagsall))
b=length(grep(pattern='ひざの靴下',unlist(tagsall),value=F))/length(unlist(tagsall))
c=length(grep(pattern='ガーターストッキング',unlist(tagsall),value=F))/length(unlist(tagsall))
barplot(c(a,b,c),main='连裤袜，过膝袜还是吊带袜？',names.arg=c('パンスト','ひざの靴下','ガーターストッキング'))

x=c(length(grep(pattern='Fate',unlist(tagsday11),value=F))/length(unlist(tagsday11)),length(grep(pattern='宝石',unlist(tagsday11),value=F))/length(unlist(tagsday11)),length(grep(pattern='VOCALOID',unlist(tagsday11),value=F))/length(unlist(tagsday11)),length(grep(pattern='東方',unlist(tagsday11),value=F))/length(unlist(tagsday11)),length(grep(pattern='アズールレーン',unlist(tagsday11),value=F))/length(unlist(tagsday11)),length(grep(pattern='艦これ',unlist(tagsday11),value=F))/length(unlist(tagsday11)))
barplot(x,main="",names.arg=c())

unique(tags)


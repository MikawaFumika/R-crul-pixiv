library(RCurl)
library(XML)
library(rvest)
library(xml2)
library(httr)


myHttpheader <- c(
  
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  
  "Accept-Language"="en-us",
  
  "Connection"="keep-alive",
  
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"
  
  
)

#被关注
#初始化要求取的红人
url='https://www.douban.com/people/cenkomayoi/rev_contacts'
#第一次爬取
pgsession <- html_session(url)
pgform <- html_form(pgsession)[[1]]
captchaid=unlist(strsplit(as.character(pgform)[5],split='captcha-id'))[3]
captchaid=substr(captchaid[1],30,56)
capfigurl=paste0("https://www.douban.com/misc/captcha?id=",captchaid,"&size=s")
capfigurl

#给出第一个关注列表信息
filled_form <- set_values(pgform,
                          'form_email'='ye_ys@qq.com',
                          'form_password'='yys20191',
                          'captcha-solution'='chance')
sbmt <- submit_form(pgsession,filled_form)
position<-sbmt %>% html_nodes("dl.obu") %>% html_children()
#计算focus人数
info<-sbmt %>% html_nodes("div.info") %>% html_children()
info=unlist(strsplit(as.character(info[1]),split = '人'))
info=unlist(strsplit(info[2],split = ')'))
info=as.numeric(substr(info[1],2,nchar(info[1])))
x=floor(info/70)
#制造网页链接
webpage=character(0)
webpage[1]=url
for(i in 1:x)
{
  p=i*70
webpage[1+i]=paste0(url,'?start=',p)
}
#初始化focus网页串
focusweb=as.character(position)
#重复循环提取网页
pgsession <- html_session(webpage[7])
pgform <- html_form(pgsession)[[1]]
captchaid=unlist(strsplit(as.character(pgform)[5],split='captcha-id'))[3]
captchaid=substr(captchaid[1],30,56)
capfigurl=paste0("https://www.douban.com/misc/captcha?id=",captchaid,"&size=s")
capfigurl
#重复验证码报头
filled_form <- set_values(pgform,
                          'form_email'='13240001871',
                          'form_password'='Tidewater2019130011',
                          'captcha-solution'='ticket')
sbmt <- submit_form(pgsession,filled_form)
position<-sbmt %>% html_nodes("dl.obu") %>% html_children()
#叠加focus网页
webids=as.character(position)
focusweb=c(focusweb,webids)

focusurl=character(0)
fobefo=list(0)
for(i in 411:info)
{
  focusurl[i]=unlist(strsplit(focusweb[i*2],split='"'))[2];
  pp=html_session(focusurl[i]);
  ppp=pp %>% html_nodes("span.pl") %>% html_children();
  pppp=pp %>% html_nodes("div.stream-items") %>% html_children();
  if(status_code(pp)==200)
  {
  if(length(grep(pattern='的广播',pppp,value=F))>0)
  {
  fo=unlist(strsplit((unlist(strsplit(as.character(ppp[grep(pattern='>成员',ppp,value=F)]),split='成员')))[2],split='<'))[1];
  qqq=pp %>% html_nodes("p.rev-link") %>% html_children();
  befo=unlist(strsplit(unlist(strsplit(as.character(qqq),split='被'))[2],split='人'))[1];
  fobefo[[i]]=c(as.numeric(fo),as.numeric(befo));
  }
  }
  Sys.sleep(runif(1, min = 5, max = 10));
}
  
#关注列表

#输入url
url='https://www.douban.com/people/you-kyan-yui-17/contacts'

#跑fo web list
pgsession <- html_session(url)
pgform <- html_form(pgsession)[[1]]
captchaid=unlist(strsplit(as.character(pgform)[5],split='captcha-id'))[3]
captchaid=substr(captchaid[1],30,56)
capfigurl=paste0("https://www.douban.com/misc/captcha?id=",captchaid,"&size=s")
capfigurl
#验证码
filled_form <- set_values(pgform,
                          'form_email'='ye_ys@qq.com',
                          'form_password'='yys2019130011',
                          'captcha-solution'='though')
filled_form <- set_values(pgform,
'form_email'='ye_ys@qq.com',
'form_password'='yys2019130011')
sbmt <- submit_form(pgsession,filled_form)

#做网页链接 提取链接内容
position<-sbmt %>% html_nodes("dl.obu") %>% html_children()
focusurl1=character(0)
fobefo1=list(0)
for(i in 1:(length(position)/2))
{
  focusurl1[i]=unlist(strsplit(as.character(position[i*2]),split='"'))[2];
  pp=html_session(focusurl1[i]);
  ppp=pp %>% html_nodes("span.pl") %>% html_children();
  pppp=pp %>% html_nodes("div.stream-items") %>% html_children();
  if(status_code(pp)==200)
  {
    if(length(grep(pattern='的广播',pppp,value=F))>0)
    {
      fo=unlist(strsplit((unlist(strsplit(as.character(ppp[grep(pattern='>成员',ppp,value=F)]),split='成员')))[2],split='<'))[1];
      qqq=pp %>% html_nodes("p.rev-link") %>% html_children();
      befo=unlist(strsplit(unlist(strsplit(as.character(qqq),split='被'))[2],split='人'))[1];
      fobefo1[[i]]=c(as.numeric(fo),as.numeric(befo));
    }
  }
  Sys.sleep(runif(1, min = 2, max = 10));
}

fobefo2=fobefo1
fobefo3=fobefo1
#统计
a=character(0)
b=character(0)
for(i in 1:info)
{
  if(is.null(fobefo[[i]])=='FALSE')
    {a[i]=fobefo[[i]][1];
    b[i]=fobefo[[i]][2];}
}

for(i in 1:(length(fobefo2)))
{
  if(is.null(fobefo2[[i]])=='FALSE')
  {a[i]=fobefo2[[i]][1];
  b[i]=fobefo2[[i]][2];}
}

length(which(is.na(a)==F))/(length(fobefo1))

clea=which(is.na(a) |is.na(b) | a==0 | b==0)
a<-a[-(clea)]
b<-b[-(clea)]

plot(a,b,type='p',cex=0.3,xlim=c(0,2000),ylim=c(0,2000))

c=as.numeric(a)-as.numeric(b)
d=as.numeric(c)/(as.numeric(b)+as.numeric(a))
hist(c,breaks=500,xlim=c(-1000,1000))
hist(d,xlim=c(-1,1),breaks=20,main="Red------Normal------Robot",ylab="",xlab="weight")

mean(c[c>-5000])






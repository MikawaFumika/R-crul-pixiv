library(RCurl)
library(XML)
library(rvest)
library(xml2)
library(httr)
library(jpeg)
library(imager)
library(downloader)

myHttpheader <- c(
  
  "User-Agent"="Mozilla/5.0 (Windows; U; Windows NT 5.1; zh-CN; rv:1.9.1.6) ",
  
  "Accept"="text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  
  "Accept-Language"="en-us",
  
  "Connection"="keep-alive",
  
  "Accept-Charset"="GB2312,utf-8;q=0.7,*;q=0.7"

)

url='https://www.douban.com'
pgsession <- html_session(url)
pgform <- html_form(pgsession)

filled_form <- set_values(pgform,
                          'form_email'='ye',
                          'form_password'='y1')
sbmt <- submit_form(pgsession,filled_form)
pgform1 <- html_form(sbmt)
filled_form1 <- set_values(pgform1[[2]],
                          'comment'='ccc')

sbmt1 <- submit_form(pgsession,filled_form1)



setwd("/Users/yeyusong/Desktop/python")
png <- getBinaryURL("https://www.pixiv.net/member_illust.php?mode=medium&illust_id=",httpheader=myHttpheader)
download(picture[i],paste("F:/数据可视化/Image/picture",i,".jpg",sep = "")
url <- 'https://www.pixiv.net/member_illust.php?mode=medium&illust_id='
picture<- html_session(url) %>% html_nodes("div._layout-thumbnail ui-modal-trigger") %>% html_children()
download('https://i.pximg.net/c/240x480/img-master/img/2018/02/08/00/00/00/67162181_p0_master1200.jpg','/Users/yeyusong/Desktop/python/pic1.jpg',mode = "wb")

filled_form1 <- set_values(pgform1[[3]],
                        'image'='pic1.jpg')


captchaid=unlist(strsplit(as.character(pgform)[5],split='captcha-id'))[3]
captchaid=substr(captchaid[1],30,56)
capfigurl=paste0("https://www.douban.com/misc/captcha?id=",captchaid,"&size=s")
capfigurl

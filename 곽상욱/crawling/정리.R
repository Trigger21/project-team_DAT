# R Å©·Ñ¸µ ÇÏ´Â¹ý

#install.packages("rvest")
library(rvest)
library(dplyr)
library(stringr)

# ¼±ºí·Ï
html<- read_html("https://www.glowpick.com/category_product?id=3")

# Á¦Ç°±º ³» 1~3À§ total
total <- html_nodes(html,css = "#category1 > div > div > a > span")%>% 
  html_text()
total<- strsplit(total,"\n")

total<- unlist(total)
total<- str_trim(total)

total <- grep("[[:alpha:]]|[[:digit:]]",total,value = T)
total   #1¹ø ¸ÞÀÌÄ¿ , 2¹ø Á¦Ç°¸í, 3¹ø °¡°Ý, 4¹ø ÆòÁ¡

#¸ÞÀÌÄ¿,Á¦Ç°¸í,°¡°Ý,ÆòÁ¡º°·Î ³ª´©±â
maker1<- total[c(1,5,9)]
title1<- total[c(2,6,10)]
price1<- total[c(3,7,11)]
star1<- total[c(4,8,12)]

# Á¦Ç°±º ³» 4~10À§ ¸ÞÀÌÄ¿
maker2 <- html_nodes(html,css = "#category1 > div > div.hidden-xs.pcList > div > div > div.item3 > a > h5")%>% 
  html_text()
maker2

# Á¦Ç°±º ³» 4~10À§ Á¦Ç°¸í
title2 <- html_nodes(html,css = "#category1 > div > div.hidden-xs.pcList > div > div > div.item3 > a > p")%>% 
  html_text()
title2

# Á¦Ç°±º ³» 4~10À§ °¡°Ý
price2 <- html_nodes(html,css = "#category1 > div > div.hidden-xs.pcList > div > div > div.item4")%>% 
  html_text()
price2<- gsub("\n","",price2)
price2<- str_trim(price2)

# Á¦Ç°±º ³» 4~10À§ ÆòÁ¡
star2 <- html_nodes(html,css = "#category1 > div > div.hidden-xs.pcList > div > div > div.item5")%>% 
  html_text()
star2<- gsub("\n","",star2)
star2<- gsub("Á¡","",star2)
star2<- str_trim(star2)
star2<- unlist(strsplit(star2,'/'))
star2<- star2[c(1,3,5,7,9,11,13)]
star2

#ÅëÇÕ
maker<- append(maker1,maker2)
title<- append(title1,title2)
price<- append(price1, price2)
star<- append(star1,star2)

maker
title
price
star


# Á¦Ç°±º ³» 1~3À§ µÞ¸µÅ©
url1 <- html_nodes(html,"#category1 > div > div > a")%>%
  html_attr('href')
url1

# Á¦Ç°±º ³» 4~10À§ µÞ¸µÅ©
url2 <- html_nodes(html,".item3Link" )%>% 
  html_attr("href")
url2

url<- append(url1,url2)

text <- c()
id <- c()
for(j in 1:10){
  
  html2 <- read_html(paste0("https://www.glowpick.com",url)[j])
  html2
  
  for(i in 1:20){
    id1<- html_node(html2, paste0("#reviewList > ul > li:nth-child(",i,") > div:nth-child(2) > div > div.reviewerInfo > div:nth-child(1)"))%>%
      html_text() 
    id<- c(id,id1)
    
    text1<- html_node(html2, paste0("#reviewList > ul > li:nth-child(",i,") > div:nth-child(2) > div > p"))%>%
      html_text() 
    text<- c(text,text1)
  }
}

#ÅëÇÕ
text
id

#id ¾È¿¡ ÀÖ´ø Á¤º¸¸¦ name, age, skin_type ·Î ³ª´©´Â ÀÛ¾÷
name<- c()
a<- c()
for(i in 1:length(id)){
  name<- c(name,str_split(id, " ")[[i]][1])
  a<- c(a,str_split(id, " ")[[i]][2])
}



age<- c()
type<- c()
for(i in 1:length(id)){
  age<- c(age,str_split(a, "/")[[i]][1])
  type<- c(type,str_split(a, "/")[[i]][2])
}

age<- gsub('\\(',"",age)
age<- gsub('¼¼',"",age)
type<- gsub("\\)","",type)

age
name
type

maker
title
price
star

#text ÁÖº¯ Á¤¸®
text<- gsub("\\\r"," ",text)
text<- gsub("\\\n"," ",text)
text<- str_trim(text)
text

review <- data.frame(maker = rep(maker,20), title = rep(title,20), price = rep(price,20), star = rep(star, 20), text = text, age = age, skin_type = type, name = name,stringsAsFactors = F)
str(review)
levels(review[,'maker'])<- maker
review[,'maker']<-sort(review[,'maker'])

levels(review[,'title'])<- title
review[,'title']<-sort(review[,'title'])

levels(review[,'price'])<- price
review[,'price']<-sort(review[,'price'])

levels(review[,'star'])<- star
review[,'star']<-sort(review[,'star'])

review

write.csv(review,"c:/r/review_project")

unlist(text)

text1<- SimplePos09(unlist(text))

text1<-unlist(str_match_all(text1, '([A-Z°¡-ÆR]+)/N'))

text1<-text1[!str_detect(text1, '/')]

text2<- sort(table(text1),decreasing = T)

text1

text5<- text2[nchar(names(text2))>1]
text5

wordcloud2(text5,size = 2)



#½ÉÇÃÆ÷½º ¾È¾²°í ¶ç¿ö¾²±â ±âÁØÀ¸·Î ´Ü¾î ºÐÇØ
unlist(text)

test1 <- strsplit(unlist(text),split=" ")

test2 <- unlist(test1)
test2
td<- data.frame(name=a,cnt = length(a),stringsAsFactors = F)


a<- read.table("c:/r/aaa4.txt",stringsAsFactors = F)
a<- a$V1
a<- unique(a)
a


for(i in a){
  td$cnt[td$name==i]<- length(test2[grep(i,test2,ignore.case = TRUE)])
}

td


#Á¶¸³ qwe ÇÏ°í td
text6<- text5[text5<=5]

qwe<- data.frame(text6)
names(qwe)<- names(td)
head(qwe)
  td$cnt 
text7 <- merge(td,qwe,all=T)

wordcloud2(text7,size=2)


#20´ë ¸®ºä µû·Î »©±â
review$age<- as.numeric(review$age)
review[review$age> 20&review$age < 30,]

text_20<- review[review$age> 20&review$age < 30,"text"]

#½ÉÇÃÆ÷½º ¾È¾²°í ¶ç¿ö¾²±â ±âÁØÀ¸·Î ´Ü¾î ºÐÇØ
unlist(text_20)

test1 <- strsplit(unlist(text_20),split=" ")

test2 <- unlist(test1)

a<- read.table("c:/r/aaa4.txt",stringsAsFactors = F)
a<- a$V1
a<- unique(a)
a

td_20<- data.frame(name=a,cnt = length(a),stringsAsFactors = F)


for(i in a){
  td_20$cnt[td$name==i]<- length(test2[grep(i,test2,ignore.case = TRUE)])
}

td_20

wordcloud2(td_20)


#Á¶¸³ qwe ÇÏ°í td
text6<- text5[text5<=5]

qwe<- data.frame(text6)
names(qwe)<- names(td)
head(qwe)
td$cnt 
text7 <- merge(td,qwe,all=T)

wordcloud2(text7)

ggplot(td_c30,aes(x=name,y=cnt))+
  geom_bar(stat = "identity",fill = rainbow(length(td_c20$name)))+
  labs(title = '30´ë ÁÖ¿ä Å°¿öµå',x='Å°¿öµå',y='È½¼ö')+
  theme(plot.title=element_text(face='bold', color='darkblue',hjust=0.5))+
  theme(axis.text.x=element_text(angle=45,hjust=1,vjust=1,colour="blue",size=7))+
  theme(axis.text.y=element_text(face='bold.italic',color="brown",size=10))

pal <- brewer.pal(20,"Set3")

ggplot(td_12,aes(x=reorder(name, +cnt),y=cnt))+
  geom_bar(stat = "identity",fill = rainbow_hcl(20))+
  labs(title = '30´ë ÁÖ¿ä Å°¿öµå',x='Å°¿öµå',y='È½¼ö')+
  theme(plot.title=element_text(face='bold', color='darkblue',hjust=0.5))+
  coord_flip()+
  theme(axis.text.y=element_text(hjust=1,vjust=1,colour="darkblue",size=10))+
  theme(axis.text.x=element_text(face='bold.italic',color="brown",size=10))


#30´ë ¸®ºä µû·Î »©±â
review$age<- as.numeric(review$age)
review[review$age> 30,]

text_30<- review[review$age> 30,"text"]

#½ÉÇÃÆ÷½º ¾È¾²°í ¶ç¿ö¾²±â ±âÁØÀ¸·Î ´Ü¾î ºÐÇØ
unlist(text_30)

test1 <- strsplit(unlist(text_30),split=" ")

test2 <- unlist(test1)

a<- read.table("c:/r/aaa4.txt",stringsAsFactors = F)
a<- a$V1
a<- unique(a)
a

td_30<- data.frame(name=a,cnt = length(a),stringsAsFactors = F)


for(i in a){
  td_30$cnt[td$name==i]<- length(test2[grep(i,test2,ignore.case = TRUE)])
}

td_30

wordcloud2(td_30)


#Á¶¸³ qwe ÇÏ°í td
text6<- text5[text5<=5]

qwe<- data.frame(text6)
names(qwe)<- names(td)
head(qwe)
td$cnt 
text7 <- merge(td,qwe,all=T)

wordcloud2(text7)









wordcloud2(td)

test3<- sort(table(test2),decreasing = T)

head(test3,50)

wordcloud2(test3)

p <- read.csv("c:/r/review_project.csv")
p


a<- read.table("c:/r/aaa3.txt",stringsAsFactors = F)
a<- a$V1
a<- unique(a)
a

b<- c(rep(0,length(a)))
c<- c()
for(i in 1:length(text)){
  c<- str_count(text[i],ignore.case(a))
  b<- b+c
}

ttt<- data.frame(name= a, cnt = b)
ttt
wordcloud2(ttt)

ttt3<- data.frame(name=a,cnt = length(a),stringsAsFactors = F)
for(i in a){
  ttt$cnt[ttt$name==i]<- length(t4[grep(i,t4,ignore.case = TRUE)])
}
table(ttt)
ttt2<- data.frame(cnt = b)
rownames(ttt2)<- a

useSejongDic()
write(a,'c:/r/aaa4.txt')
buildDictionary(ext_dic="sejong",user_dic=data.frame(read.lines("c:/r/aaa4.txt"),"ncn"),replace_usr_dic=T)
help("read.lines")
'??read.lines'
mergeUserDic(data.frame(a,"ncn"))

data = sapply(text,extractNoun,USE.NAMES = F)
data1<- unlist(data)
data2<- Filter(function(x){nchar(x)>=2},data1)
data2
t_data<- table(data2)
wordcloud2(t_data,shape = "star")

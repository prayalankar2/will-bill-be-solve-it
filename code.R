library(readr)
pill<-read_csv("D:/New folder (7)/bill/problems.csv")
sill<-read_csv("D:/New folder (7)/bill/submissions.csv")
uill<-read_csv("D:/New folder (7)/bill/users.csv")

tpill<-read_csv("D:/New folder (7)/bill/test/problems.csv")
ttill<-read_csv("D:/New folder (7)/bill/test/test.csv")
tuill<-read_csv("D:/New folder (7)/bill/test/users.csv")

library(dplyr)
library(car)
library(splitstackshape)

##########  processing "train submission filee"

str(sill)

sill$result[sill$result=="AC"]<-1
sill$result[sill$result=="PAC"]<-0
sill$result<-as.factor(sill$result)

sill<-sill[,-c(3,5,6)]
sill$result<-as.numeric(sill$result)

df22 <- sill %>% group_by(user_id , problem_id) %>% 
  summarise_each(funs(sum))

df22$result[df22$result>1]<-1



#############   processing "train users"

uill<-concat.split.multiple(uill, "skills", "|")

uill[,p]<-0
uill$`Java (openjdk 1.7.0_09)`<-0

df24$Befunge[is.na(df24$Befunge)]<-0
df24$`Python 3`[is.na(df24$`Python 3`)]<-0
for(j in vi)
  uill$`C++`[uill[[j]]=="C++"]<-1
for(j in vi)
  uill$Text[uill[[j]]=="Text"]<-1
for(j in vi)
  uill$`JavaScript(Rhino)`[uill[[j]]=="JavaScript(Rhino)"]<-1
for(j in vi)
  uill$C[uill[[j]]=="C"]<-1
for(j in vi)
  uill$Java[uill[[j]]=="Java"]<-1
for(j in vi)
  uill$Python[uill[[j]]=="Python"]<-1
for(j in vi)
  uill$`C#`[uill[[j]]=="C#"]<-1
for(j in vi)
  uill$PHP[uill[[j]]=="PHP"]<-1
for(j in vi)
  uill$`Java (openjdk 1.7.0_09)`[uill[[j]]=="Java (openjdk 1.7.0_09)"]<-1
for(j in vi)
  uill$Scala[uill[[j]]=="Scala"]<-1
for(j in vi)
  uill$`C++ (g++ 4.8.1)`[uill[[j]]=="C++ (g++ 4.8.1)"]<-1
for(j in vi)
  uill$`R(RScript)`[uill[[j]]=="R(RScript)"]<-1
for(j in vi)
  uill$Lisp[uill[[j]]=="Lisp"]<-1
for(j in vi)
  uill$Clojure[uill[[j]]=="Clojure"]<-1
for(j in vi)
  uill$Pascal[uill[[j]]=="Pascal"]<-1
for(j in vi)
  uill$Haskell[uill[[j]]=="Haskell"]<-1
for(j in vi)
  uill$Befunge[uill[[j]]=="Befunge"]<-1
for(j in vi)
  uill$Perl[uill[[j]]=="Perl"]<-1
for(j in vi)
  uill$`Python 3`[uill[[j]]=="Python 3"]<-1
for(j in vi)
  uill$Go[uill[[j]]=="Go"]<-1
for(j in vi)
  uill$`Objective-C`[uill[[j]]=="Objective-C"]<-1
for(j in vi)
  uill$`JavaScript(Node.js)`[uill[[j]]=="JavaScript(Node.js)"]<-1
for(j in vi)
  uill$Ruby[uill[[j]]=="Ruby"]<-1
for(j in vi)
  uill$JavaScript[uill[[j]]=="JavaScript"]<-1


uill<-uill[,-2]
str(uill)
uill<-uill[,-c(5:23)]

uill$user_type[uill$user_type=="W"]<-1
uill$user_type[uill$user_type=='S']<-2
uill$user_type[is.na(uill$user_type)]<-0
uill$user_type<-as.numeric(uill$user_type)

for(i in names(uill[,c(5:30)]))
  uill[[i]]<-as.factor(uill[[i]])

######   processing "train problems"

for(i in pi)
{
  for(j in t)
  { pill[[i]][pill[[j]]==i]<-1
  
  }
}

for(i in pi)
  pill[[i]][is.na(pill[[i]])]<-0

pill$level<-recode(pill$level," 'E'=1;
                    'E-M'=2; 'M'=3; 'M-H'=4; 'H'=6; 
                    'O'=2;")
str(tpill)
str(pill)
pill$level[is.na(pill$level)]<-2
tpill$level[is.na(tpill$level)]<-2


pill<-pill[,-c(7:11)] 




###############processing "tesr users"

tuill<-concat.split.multiple(tuill, "skills", "|")

p<-unique(tuill$skills_01)
tuill[,p]<-0
tuill$`Java (openjdk 1.7.0_09)`<-0


str()
tuill$Befunge[is.na(tuill$Befunge)]<-0
tuill$`Python 3`[is.na(tuill$`Python 3`)]<-0

for(j in vi)
  tuill$`C++`[tuill[[j]]=="C++"]<-1
for(j in vi)
  tuill$Text[tuill[[j]]=="Text"]<-1
for(j in vi)
  tuill$`JavaScript(Rhino)`[tuill[[j]]=="JavaScript(Rhino)"]<-1
for(j in vi)
  tuill$C[tuill[[j]]=="C"]<-1
for(j in vi)
  tuill$Java[tuill[[j]]=="Java"]<-1
for(j in vi)
  tuill$Python[tuill[[j]]=="Python"]<-1
for(j in vi)
  tuill$`C#`[tuill[[j]]=="C#"]<-1
for(j in vi)
  tuill$PHP[tuill[[j]]=="PHP"]<-1
for(j in vi)
  tuill$`Java (openjdk 1.7.0_09)`[tuill[[j]]=="Java (openjdk 1.7.0_09)"]<-1
for(j in vi)
  tuill$Scala[tuill[[j]]=="Scala"]<-1
for(j in vi)
  tuill$`C++ (g++ 4.8.1)`[tuill[[j]]=="C++ (g++ 4.8.1)"]<-1
for(j in vi)
  tuill$`R(RScript)`[tuill[[j]]=="R(RScript)"]<-1
for(j in vi)
  tuill$Lisp[tuill[[j]]=="Lisp"]<-1
for(j in vi)
  tuill$Clojure[tuill[[j]]=="Clojure"]<-1
for(j in vi)
  tuill$Pascal[tuill[[j]]=="Pascal"]<-1
for(j in vi)
  tuill$Haskell[tuill[[j]]=="Haskell"]<-1
for(j in vi)
  tuill$Befunge[tuill[[j]]=="Befunge"]<-1
for(j in vi)
  tuill$Perl[tuill[[j]]=="Perl"]<-1
for(j in vi)
  tuill$`Python 3`[tuill[[j]]=="Python 3"]<-1
for(j in vi)
  tuill$Go[tuill[[j]]=="Go"]<-1
for(j in vi)
  tuill$`Objective-C`[tuill[[j]]=="Objective-C"]<-1
for(j in vi)
  tuill$`JavaScript(Node.js)`[tuill[[j]]=="JavaScript(Node.js)"]<-1
for(j in vi)
  tuill$Ruby[tuill[[j]]=="Ruby"]<-1
for(j in vi)
  tuill$JavaScript[tuill[[j]]=="JavaScript"]<-1

tuill<-tuill[,-c(5:23)]

tuill$user_type[tuill$user_type=="W"]<-1
tuill$user_type[tuill$user_type=='S']<-2
tuill$user_type[is.na(tuill$user_type)]<-0

str(tuill)

tuill$user_type<-as.numeric(tuill$user_type)
tuill<-tuill[,-2]

  

############## processing "test problems"
str(tpill)

for(i in pi)
{
  for(j in t)
  { tpill[[i]][tpill[[j]]==i]<-1
  
  }
}

for(i in pi)
  tpill[[i]][is.na(tpill[[i]])]<-0

pill$level<-recode(pill$level," 'E'=20;
                    'E-M'=30; 'M'=50; 'M-H'=80; 'H'=100; 
                    'O'=40;")

str(tpill)
pill<-pill[,-c(7:11)]
str(pill)
tpill$level[tpill$level==30]<-2

tpill<-tpill[,-c(7:12)] # removing tags 

for(i in names(tpill[,c(7:84)]))
  tpill[[i]]<-as.factor(tpill[[i]])




#########     merging   train   data


df23<-merge(df22,uill,by ="user_id")
df24<-merge(df23,pill,by ="problem_id")

df24<-df24[,-c(1,2)]



##########merging test data

dim(tf24)
tf23<-merge(ttill,tuill,by ="user_id")
tf24<-merge(tf23,tpill,by ="problem_id")

importv<-tf24[,3]   #saving problem index as they will change suring the process of merging
tf24<-tf24[,-c(1:3)]


##############  prediction


ll<-df24[,1]
ll<-as.factor(ll)
df25<-df24[,-1]

lmFitAll <- randomForest(df25[,], 
                         ll,
                         ntree=300,mtry=112,nodesize = 60,
                         sampsize=15000, oob_score = TRUE,
                         do.trace=TRUE)

ss<-predict(lmFitAll,tf24)
aba<-data.frame("Id"=importv,"solved_status"=s)
write_csv(aba,"D:aba.csv")

########using xgboost

param <- list("objective" = "binary:logistic",
              "eval_metric" = "error",
              "nthread" = 7,missing=NA)

df24$result<-as.factor(df24$result)
ll1<-df24$result
df25<-df24[,-1]
trainX<-as.matrix(df25)
ll1<-as.integer(ll1)-1
library(xgboost)
bst = xgboost(param=param, data = trainX, label = ll1,
              nrounds=2500, max.depth=20, eta=0.05, min_child_weight=8,missing = NA)



testx<-as.matrix(tf25)
res<-predict(bst,testx,missing=NA)


badl<-data.frame("Id"=importv,"solved_status"=res)

length(badl$solved_status[badl$solved_status>=0.5])

badl$solved_status[badl$solved_status>=0.5]<-1
badl$solved_status[badl$solved_status<0.5]<-0

write_csv(badl,"D:xge1838.csv")


########using neural net
str(df24)
library(nnet)

df24$result<-as.factor(df24$result)

fit1<-nnet(result ~., df24, size = 10,
          rang = 0.1, decay = 5e-1, maxit = 1500)

str(tf24)
predicted2<-predict(fit1,tf24,type="raw")  
predicted3<-predict(fit1,tf24,type="class")  

predicted2<-predict(fit1,tf24,type="raw")  

predicted2[predicted2>0.4]<-1
predicted2[predicted2<=0.4]<-0

nn1<-data.frame("Id"=importv,"solved_status"=predicted2)
write_csv(nn1,"D:nn23.csv")

#################### ensembling

asd<-read_csv("D:nn23.csv")
asd2<-read_csv("D:xge1838.csv")
asd1<-read_csv("D:aba.csv")

temp1<-0.7*(asd2$solved_status+asd$solved_status+
              asd1$solved_status+asd3$solved_status)

temp1[temp1>0.7]<-1
temp1[temp1<=0.7]<-0
badl<-data.frame("Id"=asd$Id,"solved_status"=temp1)
write_csv(badl,"D:e52.csv")




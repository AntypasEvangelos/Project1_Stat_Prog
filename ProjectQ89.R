### Project 1


a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)]  ## strip out verse numbers

### Q4
#Define Function split_punct
split_punct<-function(text,pun){
#Using for loop to consider all of words with different punctuation marks   
  for(i in 1:length(pun)){
  remove<-gsub(pun[i],"",text,fixed=TRUE) #Remove words with punctuation marks[i]
  check<-grep(pun[i],text,fixed=TRUE)
  empty<- rep("",length(text)+length(check))
  sym<- check+1:length(check)
   empty[sym]   <- pun[i] 
   empty[-sym]<- remove
   text<-empty
  }
  text
}  

### Q5
a<-split_punct(a,c(",", ".", ";", "!", ":","?"))  
  

#Q6 
a<-tolower(a)
a_uni<-unique(a)
match(a,a_uni)
occur<-tabulate(match(a,a_uni))

#check<- function(a){
  #length(which(occur>a))
#}
a_uni[which(occur>162)]
length(a_uni[which(occur>162)])
### When a is 500, length is 196
b<-a_uni[which(occur>162)]
b
length(b)
#Q7
#(a)
match(a,b)

#(b)
#Matrix will be 
c(NA,match(a,b))
col_1<-c(match(a,b),NA,NA)
col_2<-c(NA,match(a,b),NA)
col_3<-c(NA, NA, match(a,b))

mat_T<-cbind(col_1,col_2,col_3)
mat_T2<-mat_T[c(-1,-2,-length(mat_T)+1,-length(mat_T)),]

#(c)
?rowSums#Form row and column sums and means for numeric arrays (or data frames).
RS<-rowSums(mat_T2)
which(is.na(RS))#NA
CWT<-mat_T2[-which(is.na(RS)),]
OWT<-mat_T2[which(is.na(RS)),]

#(d)
l<-length(b)
?array
rawmat<-array(0,c(l,l,l))
dim(rawmat)
for(i in 1:length(CWT[,3])){
  vec<-CWT[i,]
  rawmat[vec[1],vec[2],vec[3]]<-rawmat[vec[1],vec[2],vec[3]]+1
  
}
T<-rawmat
####
mat_A<-cbind(col_1,col_2)
mat_A<-mat_A[c(-1,-length(mat_T[,1])),]
rs_A<-rowSums(mat_A)
mat_A2<-mat_A[which(!is.na(rs_A)),]
length(mat_A2)
rawmatA<-array(0,c(500,500))
for(i in 1:length(mat_A2[,2])){
  vec<-mat_A2[i,]
  rawmatA[vec[1],vec[2]]<-rawmatA[vec[1],vec[2]]+1
  
}
A<-rawmatA

### Produce Matrix S
rawlist<-match(a,b)
rawlist<- rawlist[-which(is.na(rawlist))]

rawmatS<-rep(0,times=500)
for (i in 1:length(rawlist)){
  rawmatS[rawlist[i]]<-rawmatS[rawlist[i]]+1 
  
}
S<- rawmatS

#Question 8
set.seed(0)
colSums(A)
S_b_A<-sample(b,50,prob = colSums(A))
cat(S_b_A,file = "", sep = "\n")

#Question 9 
set.seed(0)
S_b_S<-sample(b,50,prob = S)
cat(S_b_S,file = "", sep = "\n")
dat<-data.frame(S_b_A,S_b_S)
com<-table(dat)
sum(diag(com)/sum(com)) # Comparison : 0.08 which means we get 50*8% = 4 identical words.

#Question 10


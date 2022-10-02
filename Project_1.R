### Project 1


a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a <- a[-grep("[0123456789]:[0123456789]",a)]  ## strip out verse numbers


### Q4
split_punct<-function(text,pun){
  
  for(i in 1:length(pun)){
  remove<-gsub(pun[i],"",text,fixed=TRUE)
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
  
  
### Q6
a<-tolower(a)
a_uni<-unique(a)
match(a,a_uni)
occur<-tabulate(match(a,a_uni))

check<- function(a){
length(which(occur>a))
}

### When a is 175, length is 502
b<-a_uni[which(occur>175)]

### Q7

col_1<-c(match(a,b),NA,NA)
col_2<-c(NA,match(a,b),NA)
col_3<-c(NA, NA, match(a,b))

mat_T<-cbind(col_1,col_2,col_3)

### Remove the first and last two rows
mat_T2<-mat_T[c(-1,-2,-length(mat_T[,1])+1,-length(mat_T[,1])),]

###7c
rs<-rowSums(mat_T2)
mat_T3<-mat_T2[which(!is.na(rs)),]

### 7d
### generate a 3 dimensional vectors with 0's initially
rawmat<-array(0,c(502,502,502))

### Run through the table for each row, and 1 to the corresponding entry of the vector rawmat
for(i in 1:length(mat_T3[,3])){
  vec<-mat_T3[i,]
  rawmat[vec[1],vec[2],vec[3]]<-rawmat[vec[1],vec[2],vec[3]]+1
  
}
T<-rawmat




#### Question 8
###  it seems it should start like this
word<-sample(b,prob=S)[1] ### first word
...
### Then use the first word from that then go to matrix A using the column vector of the word selected in A 
### to pick the second word
### something like sample(b,prob=A[,the cooresponding columnn for the word selected]))...
### Then go to matrix T for the third word, then keep using T until 50 words have been produced



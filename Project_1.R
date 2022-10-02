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
anew<-tolower(a)
a_uni<-unique(anew)
match(anew,a_uni)
occur<-tabulate(match(anew,a_uni))

check<- function(a){
length(which(occur>a))
}

### When a is 175, length is 502
b<-a_uni[which(occur>162)]

### Q7

col_1<-c(match(anew,b),NA,NA)
col_2<-c(NA,match(anew,b),NA)
col_3<-c(NA, NA, match(anew,b))

mat_T<-cbind(col_1,col_2,col_3)

### Remove the first and last two rows
mat_T2<-mat_T[c(-1,-2,-length(mat_T[,1])+1,-length(mat_T[,1])),]

###7c
rs<-rowSums(mat_T2)
mat_T3<-mat_T2[which(!is.na(rs)),]

### 7d
### generate a 3 dimensional vectors with 0's initially
rawmat<-array(0,c(length(b),length(b),length(b)))

### Run through the table for each row, and 1 to the corresponding entry of the vector rawmat
for(i in 1:length(mat_T3[,3])){
  vec<-mat_T3[i,]
  rawmat[vec[1],vec[2],vec[3]]<-rawmat[vec[1],vec[2],vec[3]]+1
  
}
Tmat<-rawmat

### 7f
### Produce Matrix A, S
### Generate the pairs
mat_A<-cbind(col_1,col_2)
mat_A<-mat_A[c(-1,-length(mat_T[,1])),]
rs_A<-rowSums(mat_A)
mat_A2<-mat_A[which(!is.na(rs_A)),]

rawmatA<-array(0,c(length(b),length(b)))
for(i in 1:length(mat_A2[,2])){
  vec<-mat_A2[i,]
  rawmatA[vec[1],vec[2]]<-rawmatA[vec[1],vec[2]]+1
  
}
Amat<-rawmatA

### Produce Matrix S
rawlist<-match(a,b)
rawlist<- rawlist[-which(is.na(rawlist))]

rawmatS<-rep(0,times=length(b))
for (i in 1:length(rawlist)){
 rawmatS[rawlist[i]]<-rawmatS[rawlist[i]]+1 
  
}
Smat<- rawmatS


#### Question 8
set.seed(0)
word<- c()
### pick the first word using S
word[1]<-sample(b,1,prob = Smat)


### Pick the 2nd word using A, the loop will do that automatically as T vector must be 0
### Pick the 3rd-50th words using T if not A and still if not S

for (i in 2:50){
  
  ### Check the possibilities
  probvecT<-Tmat[,which(b==word[i-1]),which(b==word[i-2])]
  
  if(sum(probvecT)>0){
  word[i]<- sample(b,1,prob=probvecT)
  
  } else {
    
    ### This is if the vector sum is 0, we need to move on level down to matrix A
  probvecA<-Amat[,which(b==word[i-1])]
  
  if (sum(probvecA>0)){
    word[i]<-sample(b,1,prob=probvecA)
    
  } else {
      ### If the vector sum is again 0 for A, we need to use S
    word[i]<- sample(b,1,prob=Smat)
    }
}
}
cat(word)


#Question 9 
set.seed(0)
wordinS<-sample(b,50,prob = Smat)
cat(wordinS)

### Question 10

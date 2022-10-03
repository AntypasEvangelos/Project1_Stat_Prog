## This file contains the solution for the first practical project in Statistical Programming.
## Group34 Antypas Evangelos(Vangelis) s2449453, Jihan Li s2322347, Daniel Kwok s2308472

##------------------------------------------------------------------------------------------------------------------------------------------------------------

## Solution to Q1 & Q2
## A private github repository was created by Vangelis who added Daniel and Jihan as contributors and uploaded the pg10.txt
## The repository can be found at "https://github.com/AntypasEvangelos/Project1_Stat_Prog"

##------------------------------------------------------------------------------------------------------------------------------------------------------------

## Solution to Q3
setwd("C:\\Users\\Vaggelis Antypas\\Project1_Stat_Prog")                 ## this is purely indicative of Vangelis's path every member of the group used their own path

## We run the given code to 'clean' the a vector

a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a<- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers

##-----------------------------------------------------------------------------------------------------------------------------------------------------------

## Solution to Q4
## Submission by Daniel, Jihan
## Comments by Vangelis, Jihan

 
## The functionality of this function is to take words that contain punctuation marks, split the word and the punctuation mark 
## and then reinsert the punctuation mark back into the text

split_punct<-function(text,pun){                                                                       
  
  for(i in 1:length(pun)){                                ## the function takes as input text and a vector containing the punctuation marks
    remove<-gsub(pun[i],"",text,fixed=TRUE)               ## we use the gsub function to substitute the punctuation marks with empty strings 
    check<-grep(pun[i],text,fixed=TRUE)                   ## use grep to find the indices of the words containing punctuation marks 
    empty<- rep("",length(text)+length(check))            ## use rep to create a vector into which the words and punctuation marks will be inserted
    sym<- check+1:length(check)							              ## sym containing the indices(locations) of the punctuation marks in new vector
    empty[sym]   <- pun[i]                                ## assign punctuation marks to the elements of new vector empty indexed by sym  
    empty[-sym]<- remove                                  ## words are stored in the elements of new vector empty, means all the elements except those indexed by sym.
    text<-empty                    
  }
  text
} 

## Test example
## The testfile.txt is in our repository and it contains the sentence "The quick brown fox, jumps over the lazy dog!" (punctuation marks added for the test)
## > text<-scan("testfile.txt",what="character")
## Read 9 items
## > text
## [1] "The"   "quick" "brown" "fox,"  "jumps" "over"  "the"   "lazy"  "dog!" 
## > pun<-c(",","!")
## > split_punct(text,pun)
## [1] "The"   "quick" "brown" "fox"   ","     "jumps" "over"  "the"   "lazy" 
## [10] "dog"   "!" 
## It works as intended  

##-----------------------------------------------------------------------------------------------------------------------------------------------------------

## Solution to Q5
## Submission by Daniel
a<-split_punct(a,c(",",".",";","!",":","?"))					   ## We split words and punctuation marks

##------------------------------------------------------------------------------------------------------------------------------------------------------------

## Solution to Q6
## Submission/Comments by Vangelis
 
anew<-tolower(a)
a_uni<-unique(anew)
match(anew,a_uni)
occur<-tabulate(match(anew,a_uni))     ## we find the frequency of every unique word in the text 

check<-function(thres){                ## we are looking for potential thresholds that make the length of roughly 500
	l<-length(which(occur>thres))                                                 
	print(l)
}

for(i in 160:170){                     ## the range (160,170) was chosen based on trial and error experimentation
	check(i)
}


## The results are 505,504,500,498,498,496,495,493,490,486,483
## If we choose thres=162 then length(b)=500
b<-a_uni[which(occur>162)]            ## we take every unique word which occurs more than thres=162 and we create the b vector
b

##---------------------------------------------------------------------------------------------------------------------------------------------------------------

## Solution to Q7
## Submission/Comments by Daniel

col_1<-c(match(anew,b),NA,NA)
col_2<-c(NA,match(anew,b),NA)
col_3<-c(NA, NA, match(anew,b))

mat_T<-cbind(col_1,col_2,col_3)        ## creation of the matrix T

## Remove the first and last two rows
mat_T2<-mat_T[c(-1,-2,-length(mat_T[,1])+1,-length(mat_T[,1])),]

## Q7c
rs<-rowSums(mat_T2)
mat_T3<-mat_T2[which(!is.na(rs)),] #Common Words Triplets

## Q7d
## generate a 3 dimensional vectors with 0's initially

rawmat<-array(0,c(500,500,500))				 ## we have calculated that length(b)=500


## Run through the table for each row, and 1 to the corresponding entry of the vector rawmat

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
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#### Question 8
#Submission by Daniel
#Comments by Jihan
set.seed(0)  #we could reproduce what we got by using set.seed before sample
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
cat(word,sep = '\n')# Print out the corresponding text with cat

#Question 9 
#Submission by Jihan
#Comments by Jihan

set.seed(0) #same as above
wordinS<-sample(b,50,prob = Smat) #word probabilities are simply taken from S
cat(wordinS,sep = '\n')
dat<-data.frame(word,wordinS)#Have a brief comparsion on 50-words section 

### Question 10
#Submission & Comments by Daniel
#Test by Jihan
### Use the vector in b
bnew<-c()
for (i in 1:500){
  
  ### We first extract the words that match the orginial vector b regardless of case
  trycase<-match(anew,b[i])
  
  ### We see the variations of that particular word in vector a (with different cases)
  vartable<-table(a[which(trycase==1)])
  
  ### Find the corresponding entry of the max variation 
  common<-which(vartable==max(vartable))
  
  ### We insert the new "version" of that word
  bnew[i]<-names(common)
}

#### TEST
set.seed(0)
wordinS_1<-sample(bnew,50,prob = Smat)
cat(wordinS_1,sep = '\n')
data<-data.frame(wordinS,wordinS_1)
sum(diag(table(data))/sum(table(data)))# 1, which means we make words that most often start with a capital letter in the main text, also start with a capital letter in your simulation,
#but we achieve this in a way that does not mess up the word frequencies.

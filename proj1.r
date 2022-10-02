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
## Submission by Daniel
## Comments by Vangelis

 
## The functionality of this function is to take words that contain punctuation marks, split the word and the punctuation mark 
## and then reinsert the punctuation mark back into the text

split_punct<-function(text,pun){                                                                       
  
  for(i in 1:length(pun)){                                              ## the function takes as input text and a vector containing the punctuation marks
    remove<-gsub(pun[i],"",text,fixed=TRUE)                             ## we use the gsub function to substitute the punctuation marks with empty strings 
    check<-grep(pun[i],text,fixed=TRUE)                                 ## and create a new vector containing no punctuation marks 
    empty<- rep("",length(text)+length(check))                          ## we use the grep function to detect punctuation marks in the original vector
    sym<- check+1:length(check)							## we create an augmented vector with length equal to the total number of words 
    empty[sym]   <- pun[i]                                              ## words + total number of punctuation characters 
    empty[-sym]<- remove                                                ## we fill the new vector with elements
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
 
a<-tolower(a)                                                              ## we turn every letter into lowercase
a_unique<-unique(a)                                                        ## we check for every unique word in the text 
oc<-tabulate(match(a,a_unique))                                            ## we find the frequency of every unique word in the text 

check<-function(thres){                                                    ## we are looking for potential thresholds that make the length of roughly 500
	l<-length(which(oc>thres))                                                 
	print(l)
}

for(i in 160:170){                                                         ## the range (160,170) was chosen based on trial and error experimentation
	check(i)
}


## The results are 505,504,500,498,498,496,495,493,490,486,483
## If we choose thres=162 then length(b)=500
b<-a_unique[which(oc>162)]                                                 ## we take every unique word which occurs more than thres=162 and we create the b vector
b

##---------------------------------------------------------------------------------------------------------------------------------------------------------------

## Solution to Q7
## Submission/Comments by Daniel

col_1<-c(match(a,b),NA,NA)
col_2<-c(NA,match(a,b),NA)
col_3<-c(NA, NA, match(a,b))

mat_T<-cbind(col_1,col_2,col_3)                                           ## creation of the matrix T

## Remove the first and last two rows
mat_T2<-mat_T[c(-1,-2,-length(mat_T[,1])+1,-length(mat_T[,1])),]

## Q7c
rs<-rowSums(mat_T2)
mat_T3<-mat_T2[which(!is.na(rs)),]

## Q7d
## generate a 3 dimensional vectors with 0's initially

rawmat<-array(0,c(500,500,500))				                   ## we have calculated that length(b)=500


## Run through the table for each row, and 1 to the corresponding entry of the vector rawmat

for(i in 1:length(mat_T3[,3])){
  vec<-mat_T3[i,]
  rawmat[vec[1],vec[2],vec[3]]<-rawmat[vec[1],vec[2],vec[3]]+1
  
}
T<-rawmat

## Q7f
## we produce matrices A, S
## we generate the pairs


mat_A<-cbind(col_1,col_2)
mat_A<-mat_A[c(-1,-length(mat_T[,1])),]
rs_A<-rowSums(mat_A)
mat_A2<-mat_A[which(!is.na(rs_A)),]

rawmatA<-array(0,c(502,502))
for(i in 1:length(mat_A2[,2])){
  vec<-mat_A2[i,]
  rawmatA[vec[1],vec[2]]<-rawmatA[vec[1],vec[2]]+1
  
}

A<-rawmatA

## we produce Matrix S
rawlist<-match(a,b)
rawlist<- rawlist[-which(is.na(rawlist))]

rawmatS<-rep(0,times=502)

for (i in 1:length(rawlist)){
 rawmatS[rawlist[i]]<-rawmatS[rawlist[i]]+1 
  
}
S<- rawmatS
##----------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Solution to Q8
## Submission/Comments by Vangelis 

word<-sample(b,prob=S)[1]								## sampling the first word  
ii<-match(word,mat_T)

## something trivial for Jihan Li

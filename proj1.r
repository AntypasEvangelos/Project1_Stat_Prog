## This file contains the solution for the first practical project in Statistical Programming.
## Group34 Antypas Evangelos(Vangelis) s2449453, Jihan Li s2322347, Daniel Kwok s2308472

##------------------------------------------------------------------------------------------------------------------------------------------------------------

## For the sake of keeping a record, we have reported who submitted the solution/ commented in every question.
## However the submissions are not a product of individual work because we worked by getting together and talking about the questions and 
## this just cannot be tracked, so to avoid writing each and every one of our names in every question we decided to follow the convention mentioned above.
## As far as we are concerned the distribution of work was as uniform as possible.

##------------------------------------------------------------------------------------------------------------------------------------------------------------

## Solution to Q1 & Q2
## A private github repository was created by Vangelis who added Daniel and Jihan as contributors and uploaded the pg10.txt
## The repository can be found at "https://github.com/AntypasEvangelos/Project1_Stat_Prog"

##------------------------------------------------------------------------------------------------------------------------------------------------------------

## Solution to Q3
## Submission by Vangelis
## Comments by Vangelis 

## This is purely indicative of Vangelis's path every member of the group used their own path
setwd("C:\\Users\\Vaggelis Antypas\\Project1_Stat_Prog")                

## We run the given code to 'clean' the a vector

a <- scan("pg10.txt",what="character",skip=104) ## Skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## Strip license
a<- a[-grep("[0123456789]:[0123456789]",a)] ## Strip out verse numbers

##-----------------------------------------------------------------------------------------------------------------------------------------------------------

## Solution to Q4
## Submission by Daniel &  Jihan
## Comments by Vangelis & Jihan

 
## The functionality of this function is to take words that contain punctuation marks, split the word and the punctuation mark 
## and then reinsert the punctuation mark back into the text

split_punct<-function(text,pun){                                                                       
  
  ## The function takes as input text and a vector containing the punctuation marks
  for(i in 1:length(pun)){                                
    ## We use the gsub function to substitute the punctuation marks with empty strings 
    remove<-gsub(pun[i],"",text,fixed=TRUE)
    
    ## Use grep to find the indices of the words containing punctuation marks 
    check<-grep(pun[i],text,fixed=TRUE)                   
    
    ## Use rep to create a vector into which the words and punctuation marks will be inserted
    empty<- rep("",length(text)+length(check))          
    
    ## "sym" containing the indices(locations) of the punctuation marks in new vector
    sym<- check+1:length(check)				    
    
    ## Assign punctuation marks to the elements of new vector empty indexed by sym  
    empty[sym]<- pun[i]        
    
    ## Words are stored in the elements of new vector empty, means all the elements except those indexed by sym.
    empty[-sym]<- remove                                 
    text<-empty                    
  }
  text
} 


##-----------------------------------------------------------------------------------------------------------------------------------------------------------

## Solution to Q5
## Submission by Daniel
## Comment by Vangelis
a<-split_punct(a,c(",",".",";","!",":","?"))					   ## We split words and punctuation marks

##------------------------------------------------------------------------------------------------------------------------------------------------------------

## Solution to Q6
## Submission & Comments by Vangelis
 
## Convert all letters to lower case
anew<-tolower(a)

## Extract all unique words and punctuation marks
a_uni<-unique(anew)

## Find the frequency of every unique word in the text
occur<-tabulate(match(anew,a_uni))    

## We are looking for potential thresholds that make the length of roughly 500
check<-function(thres){                
	l<-length(which(occur>thres))                                                 
	print(l)
}

## The range (160,170) was chosen based on trial and error experimentation
for(i in 160:170){                    
	check(i)
}


## If we choose thres=162 then length(b)=500
## We take every unique word which occurs more than thres=162 and we create the b vector
b<-a_uni[which(occur>162)]           

##---------------------------------------------------------------------------------------------------------------------------------------------------------------

## Solution to Q7
## Submission & Comments by Daniel

## Here 3 columns have been created for col 2 and 3 we move down 1 and 2 rows
## so that each row has 3 entries, forming a common words triplets from right to left 
col_1<-c(match(anew,b),NA,NA)
col_2<-c(NA,match(anew,b),NA)
col_3<-c(NA, NA, match(anew,b))

## Creation of the matrix T
mat_T<-cbind(col_1,col_2,col_3)     

## Remove the first and last two rows respectively
mat_T2<-mat_T[c(-1,-2,-length(mat_T[,1])+1,-length(mat_T[,1])),]

## Q7c

## Calculate the row sum and common triplets are those if it is NOT zero
## Extract Common Words Triplets
rs<-rowSums(mat_T2)
mat_T3<-mat_T2[which(!is.na(rs)),] 

## Q7d
## Generate a 3 dimensional vectors with 0's initially

rawmat<-array(0,c(500,500,500))				 ## We have calculated that length(b)=500


## Run through the table for each row, add 1 to the corresponding entry of the vector rawmat

for(i in 1:length(mat_T3[,3])){
  vec<-mat_T3[i,]
  rawmat[vec[1],vec[2],vec[3]]<-rawmat[vec[1],vec[2],vec[3]]+1
  
}
Tmat<-rawmat

## Q7f
## Produce Matrix A using the same principle
## Generate the pairs

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

## Produce Matrix S using the same principle
rawlist<-match(anew,b)
rawlist<- rawlist[-which(is.na(rawlist))]

rawmatS<-rep(0,times=length(b))
for (i in 1:length(rawlist)){
  rawmatS[rawlist[i]]<-rawmatS[rawlist[i]]+1 
  
}
Smat<- rawmatS

##----------------------------------------------------------------------------------------------------------------------------------------------------------------------

## Solution to Q8
## Submission by Daniel & Vangelis
## Comments by Jihan & Vangelis 

set.seed(0)  ## We could reproduce what we got by using set.seed before sample
word<- c()  

## Pick the first word using S
word[1]<-sample(b,1,prob = Smat)


## Pick the second word using A, the loop will do that automatically as the T vector must be 0
## Pick the 3rd-50th words using T if not A and still if not S

for (i in 2:50){
  
  ## Check the corresponding probability vector in T
  probvecT<-Tmat[,which(b==word[i-1]),which(b==word[i-2])]
  
  if(sum(probvecT)>0){
    
    ## We sample the ith word using the found probability vector
    word[i]<- sample(b,1,prob=probvecT)
  
## This is if the vector sum is 0, we need to move on down a level to matrix A  
  } else {
    
    ## Same principle as T
    probvecA<-Amat[,which(b==word[i-1])]
    
    if (sum(probvecA>0)){
      word[i]<-sample(b,1,prob=probvecA)
      
    } else {
      ## If the vector sum is again 0 for A, we need to use S
      word[i]<- sample(b,1,prob=Smat)
    }
  }
}
## Print out the corresponding text with cat
cat(word,sep = '\n')  

##---------------------------------------------------------------------------------------------------------------------------------------------------------------

## Solution to Q9 
## Submission by Jihan
## Comments by Jihan & Vangelis

set.seed(0) ## Same as above
wordinS<-sample(b,50,prob = Smat) ## Word probabilities are simply taken from S
cat(wordinS,sep = '\n')
dat<-data.frame(word,wordinS) ## Have a brief comparison on 50-words section 

##---------------------------------------------------------------------------------------------------------------------------------------------------------------

## Solution to Q10
## Submission & Comments by Daniel
## Test by Jihan
## Create a new vector bnew
bnew<-c()

for (i in 1:500){
  
  ## We first extract the words that match the original vector b regardless of the case
  trycase<-match(anew,b[i])
  
  ## We see the variations of that particular word in vector a (original text)
  vartable<-table(a[which(trycase==1)])
  
  ## Find the variation of that word with 
  common<-which(vartable==max(vartable))
  
  ## We insert the new "version" of that word
  bnew[i]<-names(common)
}

## Compare b and bnew
bcomp<-data.frame(b,bnew)

##---------------------------------------------------------------------------------------------------------------------------------------------------------------

## End

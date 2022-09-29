# This file contains the solution for the first practical project in Statistical Programming.
# Group34 Antypas Evangelos s2449453, Jihan Li s2322347, Daniel Kwok s2308472

# Solution to Q3
setwd("C:\\Users\\Vaggelis Antypas\\Project1_Stat_Prog")

# We prepare the vector a, which contains all of the words in the text
a <- scan("pg10.txt",what="character",skip=104) ## skip contents
n <- length(a)
a <- a[-((n-2886):n)] ## strip license
a<- a[-grep("[0123456789]:[0123456789]",a)] ## strip out verse numbers

# Solution to Q4
# Submission by Daniel

split_punct<-function(text,pun){
  for(i in 1:length(pun)){remove<-gsub(pun[i],"", text, fixed=TRUE)
  check<- grep(pun[i],text,fixed=TRUE)
  empty<- rep("",length(text)+length(check))
  empty[sym]<-pun[i]
  empty[-sym]<- remove
  text<-empty
	}
  text
}

# Solution to Q5
a<-split_punct(a,c(",",".",";","!",":","?"))

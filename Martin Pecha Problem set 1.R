#PROBLEM 1
#print actual time
now = Sys.time()
now

#create a list ”bad list” with 3 positions: ’na’ with values ’NA’ 3 times, ’chars’ with values
#’a’,’b’,’c’, ’bools’ with values FALSE 2 times and TRUE once

na=c(NA,NA,NA)
chars=c(a,b,c)
bools=c(FALSE,FALSE,TRUE)

bad_list=list(na,chars,bools)

#create a data frame ’happyframe’ with similar specification as in previous points: 3 columns
#’na’, ’chars’, ’bools’; 3 rows
happyframe=data.frame(na,chars,bools)

#PROBLEM 2
#write the commands that will calculate the length of numeric vector ’input’ using arithmetic
#operators and functions only (without using len function)
x=0
p=c(1,3,4,5)
findlen <- function(p) {
  if (x!=0){
    print("You have wrong starting value.")
  }
  else {
  for (i in p) {
  x=x+1
  }
    print(paste("Length of vector is",x,"."))  
  }
}

findlen(p)

#write the commands that will output boolean vector with TRUE for every 3 3rd position of
#the vector and FALSE for every other position using arithmetic & comparison operators

p=c(FALSE,TRUE,FALSE,FALSE,FALSE,FALSE,FALSE) #define the vector
boolfun <- function(p) {
  x=0
  for (i in p) {
    x=x+1
    if (x%%3==0){
      p[x]=TRUE
    }
    else {  
    p[x]=FALSE
    }
  }
  return(p)
}

boolfun(p)

#given the following input, return the boolean vector indicating whether the fruit is
#1. red fruit larger than 5
#2. non-red fruit smaller than 5

#note - there was no input - so I assuming, it will come in form of list
weight=c(3,1,6,7)
color=c("blue","red","yellow","red")
fruit=data.frame(weight=weight,color=color)

investigate <- function(fruit){
  value=c()
  for (i in 1:length(fruit$weight)){
    if (fruit$weight[i]>5 & fruit$color[i]=="red"){
      value=append(value,TRUE)
    }
    else if (fruit$weight[i]<5 & fruit$color[i]!="red"){
      value=append(value,TRUE)
    }
    else{
      value=append(value,FALSE)
    }
  }
 return(value)   
}

investigate(fruit)

#load mtcars dataset (to do that simply write df = mtcars)
df=mtcars
glimpse(df)
head(df)
#slice rows where columns carb is less than or equal to 2
#choose rows 3,8,12
df %>%
filter(carb<=2)%>%
  slice(3,8,12)%>%
  summarise(sumdisp=sum(disp))

#create a list with the following elements:
#1. slice the fifth row of the third column from data.frame (using [] notation)
#2. slice the eight position of the drat column (using $ notations)         
data1=df[5,3]
data1
data2=df$drat[8]
data2



data(mtcars)
View(mtcars)

mtcars$am <- as.factor(mtcars$am)

###Phase I: Identify the problem
#Hocking [original transcriber]'s noncrucial 
#coding of the Mazda's rotary engine as a straight six-cylinder engine 
#and the Porsche's flat engine as a V engine, as well as 
#the inclusion of the diesel Mercedes 240D, have been retained 
#to enable direct comparisons to be made with previous analyses.’

###phase II:MAKE ASSUMPTIONS AND DEFINE ESSENTIAL VARIABLES
#Four basic assumptions
#All the important factors are included in the study.
#Depenent Varialbe: mpg
#Independent Variable: Other variables in the dataset.
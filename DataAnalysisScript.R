
resp = read.csv("Assessment.csv", header = T, sep = ",",
                stringsAsFactors = T)

print(resp)
class(resp)

COL3 = brewer.pal(n=3, name = "Set3")
COL4 = brewer.pal(n=4, name = "Set3")
COL5 = brewer.pal(n=5, name = "Set3")
COL6 = brewer.pal(n=6, name = "Set3")
COL7 = brewer.pal(n=7, name = "Set3")

# max: This method will return the maximum value within the column
# min: This method will return the minimum value within the column
# subset(data, condition): This method will return the subset of data, 
# and the data depends on the condition
# example:

femalesWhoArefirstyear = subset( x= resp, Gender == "Female" & Year == "1st Year")
dim(femalesWhoArefirstyear)

#useful functions:
#head (data, limit), tail (data, limit)
#structur = str, summary 
print(str(resp))
print (summary(resp))


# when you are examining a column of the dataset, if you're trying to plot that column,
# what R would do is plot each record of it, so you need to transform it into a table 
# to get some kind of frequency distribution

#PIE CHAT: PLOTTING MALE VS FEMALE ###################################################
x <-  table(resp$Gender)
library("RColorBrewer")

piepercent<- round(100*x/sum(x), 1)

library( plotly)
# Plot the chart.
png ("Gender.png")

pie(x, labels = piepercent, main = "Gender of respondents",
    col = COL3, radius =  0.9)
legend("topright", c("Female students" , "Male students"), cex = 0.8,
       fill = COL3 )
d <- data.frame(resp$Gender)


dev.off()

# Year PIE CHART ^^^############################################################
library("RColorBrewer")
x <-  table(resp$Year)
col 

piepercent<- round(100*x/sum(x), 1)

# Plot the chart.
pie(x, labels = piepercent, main = "Year of respondents",
    col=COL5)
legend("topright", c("1st Year", "2nd Year", "3rd Year", "4th Year" , "Post Graduate"),
       cex = 0.7,
       fill = COL5)

## STEAM AND LEAF : CGPA , credits#############################################################

stem(resp$CGPA, scale = 0.5, atom = 2)
stem(resp$Credits)


##Histogram of Credits#####################################################################
?hist

library("RColorBrewer")
hist(resp$Credits, main = "Number of credits taken by respondents",
     col = COL6,breaks = 10,
     xlab = "Number of Credits")


#Histogram: PLOTTING MONEY SPENT ON TRASPORT###########################################


library("RColorBrewer")
hist(resp$TransportMoney, main = "Average money spent daily to get to class",
     col = brewer.pal(n = 3, name = "Blues"),breaks = 5,
     xlab = "Amount (RM)", ylim = c(0,160))
abline(h=150)



#BAR CHART, TRANSPORT VS ABSENCE AT 8 AM#############################################


library("lattice")
library("RColorBrewer")

byBus = subset(resp, resp$Transport == "Bus")
byBus = table (byBus$Skip8to10)
byCar = subset(resp, resp$Transport == "Car")
byCar = table (byCar$Skip8to10)
byBike = subset(resp, resp$Transport == "Bike")
byBike = table (byBike$Skip8to10)
byWalking= subset(resp, resp$Transport == "Walking")
length(byWalking)
byWalking= table (byWalking$Skip8to10)



byWalking
byBike
byBus
byCar
x= matrix (c(byWalking, byBike,byBus, byCar), byrow = FALSE, nrow = 6)


labels = c ("Walking","Bike", "Bus", "Car")
legendLabels = c( "1 time",  "2 times",  "3 times",  "4 times", "5+ times ",   "Never" )

barplot(x, beside =  TRUE, names.arg = labels, 
        col = brewer.pal(n = 6, name = "Dark2"))

legend("topright" ,  legend = legendLabels, 
       fill = brewer.pal(n = 6, name = "Dark2"),
       box.lty = 6, cex = 0.9)

#BAR CHAR: PLOTTING TIME TAKEN TO GO TO CLASS ##########################################

timeToClass = table (resp$TransportTime)
timeToClass
?barplot
barplot(timeToClass, main ="Time spent to get to classes (minutes)",
        xlab = "Duration", ylab ="Frequency", ylim = c(0,60),
        col = c ("#0652DD"), border = "#0652DD" )

#HISTOGRAM: PLOTTING SLEEP ###########################################
sleepHours = (resp$Sleep)

hist(sleepHours, breaks = 5, col = COL7, 
     main = "Hours of sleep of respondents", xlab =  "Hours")


#BAR CHART: HOW OFTEN DO U SKIP CLASSES ##################################
lev = c("Never", "1 time", "2 times", "3 times", "4 times", "5+ times")


skip8to10 = table(factor(x=resp$Skip8to10, levels =lev ))
skip10to12 = table(factor(x=resp$Skip10to12, levels =lev ))
skip12to1 = table(factor(x=resp$Skip12to1, levels =lev ))
skip2to4 = table(factor(x=resp$Skip2to4, levels =lev ))
skip2to5 = table(factor(x=resp$Skip2to5, levels =lev ))
skip4to6 = table(factor(x=resp$Skip4to6, levels =lev ))


skip10to12
skip8to10
skip12to1
skip2to4
skip2to5
skip4to6

allSkips = matrix(c(skip8to10,skip10to12,
                    skip12to1,skip2to4,skip2to5,skip4to6),
                  byrow = T, nrow = 6)
skipLabels = c("Never", "1 time", "2 times", "3 times", "4 times", "5+ times")
skipLegend = c("8 - 10 am", "10 - 12 pm", "12 - 1 pm", "2 - 4 pm",
               "2 - 5 pm", "4 - 6 pm")
skipColors = rainbow(6)
?barplot()
barplot(allSkips, main = "Expected number of absences", beside = T, col=skipColors,
        names.arg = skipLabels, density= 100, cex.axis = 1,
        ylim = c(0,200))

legend("topright",fill= skipColors, density=100, skipLegend, box.lty = 3, cex = 0.9)

#BAR CHART: CONCENTRATION LEVELS ##################################
lev = c("100%","75%","50%","25%" ,"0%"  )

conc8to10 = table(factor(x=resp$Concentration8to10, levels =lev ))
conc10to12 = table(factor(x=resp$Concentration10to12, levels =lev ))
conc12to1 = table(factor(x=resp$Concentration12to1, levels =lev ))
conc2to4 = table(factor(x=resp$Concentration2to4, levels =lev ))
conc2to5 = table(factor(x=resp$Concentration2to5, levels =lev ))
conc4to6 = table(factor(x=resp$Concentration4to6, levels =lev ))

conc8to10
conc10to12
conc12to1
conc2to4
conc2to5
conc4to6

allConcs = matrix(c(conc8to10,conc10to12,
                    conc12to1,conc2to4,conc2to5,conc4to6),
                  byrow = F, nrow = 5)

concLabels = c("8 - 10 am", "10 - 12 pm", "12 - 1 pm", "2 - 4 pm",
               "2 - 5 pm", "4 - 6 pm")
concLegend = lev
concColors = rainbow(5)
?barplot()
barplot(allConcs, main = "Concentration Levels", beside = T, 
        col=concColors, names.arg = concLabels, density= 100, cex.axis = 1,
        ylim = c(0,200))

legend("topright",fill= concColors, density=100, concLegend, box.lty = 3, cex = 0.8)


#BOX PLOT: CGPA###############################

boxplot(resp$CGPA , data = resp, ylab =  "CGPA")

#BAR CHAR: SATISFACTION WITH SCHEDUELE ###########################################
satisfaction= table (resp$Satisfaction)
satisfaction
barplot(satisfaction, col= COL6, main = "Satisfaction level with classes scheduele",
        names.arg = c("0 = Very unsatisfied", 1, 2 ,3 ,4 , "5 = Very satisfied"))

#BAR CHART: HOURS OFSLEEP ####################################################
hoursOfSleep = table(resp$Sleep)
hoursOfSleep
barplot(hoursOfSleep)


# HOURS OF START PREFERANCE #################################################
# I had to use a regex library to detect in string

library(stringi)
preferat8= length((subset(x=resp$StartPreferance, stri_detect(resp$StartPreferance, regex = "8") )))


preferat9= length((subset(x=resp$StartPreferance, stri_detect(resp$StartPreferance, regex = "9") )))

preferat10= length((subset(x=resp$StartPreferance, stri_detect(resp$StartPreferance, regex = "10") )))


preferat11= length((subset(x=resp$StartPreferance,stri_detect(resp$StartPreferance, regex = "11") )))

barplot(c(preferat11,preferat10,preferat9,preferat8), names.arg = c("11 am", "10 am", "9 am", "8 am"), horiz = T, xlim =c(0,100))


# HOURS OF END PREFERANCE #################################################
# I had to use a regex library to detect in string

library(stringi)
preferat1= length((subset(x=resp$EndPreferance, stri_detect(resp$EndPreferance, regex = "1") )))

preferat3= length((subset(x=resp$EndPreferance, stri_detect(resp$EndPreferance, regex = "3") )))

preferat4= length((subset(x=resp$EndPreferance, stri_detect(resp$EndPreferance, regex = "4") )))


preferat5= length((subset(x=resp$EndPreferance,stri_detect(resp$EndPreferance, regex = "5") )))
preferat6= length((subset(x=resp$EndPreferance,stri_detect(resp$EndPreferance, regex = "6") )))

barplot(c(preferat6, preferat5,preferat4,preferat3,preferat1), 
        names.arg = c("6 pm","5 pm", "4 pm", "3 pm", "1 pm"), horiz = T, xlim =c(0,120))


#BAR CHAR:EXPECTED ABSENCE IN MALE AND FEMALE ###########################################

#BAR CHART: PLOTTING LEVEL OF CONECTRATION AT 8 AM VERSUS SLEEP HOURS###################

lev = c("100%","75%","50%","25%" ,"0%"  )
library("RColorBrewer")


sixOrMore = subset(x=resp, resp$Sleep >=6 )
sixOrMoreLevel = (sixOrMore$Concentration8to10)
sixOrMoreLevel = table (factor (sixOrMoreLevel, levels = lev))
sixOrMoreLevel

lessThanSix= subset(x=resp, resp$Sleep <6 )
lessThanSixLevel = (lessThanSix$Concentration8to10)
lessThanSixLevel = table (factor (lessThanSixLevel, levels = lev))
lessThanSixLevel


x= matrix (c (sixOrMoreLevel,lessThanSixLevel), byrow = F, nrow =5)


barplot( x, beside =  T, main = "Hours of sleep vs Concentration level at 8 am class",
         col = brewer.pal(n = 5, name = "Dark2"),
         ylab = "Frequency",
         names.arg = c(" >= 6 hours", " < 6 hours"), 
                       ylim= c(0,45)
                       )
         
?legend
legend("top",fill= brewer.pal(n = 5, name = "Dark2"), 
       lev, box.lty = 3, cex = 0.7, horiz = T)



# do not use these: 
barplot(lessThanSixLevel, main = "sleep hours < 6 and Concentration levels at 8am class")
barplot(sixOrMoreLevel, main = "sleep hours > 6,  Concentration levels at 8am class")


resp = read.csv("Assessment.csv", header = T, sep = ",",
                stringsAsFactors = T)

print(resp)
class(resp)

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

piepercent<- round(100*x/sum(x), 1)

# Plot the chart.
pie(x, labels = piepercent, main = "Gender of respondents",col = rainbow(length(x)))
legend("topright", c("Female students" , "Male students"), cex = 1,
       fill = rainbow(length(x)))


#BAR CHART: PLOTTING MONEY SPENT ON TRASPORT###########################################
moneySpent = table(resp$TransportMoney)
moneySpent
barplot(moneySpent,main = "Amount paid to get to classes (RM)")

#BAR CHAR: PLOTTING TIME TAKEN TO GO TO CLASS ##########################################

timeToClass = table (resp$TransportTime)
timeToClass
?barplot
barplot(timeToClass, main ="Time spent to get to classes (minutes)",
        xlab = "Duration", ylab ="Frequency", ylim = c(0,60),
        col = c ("#0652DD"), border = "#0652DD" )

#BAR CHAR: PLOTTING SLEEP ###########################################
sleepHours = table (resp$Sleep)
sleepHours
barplot(sleepHours)


#BAR CHAR: PREFERANCE FOR START ###########################################
preferStart= table (resp$StartPreferance)
preferStart
barplot(preferStart, horiz =T)

#BAR CHART: HOW OFTEN DO U SKIP CLASSES ##################################
lev = c("Never", "1 time", "2 times", "3 times", "4 times", "5+ times")

typeof(resp$Skip10to12)


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
        names.arg = skipLabels, density= 75, cex.axis = 1,
        ylim = c(0,200))

legend("topright",fill= skipColors, density=75, skipLegend, box.lty = 3, cex = 0.9)

#BAR CHART: CONCENTRATION LEVELS ##################################
lev = c("100%","75%","50%","25%" ,"0%"  )


oo = table(factor(x=resp$Concentration8to10, levels = lev))
oo 
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


#BOX PLOT: Year vs satisfaction level ###############################

boxplot(resp$Skip8to10 ~ resp$Transport, data = resp,
        xlab = "Year", ylab =  "Satisfaction Level")

#BAR CHAR: SATISFACTION WITH SCHEDUELE ###########################################
satisfaction= table (resp$Satisfaction)
satisfaction
barplot(satisfaction)

#BAR CHART: HOURS OFSLEEP ####################################################
hoursOfSleep = table(resp$Sleep)
hoursOfSleep
barplot(hoursOfSleep)


# HOURS OF START PREFERANCE #################################################
# I had to use a regex library to detect in strings!!!

library(string)
preferat8= length((subset(x=resp$StartPreferance, 
                  resp$StartPreferance == "8:00 AM" | stri_detect(resp$StartPreferance, regex = "8 am") )))


preferat9= length((subset(x=resp$StartPreferance, 
                  resp$StartPreferance == "9:00 AM" | stri_detect(resp$StartPreferance, regex = "9 am") )))

preferat10= length((subset(x=resp$StartPreferance, 
                          resp$StartPreferance == "10:00 AM" | stri_detect(resp$StartPreferance, regex = "10 am") )))


preferat11= length((subset(x=resp$StartPreferance, 
                          resp$StartPreferance == "11:00 AM" | stri_detect(resp$StartPreferance, regex = "11 am") )))
class(preferat8)
class(preferat9)

barplot(c(preferat11,preferat10,preferat9,preferat8), names.arg = c("11 am", "10 am", "9 am", "8 am"), horiz = T, xlim =c(0,100))


#BAR CHAR:EXPECTED ABSENCE IN MALE AND FEMALE ###########################################

#BAR CHART: PLOTTING LEVEL OF CONECTRATION AT 8 AM VERSUS SLEEP HOURS###################

sixOrMore = subset(x=resp, resp$Sleep >=6 )
lessThanSix= subset(x=resp, resp$Sleep <6 )
sixOrMoreLevel = table(sixOrMore$Concentration8to10)
lessThanSixLevel = table(lessThanSix$Concentration8to10)
sixOrMoreLevel
lessThanSixLevel
barplot(lessThanSixLevel, main = "sleep hours < 6 and Concentration levels at 8am class")
barplot(sixOrMoreLevel, main = "sleep hours > 6,  Concentration levels at 8am class")

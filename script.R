COVID19_line_list_data <- read.csv("E:/desktop/covid project/COVID19_line_list_data.csv")

rm(list=ls())

data <- read.csv("E:/desktop/covid project/COVID19_line_list_data.csv")
describe(data)

# clean death column
data$death_dummy <- as.integer(data$death != 0)

# find out the death rate
sum(data$death_dummy) / nrow(data)

# claim: people died are older
dead <- subset(data,death_dummy == 1)
alive <- subset(data,death_dummy == 0)
mean(dead$age,na.rm = TRUE)
mean(alive$age,na.rm = TRUE)
# check statistically significant
t.test(alive$age, dead$age, alternative="two.sided",conf.level = 0.99)

# normally, if p-value < 0.05, we reject null hypothesis and here p-value ~ 0
# so we reject null hypothesis and conclude this is statistically significant
# 99 percent confidence: the people who alive are 15.51 to 25.52 years younger
# data  visualizing
ages <- c(alive$age, dead$age)
group <- c(rep("Alive", nrow(alive)), rep("Dead", nrow(dead)))
boxplot(ages ~ group, 
        main="Age Distribution of Alive vs. Dead Individuals",
        xlab="Status",
        ylab="Age",
        col=c("green", "red"),
        notch=TRUE) # notch displays a notch for median, which if non-overlapping, indicates a significant difference at roughly the 95% confidence level




#claim: gender has no effect
men <- subset(data,gender=="male")
women <-subset(data,gender =="female")
prop_men <- mean(men$death_dummy, na.rm = TRUE)
prop_women <- mean(women$death_dummy, na.rm = TRUE)
t.test(men$death_dummy,women$death_dummy,alternative = "two.sided",conf.level = 0.99)
# 99 percent confidence: men is 0.78% to 8.8% higher chance of dying.
# p-value = 0.002 < 0.05, so this is statistically significant.


# data visualizing
barplot(c(prop_men, prop_women), 
        names.arg=c("Men", "Women"), 
        ylim=c(0,1), 
        main="Death Proportions by Gender", 
        ylab="Proportion Dead",
        col=c("blue", "pink"))
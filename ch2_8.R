setwd('~/Documents/R/ISLR')
college = read.csv(file = 'College.csv')
# fix(college)

## remove the data column containing row names, but keeping the names
rownames(college)=college[,1]
college = college[,-1]
# fix(college)

#summary(college)

## matrix of scatter plots of first 10 columns
# pairs(college[,1:10])
# plot(college$Private, college$Outstate)

## create and add a new qualitative variable 'Elite' based on Top10prec > 50 %
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
#summary(college)
#plot(college$Elite, college$Outstate, xlab="Elite", ylab="Outstate")


par(mfrow = c(2,2))
hist(college$PhD, breaks = 40, xlab="Nr. of PhD Students")
hist(college$Terminal, breaks = 30, xlab = "Nr. Terminals")
hist(college$Personal, breaks = 20, xlab = "Nr. of Personal")
hist(college$Top10perc, breaks = 10, xlab = "Top 10% Students Admitted")



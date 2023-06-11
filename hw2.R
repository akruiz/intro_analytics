
## Q4.1
# It would be interesting to to use clustering for image analysis. For fun and trial, the use to go towards organizing your closet: 
# by having images of all your garments and extracting features and then cluster them into similarity. For example:
# My k would be the number of cluster would be 10 so I can consider all the types of clothing in my closet. Predictors: pants, dresses, shirts, shoes and jackets etc..


## Q4.2
# clear
rm(list = ls())
# read data & name variable df
df <- read.table("iris.txt", header=TRUE)
head(df)
#count label in each column
table(df$Species)

# install packages to be used
install.packages('factoextra',repos='http://cran.us.r-project.org')
#install.packages('ggplot2',repos='http://cran.us.r-project.org')


#plot data to visualize
library(ggplot2)
ggplot(df, aes(Sepal.Length, Sepal.Width, color=Species)) + geom_point()
summary(df)

#scale data & we omit the species label for this model
scaled.data <- scale(df[1:4] )

# use kmeans to cluster point, from plot we can see there is about 2-4 clusters so we will attempt both and see which one is better
# per online documentation, 10 or more is recommended, to get stable results we will use 25 for all.
c1 <- kmeans(df[,1:4], centers=2, nstart=25)
c2 <- kmeans(df[,1:4], centers=3, nstart=25)
c3 <- kmeans(df[,1:4], centers=4, nstart=25)

# compare clusters
table(c1$cluster, df$Species)
table(c2$cluster, df$Species)
table(c3$cluster, df$Species)

# even though we already have the species label, the table above confirms that K = 3 is the best way to cluster our data
# since at 3 the labels were divided the best compared to 2 or 4.

# compare accuracy of model k=3, we will add the ones that were properly labeled and get the correct %.
c3_acc <-(50+48+36)/150
c3_acc
# 89% is the accuracy of this cluster model.



# Q5.1
# clear
rm(list = ls())
# read data & name variable df
df <- read.table("uscrime.txt", header=TRUE)
col <- df[,"Crime"]

# import packages
library(outliers)

# visualize crime data
plot(df$Crime)

# check for outliers
grubbs.test(df$Crime)

# grubbs function tells us its 1993 the outlier

# Q6.1
# At my Diagnostics jobs we work a lot with analytics. In order to detect the malfunctioning of most sensors on gas turbines we use the cusum model
# The oscillation of most sensors can be triggered by operating parameters, ambien conditions, site activities etc. So a good way to detect change is using this model
# however the anomaly identified depends on the parameter. Usually a change in exhaust temperatures thermocouple are determined in a 3 hour window past average threshold because there are other alerts that determine a fault in the sensor. (just an example)


# Q6.2
# 1. For this approach i conditional formatted the temps table in excel, and it was clear that unofficial summer end is on September 26th where all the values for all the years are below
#  the average of everyday of each year.
# 2. The climate has also gotten warmer in time because the cusum approach shows that the climate has gotten also warmer throughout the years in the summer since average values are higher than the average of total summer average of 86 degC.
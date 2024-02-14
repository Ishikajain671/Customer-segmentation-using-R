#k means clustering using elbow method

data<- read.csv("C:/Users/Amit jain/OneDrive/Desktop/Customers.csv")

any(is.na(data))
#FALSE indicates that there is ‘no missing values.
#Therefore, we conclude that the data is clean 
#and we can proceed to data visualisation.

#Customer Gender Visualization

a=table(data$Gender)
a

barplot(a,main="Using BarPlot to display Gender Comparision",
        ylab="Count",
        xlab="Gender",
        col=c("#009999", "#D6604D" ),
        legend=rownames(a))

#From the above barplot, 
#we conclude that the number of females is higher than the males.

p=round(a/sum(a)*100)
lable=paste(c("Female","Male")," ",p,"%",sep=" ")

install.packages("plotrix")
library(plotrix)

pie3D(p,labels=lable,
      main="Pie Chart Depicting Ratio of Female and Male",
      col = 2:3, labelcol = "red",
      border = "white",
      explode = 0.1)

#From the above chart, we conclude that the percentage 
#of females is 56%, whereas the percentage of male
#in the customer dataset is 44%.

#Visualization of Age Distribution

hist(data$Age,
     col="#D1E5F0",
     main="Histogram to Show Count of Age Class",
     xlab="Age Class",
     ylab="Frequency",
     labels=TRUE)

boxplot(data$Age,
        col="#4393C3",
        ylab = "Age Class",
        main="Boxplot for Descriptive Analysis of Age")

#From the above two visualizations, we conclude that the
#maximum customer ages are between 30 and 35. 
#The minimum age of customers is 18, whereas, the maximum age is 70.


#Visual Analysis of the Annual Income of the Customers

hist(data$Annual.Income..k..,
     col="#AE4371",
     main="Histogram for Annual Income",
     xlab="Annual Income Class",
     ylab="Frequency",
     labels=TRUE)

plot(density(data$Annual.Income..k..),
     col="yellow",
     main="Density Plot for Annual Income",
     xlab="Annual Income Class",
     ylab="Density")

polygon(density(data$Annual.Income..k..),
        col="aquamarine1")

#From the above descriptive analysis, we can conclude that the
#minimum annual income of the customers is $15k and the maximum
#income is $137k. 
#People earning an average income of $70k have the highest 
#frequency count in our histogram distribution. 
#The average salary of all the customers is $60.56k. 

#Evaluating Spending Score of the Customers

hist(data$Spending.Score..1.100.,
     main="HistoGram for Spending Score",
     xlab="Spending Score Class",
     ylab="Frequency",
     col="darkmagenta",
     labels=TRUE)


boxplot(data$Spending.Score..1.100.,
        horizontal=TRUE,
        col="pink",
        main="BoxPlot for Descriptive Analysis of Spending Score")

#The minimum spending score is 1, maximum is 99 and the
#average is 50.20.
#From the histogram, we conclude that customers between 
#class 40 and 50 have the highest spending score among all the classes.

# K-MEANS
x = data[4:5]

# Using the elbow method to find the optimal number of clusters
set.seed(123)
# wcss- within cluster sum of squares
wcss = vector() 


for (i in 1:10) wcss[i] = sum(kmeans(x,i)$withinss)

plot(x = 1:10,
     y = wcss,
     type = 'b',
     main = paste('The Elbow Method/Cluster of Clients'),
     xlab = 'Number of clusters',
     ylab = 'WCSS')

# Fitting K-Means to the dataset
set.seed(29)
kmeans = kmeans(x = x,
                centers = 5,
                iter.max = 500,
                nstart = 10)

# Visualizing the clusters, only in 2D
library(cluster)
clusplot(x = x,
         clus = kmeans$cluster,
         lines = 0,
         shade = TRUE,
         color = TRUE,
         labels = 2,
         plotchar = FALSE,
         span = TRUE,
         main = paste('Clusters of customers'),
         xlab = 'Annual Income',
         ylab = 'Spending Score')


# interpretation 
# 3 - high income, high spending score - potential clients
# 2 - low income, high spending score - careless clients
# 1 - high income, low spending score - careful clients
# 5 - low income, low spending score - sensible clients
# 4 - average income, average spending score - standard clients

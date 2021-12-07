# Data115-Personal-dataset
Personal dataset project by Lacey Shivers

Motivation: 
The topic for my personal data is diabetes in women and different races.
A motivation for this project is the curiousity of diabetes overall, how it is affected in the population. I was led to this specific dataset provided by the Center for Disease Control (https://www.cdc.gov/chronicdisease/data/index.htm) by the abundance of publicly available data. This source of data is related to the chronic disease of diabetes (CDC), I decided on the CDC data source because they use an open source of data that is large. The data is collected from the Centers for Disease Control and Prevention, National Center for Chronic Disease Prevention and Health Promotion, Division of Population Health. The available data is updated this year of 2021, with the original dataset covering the years from 2010 to 2019 of diabetes in gender and different races of populations. The original data has data from all the states of populations affected. The data covers the CDC indicators for diabetes. I think this data will help potentially answer one of my questions. If the data can not answer one of my questions it can answer related questions of topic. The data is already processed into organized columns that covers data of years of chronic disease throughout the different states. Since it is open source and a large amount of data, I cut out all of the year start data before 2015 and the data containing the male gender to see a more recent trend of diabetes in women. The data will look at the diabetes and see how it is prevalent in people.

I plan on answering the questions 
 1) What is the average number of women who get diabetes in comparison to the different races of population? During the last 5 years.
 2) In 2019, did the average number increase or decrease for women and different races for people who got diabetes. 

Data Process: Using the found data sourced and collected by the CDC, I processed the gathered data for cleaning by first cutting out the unwanted or what I felt was not neccessary for answering the potential questions. I cut out all the data between the years 2010 and 2014, keeping the years of data between 2015 and 2019. Then with the remaining years I took out the male pertaining data to focus on women and different races of the states populations. Next I removed the Null data of the rows missing data of the within columns. With the remaining data from the cutting down, I continued to process the data by looking over match ups of columns to correlate with potential answers to questions of diabetes. The processed transformed data is filtered out and shows the years of filled data from 2015 to 2019, to be used for answering diabetes in populations of women and different races.

Using the most recently uploaded and transformed data to the github repository, Charts_U.S.C_D_I_D.xlsx, I put together a visualization below. 

Visualization:

Visualizations of Diabetes in Women and different races.

Contingency Table1 is between the different races and the year they were reported. 

Contingency Table2 is between the different races and number of cases per the amount of people in the different races reported.

![Screenshot (73)](https://user-images.githubusercontent.com/91345984/142364664-e2a29dde-5c5a-418f-815b-6306b7577ee9.png)

The scatterplot shows a visualization of the data.
The plot represents the women and different races by the year they were reported, and colored by the number of cases reported. 
The plot helps visualize the relationship between the number of cases in the different races that were reported between the years 2015 to 2019.

![image](https://user-images.githubusercontent.com/91345984/142364212-68153f6c-1b2f-46c1-b3a3-bae35e924e70.png)

Analysis:

Coding for analysis:
```{r analysis}
mydata <- read_excel("Charts_U.S._C_D_I_D.xlsx")
attach(mydata)

summary(mydata)
summary(stat(mydata))

mydata <- na.omit(mydata) # listwise deletion of missing data
mydata <- scale(mydata[,9])  # DataValue column of data 

# Determines the number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:10) wss[i] <- sum(kmeans(mydata, centers=i)$withinss)
plot(1:10, wss, type="b", xlab="Number of Clusters", ylab="Within groups")

fit <- kmeans(mydata, 5)
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)

library(ggplot2)

Q1 <- quantile(mydata$DataValue,0.25, na.rm = TRUE) 
Q3 <- quantile(mydata$DataValue,0.75, na.rm = TRUE) 
IQR1 <- Q3-Q1
Outlier1 <- which(mydata$DataValue < Q1-(1.5*IQR1) | mydata$DataValue > Q3+(1.5*IQR1))

# BOXPLOT for outliers
ggplot(mydata, aes(y=DataValue))+ geom_boxplot(outlier.colour="red", outlier.shape=4, outlier.size=4)

structable(Stratification1) # displays the number of people with diabetes in each group of category
table(Stratification1,YearStart) # displays the number of people with diabetes in each year for each group category over the years of 2015 to 2019. 
## table of stratification and yearstart is shown in the first visualization of contingency table one. 

``` 
![image](https://user-images.githubusercontent.com/91345984/144966977-12766f48-73c1-48fc-937f-54539226cfd6.png)
![image](https://user-images.githubusercontent.com/91345984/144958699-7addfbca-9e04-42ed-b95c-5d6b136a51a1.png)

I chose to do a statistics summary of the data, then use a k-means clustering of the data to check a column of data for outliers. The statistical summary shows that the diabetes data has some distribution of data samples for the data distribution. While the boxplot only checks one of the data columns, it shoes that there are many outliers in the data. The data value column is a component for the visualization of the data. This column is a measure of value for the diabetes through 2015 to 2019. The analytical techniques help represent the diabetes in women and different races by having a more accurate representation of the data. Being able to spot the outliers and the use of the summary guides to the answer of an analytical question. Over the last 5 years the data shows how in women and different races of population there is fluctuation in the data. In comparison of all the data from the analysis, the question of 'What is the highest number of women or different races who get diabetes over the years of 2015 to 2019 of percentage and cases?'. Looking at contengency table2 in the first visual it is seen that of the percentage of cases women (Females) are the most number of cases of the population, next to the overall group of the categories. However, covering all the analysis of data, in 2019 the Female population having diabetes declined from previous years. The clusters of the data represents the observations for the nearest mean. With the mean of the clusters used to evaluate the DataValue, it demonstrates the range of value for diabetes within the categories of women and different races.  

in conclusion the statistacal analysis was able to 




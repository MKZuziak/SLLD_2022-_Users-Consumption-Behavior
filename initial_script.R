getwd()
setwd("C:/Users/rober/XAI/R_progetto_Chiaromonte/OTT_profile")
data <- read.csv('Users_Consumption_Behavior.csv')
str(data)
sapply(data, function(x) sum(is.na(x)))# checks for nas in the whole dataframe

##add a new column Traget using cluster colunm

library(dplyr)

# Adding a column based on other column:
data <- data%>% mutate(Target = case_when(cluster == "0" ~ "Low",
    cluster == "1" ~ "Medium",
    cluster==  "2" ~ "High"
  ))

#check for missing values
sum(is.na(data))
sum(is.null(data))

###checks number of unique values in each columns
require(dplyr)
dict <- sapply(data, n_distinct)
print(class(dict))
dict

#to check which ones are the values
unique(data[c("Viber_time_occupation")])#only two values 0 and 9.437501
table(data$Viber_time_occupation)#number of zeros 1248 and 1 for the value 9.437501

#to drop all the columns that have unique values < = 2 
uniquelength <- sapply(data,function(x) length(unique(x)))
data <- subset(data, select=uniquelength>2)

#data <- data[,colSums(is.na(df))<nrow(df)]

###checks again the number of unique values in each columns
#after dropping some of them
require(dplyr)
dict <- sapply(data, n_distinct)
print(class(dict))
dict

###
#Function to replace names columns to reduce their length
#helpfull when plotting fetures
colClean <- function(x){ colnames(x) <- gsub("_occupation", "", colnames(x)); x }
data <- colClean(data)

#####
#correlation for just a few features
library(corrplot)
M<-cor(data[,2:5])
head(round(M,2))

#first type of correlation plot
corrplot(M, method="circle")

#second type of correlation plot
corrplot(M, method="number")

###Removing highly correlated variables
cor_matrix <- cor(data[,1:107])  # Correlation matrix
cor_matrix

cor_matrix_rm <- cor_matrix# Modify correlation matrix
cor_matrix_rm[upper.tri(cor_matrix_rm)] <- 0
diag(cor_matrix_rm) <- 0
cor_matrix_rm

data_new <- data[ , !apply(cor_matrix_rm,    # Remove highly correlated variables
                           2,
                           function(x) any(x > 0.95))]
head(data_new)   

##subset the dataframe by using columns names
cols_amazon <- names(data_new) %in% c('Amazon_data','AmazonVideo_time','AmazonVideo_data','Amazon_time')
data_new_amazon <- data_new[cols_amazon]
#data_new_amazon <- data_new[!cols_amazon]
#rm(data_new_amazon)


##corralation amazon
M<-cor(data_new_amazon)
head(round(M,2))
corrplot(M, method="number")


#
library(mvtnorm) # for the toy simulated example
library(factoextra) # contains also decathlon data 
library(scales)# to create ggplot-like figures in base R (color transparency)
library(ellipse) # to add elliptical confidence regions in base R plots 
library(corrplot) # correlation plots

##PCA
###excluding the last two columns - cluster and Target
#data2 <- data[,1:107]   # Excluding the "Species" label in column 5
#data2 <- scale(data2) # Standardize (column-wise)
#pairs(data2)

###plot pair of the columns
pairs(data2[, 2:3], main="OTT Data (red=Low,green=Medium,blue=High)",
      pch=21, bg=c("red","green3","blue")[unclass(data$Target)],
      oma=c(3,3,3,15))
par(xpd = TRUE)
legend("bottomright", fill = c("red","green3","blue"), legend = c( levels(data$Target)))
####Legend here does not work




#PCA
res <- prcomp(data2, scale = TRUE)
get_eig(res)
fviz_eig(res, addlabels = TRUE)


plot(get_eig(res)$cumulative.variance.percent, 
     type='b', axes=F, xlab='Dimensions', ylab='cumulative PVE', ylim=c(0,100))
abline(h=100, col=alpha('blue',0.5))
abline(h=80, lty=2, col='red', lwd=2) # thresholding
box()
axis(2, at=0:100,labels=0:100)
axis(1,at=1:ncol(data2),labels=1:ncol(data2))
grid()

loadings <- res$rotation
loadings

fviz_pca_var(res,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE )    # Avoid text overlapping
################################################################



library(tidyverse)
library(nycflights13)
theme_set(theme_bw(base_size=16))

#creates a small dataframe
data3 <- data %>%
  select(Amazon_time_occupation, AmazonVideo_time_occupation, Target)%>%
  sample_n(500) %>%
  drop_na()
#plot a scatter plot for the two variables in data3
data3%>%
  ggplot(aes(Amazon_time_occupation,AmazonVideo_time_occupation, color=Target)) +
  geom_point(alpha=0.5, size=2) +
  labs(y="AmazonVideo_time_occupation", x="Amazon_time_occupation", subtitle="Scatter plot with Amazon Time ")

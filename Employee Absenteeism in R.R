#get Working directory

setwd("D:/Edwisor assignments/obsenteeism")

getwd()


#Load Libraries
x = c("ggplot2", "corrgram", "DMwR", "caret", "randomForest", "unbalanced", "C50", "dummies", "e1071", "Information",
      "MASS", "rpart", "gbm", "ROSE", 'sampling', 'DataCombine', 'inTrees','xlsx')

#install.packages(x)
lapply(x, require, character.only = TRUE)
install.packages('readxl')

install.packages("ggExtra")

library("ggExtra")
library("ggplot2")
library('readxl')

# Install  Libraries
library("dplyr")
library("plyr")
library("ggplot2")
library("data.table")
library("GGally")
library(tidyr)



#  Function to Converty  Data Types as  Factor  or numeric   based on given type

convert_factor_type= function(df,cat_names,convert_type) {
  
  if (convert_type== "factor")
    df[cat_names] <- lapply(df[cat_names], as.factor)
  else 
    df[cat_names] <- lapply(df[cat_names], as.numeric)
  df
}

# This Function will take input as data frame  and Numeric Columns and gives output as
# box plot relation ship between  Target Variable and  Independent numeric variable

plot_box = function(df, cols, col_x = 'Churn'){
  options(repr.plot.width=4, repr.plot.height=3.5) # Set the initial plot area dimensions
  for(col in cols){
    p = ggplot(df, aes_string(col_x, col)) + 
      geom_boxplot() +
      ggtitle(paste('Box plot of', col, '\n vs.', col_x))
    print(p)
  }
}



# This  Function will  take data frame and categorical columns as input
# and give  group plots between  independenta and target variable


# This Function will take dataframe  and numeric columns as input and 
# it treat outliers  using  boxplot and  return dataframe  after treating
treat_outliers  <- function(data,numeric_columns) {
  
  for (col in numeric_columns) {
    val = data[,col][data[,col] %in% boxplot.stats(data[,col])$out]
    
    df_churn_out = data[which(!data[,col] %in% val),]
    
  }  
  df_churn_out
}



# this function  will take data frame and numeric data as input and give 
# dataframe as output after  convering  numeric variables values into standardization form
standardForm_convert  <- function(data,num_col) {
  
  for(col in num_col){
    print(col)
    data[,col] = (data[,col] - mean(data[,col]))/sd(data[,col])
  }
  data
}




#  This function will take  data frame and categorical  as iput and gives output as data frame with encoded categorical data
encode_categorical  <- function(data,cat_columns) {
  
  for(col in cat_columns ) {
    data[,col]=as.numeric(as.factor(data[,col]))
  }
  data
}

# This  function will take  input as  data frame and  categorical variables and 
#give output as  barplot as target variable as output
bar_plot <- function(data,cat_variables,target="Absenteeismtime") {
  
  for(col in cat_variables){
    bar<-ggplot(df_absent) + 
      geom_bar(aes_string(x=col,y=target),stat = "identity",position = "dodge")
    print(bar)
  }
  
}


# This function will take category variable as input and   inputemissing values using median
missing_values_numeric <-function(data,numeric_col) {
  
  for (col in numeric_col) {
    if (percentage < 20  &&  class(data[[col]]) = 'numeric') {
      
      data[[col]][is.na(data[[col]])] = median(data[[col]] ,na.rm = T)
      data
    }
    
  }
}

# This function will take category variable as input and   inputemissing values using mode
missing_values_category <-function(data,cat_variables){
  for (col in cat_variables ){
    if (percentage < 20  &&  class(data[[col]]) = 'catrgory') {
      data[[col]][is.na(data[[col]])] = mode(data[[col]] ,na.rm = T)
    data
    }
  }
}




joint_plot <- function(data,numeric_cols,target_variable='Absenteeismtime') {
  #  This function will take  dataframe and numeric variables as inputand  gives
  #joint plot  to show relation between  numeric variables and target variables
  
  for(col in numeric_cols) {
    p <- ggplot(df_absent, aes_string(col, target_variable)) + geom_point() + theme_classic()
    ggExtra::ggMarginal(p, type = "histogram")
    print(p)
  }
}

# This function will take  data and numeric  columns  as inout and
#delete  rows which contains  outliers

treat_outliers  <- function(data,numeric_columns) {
  
  for (col in numeric_columns) {
    val = data[,col][data[,col] %in% boxplot.stats(data[,col])$out]
    
    df_absent_out = data[which(!data[,col] %in% val),]
    
  }  
  df_absent_out
}




# this function  will take data frame and numeric data as input and give 
# dataframe as output after  convering  numeric variables values into standardization form


standardForm_convert  <- function(data,num_col) {
  
  for(col in num_col){
    print(col)
    data[,col] = (data[,col] - mean(data[,col]))/sd(data[,col])
  }
  data
}

library(ggplot2)
library(reshape2)



# read excel in  r 

df_absent <- read_excel("Absenteeism_at_work_Project.xls")

# Change  COlumn Names 

colnames(df_absent) <- c("ID","Reasonforabsence" ,"Monthofabsence","Dayoftheweek","Seasons","Transportationexpense" ,"Distancefromresidence"
                         ,"Servicetime","Age","WorkloadAverage","Hittarget","Disciplinaryfailure","Education","Son", "Socialdrinker"
                         ,"Socialsmoker","Pet","Weight","Height","BMI","Absenteeismtime")

# CHeck Columns  names
df_obsent.columns

str(df_absent)


# Under standing data
df_obsent.head()

# Summary Of Data

df_obsent.info()


# Under standing data

head(df_absent)

# Summary Of Data

str(df_absent)

summary(df_absent)

# this data set contains 740 rows and 21 columns  out of this 21 columns  nine  columns are  categorical and remaining  
#columns are  Numeric

# Creating the  copy of  Data Frame
df_obsentees = df_absent

# Categorical Columns  inc data
cat_variables = c('Reasonforabsence','Monthofabsence','Dayoftheweek','Seasons','Disciplinaryfailure','Education','Son','Socialdrinker','Socialsmoker','Pet')


# Convert Given   Categorical Columns into type  factor
df_absent<-convert_factor_type(df_absent,cat_variables,"factor")
str(df_absent)

#convert given numeric columns into numeric type
numeric_variables <-names(df_absent)[sapply(df_absent, is.numeric)]

str(df_absent)
###############  Analyse  Univariate  Distribution #####################
#  ANalyising  Target Variable 

# It seems that   'Absenteeism time in hours' is not normally  distributed most of the obsenteeism hours lies between 0-20 hrs
# and few member  having  obsentees hours  more than 100  also it seems  there are   outliers  are present in the data


ggplot(df_absent) +
  geom_histogram(aes(x=Absenteeismtime),binwidth = 5,fill='grey')


#In Below figure it is showing that Valriables 'Age','BMI','distanceFromResidence','Hittarget','Eervicetime',
#'Transportationexpenses' , 'Weight' and 'WorkloadAverage'  are looking bit normally distributed but not perfect normal distribution still few outliers are present in each variable
ggplot(gather(df_absent[,numeric_variables]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

#  Analyse  realationship using  Joint Plot

joint_plot(df_absent,numeric_variables)

######################## Bivariate  Relationship  btween categorical  Variable and Target Variable ##########


bar_plot(df_absent,cat_variables)


###########################   Missing  Values ###################
missing_values_numeric(df_absent,numeric_variables)
missing_values_category(df_absent,cat_variables)


##########################Outlier  Analysis ###########################

num_col= c('Transportationexpense','Distancefromresidence',
          'Servicetime','Age','Hittarget','Weight',
          'Height','BMI','Absenteeismtime')



df_absent_out<-treat_outliers(df_absent,numeric_variables)

#  Variable  distribution after treating outliers

ggplot(gather(df_absent[,num_col]), aes(value)) + 
  geom_histogram(bins = 10) + 
  facet_wrap(~key, scales = 'free_x')

########################## Feature Engineering ################
#This plot is showing clearly that relation ship between  'Height' , 'Weight' and 'BMI'
#Hence  BMI is depends  on Height and Weight  than there is no need of  Height  and Weight variables


corrgram(df_absent_out[,num_col], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")


df_absent_out = subset(df_absent_out,select=-c(Height,Weight))


#########################  Standardised  Numeric  Variables ################

df_absent_out<- standardForm_convert(df_absent_out,numeric_variables)

######################  Scaling  Categorical Variables #####################

df_absent_out <- encode_categorical(df_absent_out,cat_variables)


######################### Modelling #########################


#load libraries
library(NbClust)


#extract number of clusters to bulid
NBclust_res = NbClust(df_new, min.nc=2, max.nc=15, method = "kmeans")

#Barplot to analys the optimum clusters
barplot(table(NBclust_res$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")


#below graph showing that graph is decrease  drastically when it is greater than 2.5  and it slow down from than
# an elbow shape is there at nearly 3
# Number of clusters for this data is 3

#K-mean clustering
kmeans_model = kmeans(df_new, 3, nstart=25)

#Summarize cluster output

df_absent_out["cluster_data"]=kmeans_model


#  Analyse the  Clusters  variable

table(df_absent_out["cluster_data"])


#########  COmpare the  NumericVariables

aggregate(Absenteeismtime~cluster_data,data=df_absent_out,FUN=mean)
aggregate(Transportationexpense~cluster_data,data=df_absent_out,FUN=mean)
aggregate(Distancefromresidence~cluster_data,data=df_absent_out,FUN=mean)
aggregate(Servicetime~cluster_data,data=df_absent_out,FUN=mean)
aggregate(Age~cluster_data,data=df_absent_out,FUN=mean)
aggregate(AbsenteWorkloadAverageeismtime~cluster_data,data=df_absent_out,FUN=mean)
aggregate(Hittarget~cluster_data,data=df_absent_out,FUN=mean)
aggregate(BMI~cluster_data,data=df_absent_out,FUN=mean)

#  Reason for Obsentees  analyse for categorical variables


ggplot(df_absent) +
  geom_bar(aes(x=Reasonforabsence,y=Absenteeismtime),fill=cluster_data,stat = "identity",position = "dodge") 
  
# relationhip between Monthofabsence and Clusterdata

ggplot(df_absent) +
  geom_bar(aes(x=Monthofabsence,y=Absenteeismtime),fill=cluster_data,stat = "identity",position = "dodge") 

  # relationhip between socialdrinker and Clusterdata


ggplot(df_absent) +
  geom_bar(aes(x=socialdrinker,y=Absenteeismtime),fill=cluster_data,stat = "identity",position = "dodge") 

################
#What changes company should bring to reduce the number of absenteeism
#Ans : 
#i.	 Company  should  decrease the Service Time and Work Load of Employees(Especially the young Employees)
#ii.	Company  should Provide Doctor Consultant in Office  Especially in  Spring and Summer Season  since most of the employees suffering with infectious diseases in winter and Spring Season  and  might be because of high work pressure most of the employees are suffering with ICD - 6 ( Diseases of the nervous system) mostly during summer season.

###2. Question  
##How much losses every month can we project in 2011 if same trend of absenteeism continues?

aggregate(Absenteeismtime~Monthofabsence,data=df_absent_out,FUN=mean)





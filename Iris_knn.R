


##################################################################
#                 IRIS FLOWER CLASSIFICATION (R)                 #
##################################################################


# LOAD PACKAGES
## Here if you haven't installed these packages do so!

if (!require(ggplot2))install.packages("ggplot2") 
if (!require(ggfortify))install.packages("ggfortify") 
if (!require(caret))install.packages("caret") 
if (!require(class))install.packages("class")
if (!require(gridExtra))install.packages("gridExtra") 
if (!require(GGally))install.packages("GGally") 
if (!require(RGraphics))install.packages("RGraphics") 
if (!require(plotly))install.packages("plotly") 
if (!require(gmodels))install.packages("gmodels") 


#################################################################
#                  Attaching Necessary Library                  #
#################################################################


library(ggplot2)
library(ggfortify)
library(caret)
library(e1071)
library(tibble)
library(class)
library(gridExtra)
library(plotly)
library(GGally)
library(plotly)
library(gmodels)

##################################################################
#                          GETTING DATA                          #
##################################################################


url <- 'https://raw.githubusercontent.com/M-nachid/knn_iris/main/iris.xlsx'

iris <- readxl::read_excel(url)

View(iris)

iris <- as_tibble(iris)
head(iris)

var_names <- gsub("\\.","_", colnames(iris))

colnames(iris) <- var_names
colnames(iris)

car::brief(iris)

##################################################################
#                      EXPLORATORY ANALYSIS                      #
##################################################################


# SEPAL PROPERTIES INTERACTIONS

gg1<-iris %>% ggplot(aes(x=Sepal_Width,y=Sepal_Length, 
                shape=Species, 
                color=Species)) + 
  theme(panel.background = element_rect(fill = "gray85"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray0"),
        axis.line.y = element_line(colour="gray0")) +
  geom_point(size=1) + 
  labs(title = "Sepal Width Vs. Sepal Length")

ggplotly(gg1)

# PETAL PROPERTIES INTERACTIONS
gg2<-ggplot(iris,
            aes(x=Petal_Width,y=Petal_Length, 
                shape=Species, 
                color=Species)) + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray1"),
        axis.line.y = element_line(colour="gray1")) + 
  geom_point(size=2) + 
  labs(title = "Petal Length Vs. Petal Width")

ggplotly(gg2)


########################################################################################################################################################################################################################################################################################
##  The plots below shows setosa to be most distinguisable of the three species 
##with respect to both sepal and petal attributes. We can infer then that 
##the setosa species will yield the least prediction errors, while the other 
##two species, versicolor and virginica, might not.  ##
########################################################################################################################################################################################################################################################################################



# PAIRWISE MATRIX FOR OUR DATA SET
pairs <- ggpairs(iris,
                 mapping=aes(color=Species),
                 columns=1:4) + 
  theme(panel.background = element_rect(fill = "gray98"),
        axis.line   = element_line(colour="black"),
        axis.line.x = element_line(colour="gray"),
        axis.line.y = element_line(colour="gray")) 
pairs
ggplotly(pairs) %>%
  layout(showlegend = FALSE)


############################################################################
##  This plot reduces the dimensions and gives an overarching view of the 
##interactions of the different attributes.##
############################################################################


##################################################################
#                      MODEL ESTIMATION                          #
##################################################################


#########################################################################
##  The Kth-Nearest Neighbor algorithm predicts based on majority votes, 
##measuring a certain number of neighboring observation points (k) 
##and classifies based on attribute prevalence using Euclidean distance. 
##Check the documentation on its packageCLASS as well (?class).  ##
#########################################################################

# Creating training/test set 

set.seed(88)
trainIndex <- createDataPartition(iris$Species, 
                                  p = .8, 
                                  list = FALSE, 
                                  times = 1)
# Creating 80 20 split 
training_set <- iris[trainIndex,]

test_set  <- iris[-trainIndex,]

# USING CARET PACKAGE TO ESTIMATE OPTIMAL K

knn_fit <- train(Species ~ ., 
             data = training_set, 
             method = "knn")

# Here we output the results from fit! 
knn_fit


############################################################
##  The terminal output above shows that our optimal k = 5 
##      based on the Accuracy and Kappa values.  ##
############################################################


# PREDICTION RESULTS  
# Predict
predict_test_set <- predict(knn_fit,
                            newdata = test_set)

CrossTable(x = test_set$Species, 
           y = predict_test_set, 
           prop.chisq=FALSE)

# WE CAN ESTIMATE OUR TEST ERROR RATE AS FOLLOWS FROM OUR TABLE:


#######################################################################################
##  We can see here that the model predicted virginica when it was actually versicolor
##which from our exploratory analysis we assumed there would be some prediction errors
##since these two species were the least distinguishable among all three species. 
##Both tables are just reiterating the results, but we received two wrong prediction. 
##Thus we calculate the test error as:  ##
######################################################################################



print(round(1 - knn_fit$results$Accuracy[1], 4))


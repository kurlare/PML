## We want to predict `classe` by a certain number of features related to the accel, 
## magnet, and gyro variables

## In this project, your goal will be to use
## data from accelerometers on the belt, forearm, arm, and dumbell of 6 participants.

## Read in train, test sets.  Get rid of NA columns in testing data
training <- read.csv(
    url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),
    row.names = NULL)

testing <- read.csv(
    url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),
    row.names = NULL)

testing <- testing[, colSums(is.na(testing)) != nrow(testing)]

## Find the index of matching columns in testing, training
matches_index <- match(colnames(testing), colnames(training))
training2 <- training[, matches_index[-60]] ## Drop problem_id column which became NA
training2$classe <- training$classe
training2 <- training2[-c(1,2,3,4,5,6,7)]

## process: pick features from training set that have measurements in test set,
## match the columns + classe column, fit the model to the training set, run 
## it on the test set.

## fast random forest model, very high accuracy
fit.rf <- randomForest(classe ~ ., data = training3, importance = TRUE)

## confusion matrix for new predictions on train.test set
pred <- predict(fit.rf, training3.test)
training3.test$predRight <- pred == training3.test$classe
table(pred,training3.test$classe)

## Visualize frequency by action type
totals <- training3[, grep("total|classe", colnames(training3))]

belt_accel_plot <- ggplot(totals, aes(x = totals$total_accel_belt,
                          colour = classe, fill = classe)) + 
                          geom_density(alpha = 0.5) +
                          labs(title = "Belt Accel Density", x = "Total Belt Accel",
                            y = "Density")

arm_accel_plot <- ggplot(totals, aes(x = totals$total_accel_arm, 
                         colour = classe, fill = classe)) + 
                         geom_density(alpha = 0.5) +
                         labs(title = "Arm Accel Density", x = "Total Arm Accel",
                            y = "Density")

forearm_accel_plot <- ggplot(totals, aes(x = totals$total_accel_forearm,
                         colour = classe, fill = classe)) +
                         geom_density(alpha = 0.5) +
                         labs(title = "Forearm Accel Density", x = "Total Forearm Accel",
                             y = "Density")

dumbbell_accel_plot <- ggplot(totals, aes(x = totals$total_accel_dumbbell,
                         colour = classe, fill = classe)) +
                         geom_density(alpha = 0.5) +
                         labs(title = "Dumbbell Accel Density", x = "Total Dumbbell Accel",
                             y = "Density")

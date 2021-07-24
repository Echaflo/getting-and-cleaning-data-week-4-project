---
title: "El R Markdown Getting and Cleaning Data Course Project"
author: "ECHF"
date: "20210724"
---

## las Bibliotecas utilizadas para trabajar con  tablas  y dataframes
### Libraries used to work with tables and dataframes

library(data.table)
library(dplyr)


## se lee los archivos que contienen los metadatos
### the files containing the metadata are read


featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

## ahora vamos a leer los archivos que contienen la informacion tanto de entrenamiento como test 
### now we are going to read the files that contain the information for both training and test


## Leer datos de entrenamiento
### Read training data
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

## Leer datos de prueba
### Read test data
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

## ahora creamos los subconjuntos al unir la informacion que leimos antes
### now we create the subsets by joining the information we read before

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

##ahora asignamos los nombres a las columnas con ayuda de la informacion de los metadatos
###Now we assign the names to the columns with the help of the metadata information

colnames(features) <- t(featureNames[2])

## Combinar los datos,se crea el conjunto de datos completos se almacenan ahora en .features,activity,subject,completeData
### Merge the data, the complete dataset is created are now stored in .features, activity, subject, completeData

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

## ahora se estrae de la informacion solo aquellas medidas que nos interesan: la media,la desviación estándar
## se extrae los índices de columnas que tienen mean o std en ellos.
### Now only those measures that interest us are extracted from the information: the mean, the standard deviation
### extracts column indexes that have mean or std in them.

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

## Para validar se obtiene solo las columnas que nos interesan en completeData
### To validate, only the columns that interest us in completeData are obtained

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)
### [1] 10299   563

## ahora creamos una nueva variable con la informacion que obtivimos antes y le validamos
### now we create a new variable with the information we obtained before and validate it

extractedData <- completeData[,requiredColumns]
dim(extractedData)
### [1] 10299    88


## utiliza nombres de actividad descriptivos para asignar un nombre a las actividades del conjunto de datos
## El campo en es originalmente de tipo numérico. por lo que se tiene que hacer un cast 
## Necesitamos cambiar su tipo a carácter para que pueda aceptar nombres de actividad.
## Los nombres de actividad se toman de los metadatos.

### use descriptive activity names to name the activities in the dataset
### The field in is originally of type numeric. so a cast has to be done
### We need to change its type to character so that it can accept activity names.
### Activity names are taken from metadata.

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
## Necesitamos hacer un cast a  la variable, para cambiar su clase
### We need to cast the variable, to change its class

extractedData$Activity <- as.factor(extractedData$Activity)

## ahora necesitamos renombrar el nombre de la svariables se ve a continuacion lo que se tiene 
## para despues cambiarlos usando expresiones regulares.
### now we need to rename the name of the svariables you see below what you have
### and then change them using regular expressions.

names(extractedData)



#### Al examinar , podemos decir que se pueden reemplazar las siguientes siglas:extractedData
###
#### Acc se puede reemplazar con acelerómetro
### 
#### Gyro se puede reemplazar con Giroscopio
### 
#### BodyBody se puede reemplazar con el cuerpo
### 
#### Mag se puede reemplazar con magnitud
### 
#### El carácter se puede reemplazar con frecuencia: f
### 
#### El carácter se puede reemplazar con tiempo: t


##### When examining, we can say that the following acronyms can be replaced: extractedData
###
##### Acc can be replaced with accelerometer
###
##### Gyro can be replaced with Gyroscope
###
##### BodyBody can be replaced with the body
###
##### Mag can be replaced with magnitude
###
##### The character can be replaced frequently: f
###
##### The character can be replaced with time: t 



names(extractedData)<-gsub("Acc", "Accelerometer", names(extractedData))
names(extractedData)<-gsub("Gyro", "Gyroscope", names(extractedData))
names(extractedData)<-gsub("BodyBody", "Body", names(extractedData))
names(extractedData)<-gsub("Mag", "Magnitude", names(extractedData))
names(extractedData)<-gsub("^t", "Time", names(extractedData))
names(extractedData)<-gsub("^f", "Frequency", names(extractedData))
names(extractedData)<-gsub("tBody", "TimeBody", names(extractedData))
names(extractedData)<-gsub("-mean()", "Mean", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-std()", "STD", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("-freq()", "Frequency", names(extractedData), ignore.case = TRUE)
names(extractedData)<-gsub("angle", "Angle", names(extractedData))
names(extractedData)<-gsub("gravity", "Gravity", names(extractedData))


## ahora se muestra como quedaron los nombres ya cambiados.
### now it shows how the names were already changed.

names(extractedData)




## ahora se creara un subconjunto de datos  
### independiente con el promedio de cada variable para cada actividad y cada sujeto

## now a subset of data will be created
### independent with the average of each variable for each activity and each subject

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)



## Creamos como un conjunto de datos con promedio para cada actividad y tema.
##ademas de crear unm subconjunto de datos con las informaciuoin resumida que se nos pide.

### We create as a data set with average for each activity and topic.
###In addition to creating a subset of data with the summary information that is requested.

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)










## the information of the data dictionary is in the file / UCI HAR Dataset / readme.txt in the following lines this dictionary is reproduced ##
## this work in a general way I only collect information about a work already done and that is described in the readme.txt file
## so here only work is done in a practical way for an exercise of R language practices


## generally 



### la informacion del diccioanrio de datos esta en el archivo /UCI HAR Dataset/readme.txt  en las siguientes linea se reproduce ### este diccionario 
###este trabajo de manera geneal solo recabo informacion de un trabajo ya hehco y que viene descrito en el rchivo readme.txt 
###por lo que aqui solo se hace un trabajo de manera practica para un ejecicio de de practicas del lenguaje R


### demanera general

### For each record it is provided:
####======================================

####- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
####- Triaxial Angular velocity from the gyroscope. 
####- A 561-feature vector with time and frequency domain variables. 
####- Its activity label. 
####- An identifier of the subject who carried out the experiment.

####The dataset includes the following files:
####=========================================

####- 'README.txt'

####- 'features_info.txt': Shows information about the variables used on the feature vector.

####- 'features.txt': List of all features.

####- 'activity_labels.txt': Links the class labels with their activity name.

####- 'train/X_train.txt': Training set.

####- 'train/y_train.txt': Training labels.

####- 'test/X_test.txt': Test set.

####- 'test/y_test.txt': Test labels.

####The following files are available for the train and test data. Their descriptions are equivalent. 

####- 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

####- 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard ####gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and ####'total_acc_z_train.txt' files for the Y and Z axis. 

####- 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the ####total acceleration. 

####train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. ####The units are radians/second. 






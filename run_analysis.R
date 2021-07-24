# las Bibliotecas utilizadas para trabajar con  tablas  y dataframes

library(data.table)
library(dplyr)


# se lee los archivos que contienen los metadatos


featureNames <- read.table("UCI HAR Dataset/features.txt")
activityLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)

# ahora vamos a leer los archivos que contienenla informacion tanto de entrenamiento como test 


# Leer datos de entrenamiento
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
activityTrain <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
featuresTrain <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)

# Leer datos de prueba
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
activityTest <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
featuresTest <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)

# ahora creamos los subconjuntos al unir la informacion que leimos antes

subject <- rbind(subjectTrain, subjectTest)
activity <- rbind(activityTrain, activityTest)
features <- rbind(featuresTrain, featuresTest)

#ahora asignamos los nombres a las columnas con ayuda de la informacion de los metadatos

colnames(features) <- t(featureNames[2])

# Combinar los datos
# Los datos en , y se fusionan y los datos completos se almacenan ahora en .featuresactivitysubjectcompleteData

colnames(activity) <- "Activity"
colnames(subject) <- "Subject"
completeData <- cbind(features,activity,subject)

# Parte 2 - Extrae sólo las mediciones de la media y la desviación estándar para cada medición
# Extraiga los índices de columna que tienen mean o std en ellos.

columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case=TRUE)

# Agregue columnas de actividad y asunto a la lista y examine la dimensión de completeData

requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)
## [1] 10299   563

# Creamos con las columnas seleccionadas en . Y de nuevo, nos fijamos en la dimensión de .extractedDatarequiredColumnsrequiredColumns

extractedData <- completeData[,requiredColumns]
dim(extractedData)
## [1] 10299    88


# Parte 3: utiliza nombres de actividad descriptivos para asignar un nombre a las actividades del conjunto de datos
# El campo en es originalmente de tipo numérico. Necesitamos cambiar 
# su tipo a carácter para que pueda aceptar nombres de actividad. Los nombres de
# actividad se toman de los metadatos.activityextractedDataactivityLabels

extractedData$Activity <- as.character(extractedData$Activity)
for (i in 1:6){
  extractedData$Activity[extractedData$Activity == i] <- as.character(activityLabels[i,2])
}
# Necesitamos factorizar la variable, una vez que se actualizan los nombres de actividad.activity

extractedData$Activity <- as.factor(extractedData$Activity)

# 
# Parte 4 : etiqueta adecuadamente el conjunto de datos con nombres de variables descriptivas
# Estos son los nombres de las variables en extractedData

names(extractedData)



# Al examinar , podemos decir que se pueden reemplazar las siguientes siglas:extractedData
# 
# Acc se puede reemplazar con acelerómetro
# 
# Gyro se puede reemplazar con Giroscopio
# 
# BodyBody se puede reemplazar con el cuerpo
# 
# Mag se puede reemplazar con magnitud
# 
# El carácter se puede reemplazar con frecuenciaf
# 
# El carácter se puede reemplazar con tiempot



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


# Estos son los nombres de las variables en después de que se editanextractedData

names(extractedData)




# Parte 5 - A partir del conjunto de datos en el paso 4, crea un segundo conjunto de datos ordenado 
# independiente con el promedio de cada variable para cada actividad y cada sujeto
# En primer lugar, establezcamos como una variable de factor.Subject

extractedData$Subject <- as.factor(extractedData$Subject)
extractedData <- data.table(extractedData)



# Creamos como un conjunto de datos con promedio para cada actividad y tema. Luego,
# ordenamos los enties y lo escribimos en el archivo de datos que contiene los datos procesados.tidyDatatidyDataTidy.txt

tidyData <- aggregate(. ~Subject + Activity, extractedData, mean)
tidyData <- tidyData[order(tidyData$Subject,tidyData$Activity),]
write.table(tidyData, file = "Tidy.txt", row.names = FALSE)









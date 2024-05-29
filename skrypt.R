library(car)
library(dplyr)
#args<- commandArgs(trailingOnly = false)
#if(length(args!=1)){
#  stop("sciezka do pliku musi byc jedynym argumentem")
#}
#file_path<-args[1]
#if (!file.exists(file_path)) {
# stop("Podany plik nie istnieje.")
#}

file_path<-"C:\\Users\\mysza\\Downloads\\przykladoweDane-Projekt (1).csv"
data<-read.csv2(file=file_path)
numeric_columns<-sapply(data,is.numeric)
#numeric_data<-data[,c(TRUE,numeric_columns[-1])]
numeric_column_names<-colnames(data[,numeric_columns])
#numeric_data<-data[,which(data)]
groups<-unique(data[,1])



print("statystyki ogolne:")
for(column in numeric_column_names){
  boxplot(data[,column],main=column)
}
for(group in groups){
  print(paste("grupa :",group))
  tmpdata<-data[which(data[,1]==group),]
  print(summary(tmpdata))
}

for(column in colnames(data))
{
  if(!is.numeric(data[,column])){
    print(table(data[,column]))
  }
}
#dane odstajace

for(column in numeric_column_names){
  print(paste("dane odstajace w kolumnie ",column,":"))
  tmp<-boxplot(data[,column],plot=FALSE)
  print(tmp$out)
}

#obliczenie wszystkich srednich
columnnames<-c()
for(column in colnames(data)){
  if(is.numeric(data[,column]))
    columnnames<-c(columnnames,column)
}
all_means<-data.frame(
  group=groups
)
for(column in columnnames){
  mean_vector<-c()
  for(group in groups){

    meanv<-mean(data[which(data[,1]==group),column],na.rm=TRUE)
    mean_vector<-c(mean_vector,meanv)
  }
  tmpdf<-data.frame(mean_vector)
  all_means<-cbind(all_means,tmpdf)
  names(all_means)[ncol(all_means)]<-column
}
rownames(all_means)<-groups


####uzupelnienie brakow
completed_rows<-c()
for(column in colnames(data))
{
  if(is.numeric(data[,column]))
  {
    for(row in seq_len(nrow(data)))
      if(is.na(data[row,column]))
      {
        data[row,column]<-all_means[data[row,1],column]
        completed_rows<-c(completed_rows,row)
      }
  }
}
if(length(completed_rows)>0){
  comrows<-paste(completed_rows,collapse="; ")
  print(paste("uzupelniono brakujace dane w rekordach: ",comrows))
}

wynik<- group_by(data,1)%>% summarise(
  srednia=mean("MON",na.rm=TRUE)
)
wynik





for(column in numeric_column_names){
  print(paste("statystyki z grupowaniem dla ",column,":"))
  podumowanie <- group_by(data, colnames(data)[1]) %>%
    summarise(
      count = n(),
      mean = format(round(mean(data[,column], na.rm = TRUE), 2), nsmall = 2),
      sd = format(round(sd(data[,column], na.rm = TRUE), 2), nsmall = 2),
      median = format(round(median(data[,column], na.rm = TRUE), 2), nsmall = 2)
    )
  print(podumowanie)
}

### shapiro test
print("sprawdzenie zgodnosci z rozkladem normalnym:")
for(column in numeric_column_names){
  good_for_aov=FALSE
  pvalueShapiroTestHSCRP <- data %>% group_by( colnames(data)[1]) %>%
    summarise(
      p.value = shapiro.test(data[,column])$p.value
    )
  pvalueShapiroTestHSCRP
  
  #pvalueShapiroTestHSCRP$p.value
  #pvalueShapiroTestHSCRP$p.value[(pvalueShapiroTestHSCRP[,1] == "CHOR1")]
  
  for(i in 1:length(pvalueShapiroTestHSCRP$p.value)){
    #print(pvalueShapiroTestHSCRP$p.value)
    if(pvalueShapiroTestHSCRP$p.value[i] < 0.05){
      cat("\ndla ",column," p-value = ", pvalueShapiroTestHSCRP$p.value[i], "< 0.05 - nie można założyć zgodności z rozkładem normalnym\n")
    }else{
      cat("\ndla ",column," p-value = ", pvalueShapiroTestHSCRP$p.value[i], "> 0.05 - można założyć zgodność z rozkładem normalnym\n")
      print(paste("ocena jednorodnosci wariancji dla: ",column))
      leveneTestResult<-leveneTest(data[,column]~data[,1],data=data)
      print(leveneTestResult)
      print(leveneTestResult$"Pr(>F)"[1])
      if(leveneTestResult$"Pr(>F)"[1]<0.05){
        cat("brak jednorodnosc\n")
      }else{
        cat("jednorodnosc\n")
        good_for_aov=TRUE
      }
      
    }
  }
  if(good_for_aov){
    print("aov\n\n\n")
    print(aov(data[,column]~data[,1],data=data))
    print(paste("summary: ",summary(aov(data[,column]~data[,1],data=data))))
    AOV_p_value<-summary(aov(data[,column]~data[,1],data=data))[[1]][["Pr(>F)"]][[1]]
    print(AOV_p_value)
    if(AOV_p_value<0.05){
      cat("sa roznice miedzy grupmami\n")
      
    }else{
      cat("brak roznic miedzy grupami\n")
    }
  }
}

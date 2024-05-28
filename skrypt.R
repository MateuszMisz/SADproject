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


#obliczenie wszystkich srednich
columnnames<-c()
for(column in colnames(data)){
  if(is.numeric(data[,column]))
    columnnames<-c(columnnames,column)
}
groups<-unique(data[,1])
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
  

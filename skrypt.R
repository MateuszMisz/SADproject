#args<- commandArgs(trailingOnly = false)
#if(length(args!=1)){
#  stop("sciezka do pliku musi byc jedynym argumentem")
#}
#file_path<-args[1]
#if (!file.exists(file_path)) {
 # stop("Podany plik nie istnieje.")
#}
file_path
data<-read.csv(file=file_path)
numeric_columns<-sapply(data,is.numeric)
#numeric_data<-data[,c(TRUE,numeric_columns[-1])]

numeric_data<-data[,which(data)]
for(row in rownames(data)){
  for (column in colnames(data)){
    if (column)
  }
}
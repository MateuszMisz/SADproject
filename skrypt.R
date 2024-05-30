library(car)
library(dplyr)
library(dunn.test)
library(ggpubr)
library(FSA)
openFolder<-function(folder_path){
  if(!dir.exists(folder_path))
  {
    dir.create(folder_path)
  }
  setwd(folder_path)
}
#args<- commandArgs(trailingOnly = false)
#if(length(args!=1)){
#  stop("sciezka do pliku musi byc jedynym argumentem")
#}
#file_path<-args[1]
#if (!file.exists(file_path)) {
# stop("Podany plik nie istnieje.")
#}
output_folder<-"C:\\Users\\mysza\\Documents\\SADproject\\wyniki"
mainFolder<-function(){
  setwd(output_folder)
}
openFolderFromMain<-function(folder_path){
  mainFolder()
  openFolder(folder_path)
}
file_path<-"C:\\Users\\mysza\\Downloads\\przykladoweDane-Projekt (1).csv"
data<-read.csv2(file=file_path)
numeric_columns<-sapply(data,is.numeric)
#numeric_data<-data[,c(TRUE,numeric_columns[-1])]
numeric_column_names<-colnames(data[,numeric_columns])
#numeric_data<-data[,which(data)]
groups<-unique(data[,1])
openFolder(output_folder)

##
print("statystyki ogolne:")


##statystyki ogolne
openFolderFromMain("statystyki_ogolne")
tmptext<-paste(numeric_column_names,sep=";")
text<-c(paste("parametr","min","max","srednia","mediana","IQR","wariancja","odchylenie standardowe",sep=";"))
for(column in numeric_column_names){
  png(filename=paste(column,"_boxplot.png",sep=""),width=800,height=600)
  boxplot(data[,column],main=column)
  dev.off()
  text<-c(text,paste(column,range(data[,column],na.rm=TRUE)[1],range(data[,column],na.rm=TRUE)[2],mean(data[,column],na.rm=TRUE),median(data[,column],na.rm=TRUE),IQR(data[,column],na.rm=TRUE),var(data[,column],na.rm=TRUE),sd(data[,column],na.rm=TRUE),sep=";"))
}
  writeLines(text,"statystyki_ogolne.csv")


##charakterystyki grup
openFolderFromMain("statystyki z podzialem na grupy")

text<-c(paste("grupa;","parametr;","min;","max;","srednia;","mediana;","IQR;","wariancja;","odchylenie standardowe;"))
for(group in groups){
  tmpdata<-data[which(data[,1]==group),]
  for(column in numeric_column_names){
    png(filename=paste("boxplot",group,column,".png",sep="_"),width=800,height = 600)
    boxplot(tmpdata[,column],main=paste(column,"dla",group,sep=" "))
    dev.off()
    text<-c(text,paste(group,column,range(tmpdata[,column],na.rm=TRUE)[1],range(tmpdata[,column],na.rm=TRUE)[2],mean(tmpdata[,column],na.rm=TRUE),median(tmpdata[,column],na.rm=TRUE),IQR(tmpdata[,column],na.rm=TRUE),var(tmpdata[,column],na.rm=TRUE),sd(tmpdata[,column],na.rm=TRUE),sep=";"))
  }
}
writeLines(text,"statystyki_grup.csv")
mainFolder()
#
#
#
#
#
#zrob cos z table!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#
#
#
for(column in colnames(data))
{
  if(!is.numeric(data[,column])){
    print(table(data[,column]))
  }
}
#dane odstajace
openFolderFromMain("statystyki_ogolne")
text<-c(paste("zmienna","dane odstajace",sep=";"))
for(column in numeric_column_names){
  print(paste("dane odstajace w kolumnie ",column,":"))
  tmp<-boxplot(data[,column],plot=FALSE)
  print(tmp$out)
  text<-c(text,paste(tmp$out,sep=";"))
}
writeLines(text,"dane odstajace.csv")

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
write.csv(data,"dane z uzupelnionymi brakami",row.names = FALSE)










### shapiro test

openFolderFromMain("analiza_porownawcza_miedzy_grupami")
print("sprawdzenie zgodnosci z rozkladem normalnym:")
text<-c("")
cnnnttt<-0
for(column in numeric_column_names){
  good_for_aov=FALSE
  pvalueShapiroTestHSCRP <- data %>% group_by( colnames(data)[1]) %>%
    summarise(
      p.value = shapiro.test(data[,column])$p.value
    )
  pvalueShapiroTestHSCRP
  png(filename = paste("wykres gestosci",column,"z podzialem na grupy.png",sep="_"),width=800,height=600)
  print(ggdensity(data, x =column,
            color =colnames(data)[1], fill = colnames(data)[1],
            palette = c("#99cc00", "#660099", "#0047b3"),
            ylab = "gęstośc",
            xlab = column
  ) + facet_wrap(~ colnames(data)[1], scales = "free"))
  dev.off()
  

  
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
    AOV_result<-aov(data[,column]~data[,1],data=data)
    print(paste("summary: ",summary(aov(data[,column]~data[,1],data=data))))
    AOV_p_value<-summary(aov(data[,column]~data[,1],data=data))[[1]][["Pr(>F)"]][[1]]
    print(AOV_p_value)
    if(AOV_p_value<0.05){
      cat("sa roznice miedzy grupmami\n")
      TukeyResult<-TukeyHSD(AOV_result)
      print(TukeyResult)
      writeLines(capture.output(print(TukeyResult)),paste(column,"_ANOV_istotne_statystycznie_roznice_miedzy_grupami.csv"))
      writeLines(capture.output(print(TukeyResult)),paste(column,"_ANOV_istotne_statystycznie_roznice_miedzy_grupami.txt"))
      cnnnttt<-cnnnttt+1
      
      
    }else{
      cat("brak roznic miedzy grupami\n")
    }
  }else{
    Kruskal_result<-kruskal.test(data[,column]~data[,1],data=data)
    Kruskal_p_value<-Kruskal_result$p.value
    print(Kruskal_p_value)
    if(Kruskal_p_value<0.05){
      cat("kruskalsa roznice")
      dunnTestResult<-dunnTest(data[,column],data[,1])
      print(dunnTestResult)
      writeLines(capture.output(print(dunnTestResult)),paste(column,"Kruskal_istotne_statystycznie_roznice_miedzy_grupami.csv"))
      writeLines(capture.output(print(dunnTestResult)),paste(column,"Kruskal_istotne_statystycznie_roznice_miedzy_grupami.txt"))
      cnnnttt<-cnnnttt+1
      
    }else{
      cat("kruskal nie ma roznic")
    }
  }
}


####korelacja
openFolderFromMain("wyniki_testow_korelacji")
text<-c(paste("grupa","zmienna1","zmienna2","wspolczynnik korelacji","metoda","p-value",sep=";"))
for(group in groups){
  for(i in 1:length(numeric_column_names)){
    for(j in 1:length(numeric_column_names)){
      method="pearson"
      if(i<j){
        FirstColumn<-data[which(data[,1]==group),numeric_column_names[i]]
        SecondColumn<-data[which(data[,1]==group),numeric_column_names[j]]
        FirstGroupSaphiro_pvalue <-shapiro.test(FirstColumn)$p.value
        SecondGroupSaphiro_pvalue<-shapiro.test(SecondColumn)$p.value
        if(FirstGroupSaphiro_pvalue>0.05 & SecondGroupSaphiro_pvalue>0.05){
          print(paste("w grupie",group,"dla parametrow",numeric_column_names[i],"i",numeric_column_names[j],"przeprowadzono test korelacji pearsona",sep=" "))
          CorResult<-cor.test(FirstColumn,SecondColumn,method="pearson")
          
        }else{
          print(paste("w grupie",group,"dla parametrow",numeric_column_names[i],"i",numeric_column_names[j],"przeprowadzono test korelacji spearmana",sep=" "))
          CorResult<-cor.test(FirstColumn,SecondColumn,method="spearman")
          method="spearman"
          
        }
        if(CorResult$p.value<0.05)
        {
          text<-c(text,paste(group,numeric_column_names[i],numeric_column_names[j],CorResult$estimate,method,CorResult$p.value,sep=";"))
          print(paste("istnieje korelacja; wspolczynnik = ",CorResult$estimate))
          png(filename = paste("wykres_korealcji",numeric_column_names[i],"~",numeric_column_names[j],"w grupie",group,".png",sep="_"),width=800,height=600)
              ggscatter(data[which(data[,1]==group),], x =numeric_column_names[i], y = numeric_column_names[j], 
                    add = "reg.line", conf.int = TRUE, 
                    cor.coef = TRUE, cor.method = method,
                    color = colnames(data)[1], fill = colnames(data)[1],
                    palette = c("#99cc00"),
                    ylab = numeric_column_names[i], 
                    xlab =numeric_column_names[j]
          )
              dev.off()
        }else{
          print("brak korelacji")
        }
      }
    }
  }
}

writeLines(text,"zaleznosci_miedzy_parametrami.csv")



print("zrob cos z table!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1")



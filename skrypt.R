library(car)
library(Hmisc)

library(dplyr)
library(dunn.test)
library(ggpubr)
library(FSA)
library(argparse)
library(ggplot2)
print("start")
parser<-ArgumentParser()
parser$add_argument("input",type="character")
parser$add_argument("output",type="character")
args<-parser$parse_args()
file_path<-args$input
output_folder<-args$output
#file_path<-"C:\\Users\\mysza\\Downloads\\przykladoweDaneZBrakami.csv"
#output_folder<-"C:\\Users\\mysza\\Documents\\SADproject\\wwwwwwwwwwwwwww"
if(!file.exists(file_path))
  stop("plk nie istnieje")
if(!dir.exists(output_folder))
  dir.create(output_folder)
setwd(output_folder)
openFolder<-function(folder_path){
  if(!dir.exists(folder_path))
  {
    dir.create(folder_path) 
  }
  setwd(folder_path)
}

corCoefInter <- function(corCoef) {
  if (corCoef <= -0.7 && corCoef > -1) {
    return("bardzo_silna_korelacja_ujemna")
  } else if (corCoef <= -0.5) {
    return("silna_korelacja_ujemna")
  } else if (corCoef <= -0.3) {
    return("korelacja_ujemna_o_średnim_natężeniu")
  } else if (corCoef <= -0.2) {
    return("słaba_korelacja_ujemna")
  } else if (corCoef < 0.2) {
    return("brak_korelacji")
  } else if (corCoef < 0.3) {
    return("słaba_korelacja_dodatnia")
  } else if (corCoef < 0.5) {
    return("korelacja_dodatnia_o_średnim_natężeniu")
  } else if (corCoef < 0.7) {
    return("silna_korelacja_dodatnia")
  } else if (corCoef <= 1) {
    return("bardzo_silna_korelacja_dodatnia")
  } else {
    return("Wartość_współczynnika_korelacji_jest_niepoprawna")
  }
}
#output_folder<-args[2]

mainFolder<-function(){
  setwd(output_folder)
}
openFolderFromMain<-function(folder_path){
  mainFolder()
  openFolder(folder_path)
}

print(file_path)
data<-read.csv2(file=file_path)
print(file.exists(file_path))
print("wypiszdata")
print(data)
print("podata")
numeric_columns<-sapply(data,is.numeric)
numeric_column_names<-colnames(data[,numeric_columns])
groups<-unique(data[,1])
openFolder(output_folder)
##
print("statystyki ogolne:")


##statystyki ogolne
#obliczenie wszystkich srednich
openFolderFromMain("statystyki_ogolne")

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
openFolderFromMain("statystyki_z_podzialem_na_grupy")

text<-c(paste("grupa","parametr","min","max","srednia","mediana","IQR","wariancja","odchylenie standardowe"))
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











### shapiro test

openFolderFromMain("analiza_porownawcza_miedzy_grupami")
print("sprawdzenie zgodnosci z rozkladem normalnym:")
text<-c("")
cnnnttt<-0
for(column in numeric_column_names){
  good_for_aov=TRUE
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
    good_for_aov=FALSE
      }else{
      cat("\ndla ",column," p-value = ", pvalueShapiroTestHSCRP$p.value[i], "> 0.05 - można założyć zgodność z rozkładem normalnym\n")
      print(paste("ocena jednorodnosci wariancji dla: ",column))
      leveneTestResult<-leveneTest(data[,column]~data[,1],data=data)
      print(leveneTestResult)
      print(leveneTestResult$"Pr(>F)"[1])
      if(leveneTestResult$"Pr(>F)"[1]<0.05){
        good_for_aov<-FALSE
        cat("brak jednorodnosc\n")
      }else{
        cat("jednorodnosc\n")
        
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
plots<-list()
openFolderFromMain("wyniki_testow_korelacji")
text<-c(paste("grupa","zmienna1","zmienna2","wspolczynnik_korelacji","metoda","p-value",sep=";"))
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
          text<-c(text,paste(group,numeric_column_names[i],numeric_column_names[j],CorResult$estimate,method,CorResult$p.value,corCoefInter(CorResult$estimate),sep=";"))
          print(paste("istnieje korelacja; wspolczynnik = ",CorResult$estimate))
          print("tu powininen byc wykresssssssssssssss")
          #png(filename = paste("wykres_korealcji",numeric_column_names[i],"~",numeric_column_names[j],"w grupie",group,".png",sep="_"),width=800,height=600)
          
          gg <- data[which(data[,1] == group), ]
          p <- ggscatter(gg, x = numeric_column_names[i], y = numeric_column_names[j], 
                         add = "reg.line", conf.int = TRUE, 
                         cor.coef = TRUE, cor.method = "pearson",
                         color = colnames(data)[1], fill = colnames(data)[1],
                         palette = c("#99cc00"),
                         ylab = numeric_column_names[j], 
                         xlab = numeric_column_names[i])
        ggsave(filename = paste("wykres_korelacji",numeric_column_names[i],"~",numeric_column_names[j],"w_grupie",group,".png",sep="_"),plot=p)
          print(p)
              #dev.off()
        }else{
          print("brak korelacji")
        }
      }
    }
  }
}

writeLines(text,"zaleznosci_miedzy_parametrami.csv")

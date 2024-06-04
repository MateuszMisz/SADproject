options(warn = -1)

options(show.error.messages = FALSE)
suppressMessages({
  
if (!require("car")) {
  install.packages("car")
  library("car")
}
if (!require("Hmisc")) {
  install.packages("Hmisc")
  library("Hmisc")
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library("dplyr")
}
if (!require("dunn.test")) {
  install.packages("dunn.test")
  library("dunn.test")
}
if (!require("ggpubr")) {
  install.packages("ggpubr")
  library("ggpubr")
}
if (!require("FSA")) {
  install.packages("FSA")
  library("FSA")
}
if (!require("argparse")) {
  install.packages("argparse")
  library("argparse")
}
if (!require("ggplot2")) {
  install.packages("ggplot2")
  library("ggplot2")
}})
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

dir.create(output_folder)
setwd(output_folder)
openFolder<-function(folder_path){
  if(!dir.exists(folder_path))
  {
    dir.create(folder_path) 
  }
  setwd(folder_path)
}
groups_comparisons<-function(groups){
  res<-list()
  for( i in 1:length(groups)){
    for(j in 1:length(groups)){
      if(i<j){
        res<-append(res,paste(groups[i],"~",groups[j],sep=""))
      }
    }}
  return(res)
}
TPV<-function(TukeyResult,groups){
  pvl<-c()
  pvl<-c(TukeyResult[])
  
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

mainFolder<-function(){
  setwd(output_folder)
}
openFolderFromMain<-function(folder_path){
  mainFolder()
  openFolder(folder_path)
}

data<-read.csv2(file=file_path)
print(paste("odczytano dane z ",file_path,sep=""))
numeric_columns<-sapply(data,is.numeric)
numeric_column_names<-colnames(data[,numeric_columns])
groups<-unique(data[,1])
openFolder(output_folder)
##


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
print("uzupelnianie brakow")
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
write.csv(data,"dane z uzupelnionymi brakami.csv",row.names = FALSE)
print("statystyki ogolne:")

tmptext<-paste(numeric_column_names,sep=";")
text<-c(paste("parametr","min","max","srednia","mediana","IQR","wariancja","odchylenie_standardowe",sep=";"))
for(column in numeric_column_names){
  png(filename=paste(column,"_boxplot.png",sep=""),width=800,height=600)
  boxplot(data[,column],main=column)
  dev.off()
  text<-c(text,paste(column,range(data[,column],na.rm=TRUE)[1],range(data[,column],na.rm=TRUE)[2],mean(data[,column],na.rm=TRUE),median(data[,column],na.rm=TRUE),IQR(data[,column],na.rm=TRUE),var(data[,column],na.rm=TRUE),sd(data[,column],na.rm=TRUE),sep=";"))
}
print("utworzono wykresy boxplot dla kazdej zmiennej numerycznej")
  writeLines(text,"statystyki_ogolne_zmienych_numerycznych.csv")
  text<-c()
  
  for(column in names(data)[!sapply(data,is.numeric)]){
    text<-c(text,paste("zmienna:",column),capture.output(table(data[,column])))
  }

  writeLines(text,"statystyki_ogolne_zmiennych_nienumerycznych.txt")
  #writeLines(capture.output(summary(data[,names(data)[!sapply(data,is.numeric)]])),"statystyki_ogolne_zmiennych_nienumerycznych.txt")



#dane odstajace
openFolderFromMain("statystyki_ogolne")
text<-c(paste("zmienna","dane_odstajace",sep=";"))
for(column in numeric_column_names){
  print(paste("dane odstajace w kolumnie ",column,":"))
  tmp<-boxplot(data[,column],plot=FALSE)
  print(tmp$out)
  text<-c(text,paste(column,tmp$out,sep=";"))
}
writeLines(text,"dane odstajace.csv")

##charakterystyki grup
openFolderFromMain("statystyki_z_podzialem_na_grupy")

text<-c(paste("grupa","parametr","min","max","srednia","mediana","IQR","wariancja","odchylenie_standardowe"))
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

print("utworzono pliki ze statystykami z podziałem na grupy")









### shapiro test
print("analiza porownawcza:")
openFolderFromMain("analiza_porownawcza_miedzy_grupami")
print("sprawdzenie zgodnosci z rozkladem normalnym i jednorodnosci wariancji:")
text<-c("")
cnnnttt<-0
for(column in numeric_column_names){
  good_for_aov=TRUE
  pvalueShapiroTestHSCRP <- group_by(data, data[,1]) %>%
    summarise(
      p.value = shapiro.test(data[,column])$p.value
    )
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
    break
      }else{
      cat("\ndla ",column," p-value = ", pvalueShapiroTestHSCRP$p.value[i], "> 0.05 - można założyć zgodność z rozkładem normalnym\n")
      print(paste("ocena jednorodnosci wariancji dla: ",column))
      leveneTestResult<-leveneTest(data[,column]~data[,1],data=data)
      if(leveneTestResult$"Pr(>F)"[1]<0.05){
        good_for_aov<-FALSE
        cat("brak jednorodnosci wariancji\n")
        break
      }else{
        cat("jest jednorodnosc wariancji\n")
        
      }
      
    }
  }
  if(good_for_aov){
    AOV_result<-aov(data[,column]~data[,1],data=data)
    print("przeprowadzono test ANOVA")
    
    AOV_p_value<-summary(aov(data[,column]~data[,1],data=data))[[1]][["Pr(>F)"]][[1]]
    if(AOV_p_value<0.05){
      cat("znaleziono istotne statystycznie roznice\n")
      TukeyResult<-TukeyHSD(AOV_result)
      #tpvvv<-TPV(TukeyResult,groups)
      length(TukeyResult)
      print(TukeyResult)
      tmpp<-TukeyResult$data[, 4]
      
      #writeLines(capture.output(print(TukeyResult)),paste(column,"_ANOV_istotne_statystycznie_roznice_miedzy_grupami.csv"))
      writeLines(capture.output(print(TukeyResult)),paste(column,"_ANOV_istotne_statystycznie_roznice_miedzy_grupami.txt"))
      cnnnttt<-cnnnttt+1
      print(str(TukeyResult))
      xxx<-groups_comparisons(groups)
      tekst_z_grupami<-paste(paste(groups_comparisons(groups),"p-value = ",TukeyResult$data[,4]),"\n",collapse="")
      print(tekst_z_grupami)
      p<-ggplot(data,aes_string(x=names(data)[1],y=column))+
        geom_boxplot()+
        
        labs(title=paste("wykres pudelkowy dla \n",column,"\n ",tekst_z_grupami,sep=""),
             x=names(data)[1],
             y=column)+theme(plot.title = element_text(size = 10))
      
      print(p)
      ggsave(paste("wykres_pudełkowy_dla",column,"z podzialem_na_grupy.png"),plot = p,width=6,height=8,units="in")
      
      
    }else{
      cat("brak istotnych statystycznie roznic miedzy grupami\n")
    }
  }else{
    Kruskal_result<-kruskal.test(data[,column]~data[,1],data=data)
    print("przeprowadzono test Kruskala-Willisa")
    Kruskal_p_value<-Kruskal_result$p.value
    if(Kruskal_p_value<0.05){
      cat("znaleziono istotne statystycznie roznice")
      dunnTestResult<-dunnTest(data[,column],data[,1])
      tekst_z_grupami<-paste(paste(groups_comparisons(groups),"p-value =",dunnTestResult$res$P.adj),"\n",collapse="")
      #writeLines(capture.output(print(dunnTestResult)),paste(column,"Kruskal_istotne_statystycznie_roznice_miedzy_grupami.csv"))
      writeLines(capture.output(print(dunnTestResult)),paste(column,"Kruskal_istotne_statystycznie_roznice_miedzy_grupami.txt"))
      cnnnttt<-cnnnttt+1
      p<-ggplot(data,aes_string(x=names(data)[1],y=column))+
        geom_boxplot()+
        
        labs(title=paste("wykres pudelkowy dla \n",column,"\n ",tekst_z_grupami,sep=""),
             x=names(data)[1],
             y=column)+theme(plot.title = element_text(size = 10))
      
      print(p)
      ggsave(paste("wykres_pudełkowy_dla",column,"z podzialem_na_grupy.png"),plot = p,width=6,height=8,units="in")
      
    }else{
      cat("brak istotnych statystycznie roznic miedzy grupami")
    }
  }
}


####korelacja
print("analiza zaleznosci miedzy zmiennymi")
plots<-list()
openFolderFromMain("wyniki_testow_korelacji")
text<-c(paste("grupa","zmienna1","zmienna2","wspolczynnik_korelacji","metoda","p-value","ocena_wspolczynnika_korelacji",sep=";"))
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
          
          print(paste("w grupie",group,"dla parametrow",numeric_column_names[i],"i",numeric_column_names[j],"mozna zalozyc zgodnosc z rozkladnem normalnym i przeprowadzono test korelacji pearsona",sep=" "))
          CorResult<-cor.test(FirstColumn,SecondColumn,method="pearson")
          
        }else{
          print(paste("w grupie",group,"dla parametrow",numeric_column_names[i],"i",numeric_column_names[j],"nie mozna zalozyc zgodnosci z reozkladem normalnym i przeprowadzono test korelacji spearmana",sep=" "))
          CorResult<-cor.test(FirstColumn,SecondColumn,method="spearman")
          method="spearman"
          
        }
        if(CorResult$p.value<0.05)
        {
          text<-c(text,paste(group,numeric_column_names[i],numeric_column_names[j],CorResult$estimate,method,CorResult$p.value,corCoefInter(CorResult$estimate),sep=";"))
          print(paste("istnieje korelacja; wspolczynnik = ",CorResult$estimate))
          #png(filename = paste("wykres_korealcji",numeric_column_names[i],"~",numeric_column_names[j],"w grupie",group,".png",sep="_"),width=800,height=600)
          
          gg <- data[which(data[,1] == group), ]
          p <- ggscatter(gg, x = numeric_column_names[i], y = numeric_column_names[j], 
                         add = "reg.line", conf.int = TRUE, stat.cor=TRUE,
                         cor.coef = TRUE, cor.method = method,
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

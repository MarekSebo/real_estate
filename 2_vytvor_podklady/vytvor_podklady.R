#DOCASNY KOD. NEPOUZIVAT
#mozno zle nacitava lat a long
#chcem vyhodit stlpec REGION


#tabulka koeficientov modelu
#pre kazdu lokalitu je tam hodnota vsetkych regresnych koef, iba koef lokality je tam iba jeden
#potom: predikcia je nasobenie vektorov

#deklaracie
hranicny_koeficient_modelu="Garsonka1"
dlzka_MUNI_ID=6
dlzka_STREET_ID=9
cutoff_quantile_uliceBA=0.015
cutoff_quantile_muniSR=0.01
smoothing_prah_pocty_uliceBA=20
smoothing_prah_pocty_muniSR=60
smoothing_R_uliceBA=0.75
smoothing_R_muniSR=25
smoothing_vaha_uliceBA=0.4
smoothing_vaha_muniSR=0.4
k_knn=10 #pocet k v KNN doplnani

#nacitavanie
directory<-"C://Users//Lenovo//Disk Google//Machine Learning//DTLN//integracia//2_vytvor_podklady"
print(paste("terajsia directory je:'", directory, sep=" ", "'ak treba, prepiste ju v kode (deklaracia premennych)"))
setwd(directory)  #getwd()
mLOG<-readRDS(file="mLOG.rds")
mLOGmuni<-readRDS(file="mLOGmuni.rds")
dm<-readRDS(file="dm.rds" )
dmmuni<-readRDS(file="dmmuni.rds" )
names(dmmuni)<-replace(names(dmmuni), names(dmmuni)%in% c("MUNICIPALITY","Xlok","municipalityId","MUNICIPALITY_ID"),"MUNI_ID" )

#ciselniky:
muniSRread<<-read.csv2(file="zoznam_muni.csv", header = T)
names(muniSRread)<-c("MUNI_ID","MUNI","DISTRICT_ID","REGION_ID","long","lat")
uliceBAread<<-read.csv2(file="zoznam_ulic.csv", header=T)
names(uliceBAread)<-c("STREET","MUNI_ID","STREET_ID","long","lat")
dmmuni$MUNI_ID<-as.integer(as.character(dmmuni$MUNI_ID))
REGPJ<-readRDS(file="REGPJ.rds")
dmstreets<-dm[which(nchar(as.character(dm$Xlok))==9),]
#install
require("mgcv","kknn")

#ANALYZA DUPLICIT a kompatibility
#nejake duplikaty boli vymazane
dupli_muni<-which(duplicated(muniSRread$MUNI_ID) )
dupli_streets<- which(duplicated(uliceBAread$STREET_ID) )
if(!is.null(dupli_muni)) 
  muniSR<<-muniSRread[setdiff(c(1:nrow(muniSRread)),dupli_muni),] 
if(!is.null(dupli_streets))
  uliceBA<<-uliceBAread[setdiff(c(1:nrow(uliceBAread)),dupli_streets),] 
if(nrow(muniSR)!=nrow(muniSRread) | nrow(uliceBA)!=nrow(uliceBAread)){
  print("tieto ID su tam viackrat:")
  duplicitne_muni<<-muniSRread[dupli_muni,]
  duplicitne_streets<<-uliceBAread[dupli_streets,]
  print(duplicitne_muni)
  print(duplicitne_streets)
  print(paste("duplicitne ID malo tolkoto ulic:",length(dupli_streets),
    "a tolkoto muni:", length(dupli_muni),sep=" "))
  print("dataframes pred deduplikaciou su ulozene ako muniSRread, uliceBAread.")
  print("duplicitne muni a streets s celymi riadkami su ulozene ako duplicitne_muni a duplicitne_streets")
} else print("ziadne duplicitne Xlok_ID v uliciach aj muni")

#ktore MUNI_ID z dm nie su v muniSR?
dm_muni_nie_v_muniSR<-unique(dmmuni$MUNI_ID[which(!(dmmuni$MUNI_ID %in% as.integer(muniSR$MUNI_ID) ))]) 
if(!is.null(dm_muni_nie_v_muniSR)){
print("tieto MUNI co boli v datach z roku 2015 roku nie su v ciselniku! (a teda ich nevieme ocenit.)")
print(unique(REGPJ[which(REGPJ$MUNICIPALITY_ID %in%dm_muni_nie_v_muniSR),c("MUNICIPALITY_ID","MUNICIPALITY_NAME")]))
} else
  print("vsetky muni z dmmuni su v muniSR")
#ktore STREETS z dm nie su v uliceBA?
dmstreets_nie_v_uliceBA<-unique(dmstreets$STREET_ID[which(!(dmstreets$STREET_ID %in% as.integer(uliceBA$STREET_ID) ))]) 
if(!is.null(dmstreets_nie_v_uliceBA)){
print("tieto STREETs co boli v datach z roku 2015 roku nie su v ciselniku! (a teda ich nevieme ocenit.)")
print(unique(uliceBA[which(uliceBA$STREET_ID %in%dmstreets_nie_v_uliceBA),c("STREET_ID","STREET")]))} else
  print("vsetky streets z dmstreets su v uliceBA")
#KONIEC ANALYZY


#pridaj cenovu uroven
muniSR<-data.frame(muniSR[,c("MUNI_ID","MUNI","DISTRICT_ID","REGION_ID","lat","long")], pocet=rep(NA, nrow(muniSR)), koef=rep(NA, nrow(muniSR)), koef_exp=rep(NA, nrow(muniSR)))
uliceBA<-data.frame(uliceBA[,c("STREET","MUNI_ID","STREET_ID","lat","long")], pocet=rep(NA, nrow(uliceBA)), koef=rep(NA, nrow(uliceBA)), koef_exp=rep(NA, nrow(uliceBA)))
lokality_model<-mLOG$coefficients[1:(which(names(mLOG$coefficients)==hranicny_koeficient_modelu)-1)]
names(lokality_model)<-gsub("Xlok","",names(lokality_model) )
ulice_model<-lokality_model[which(nchar(names(lokality_model))==dlzka_STREET_ID)]
muni_model<-lokality_model[which(nchar(names(lokality_model))==dlzka_MUNI_ID)]
muniSR$koef=muni_model[match(as.character(muniSR$MUNI_ID), names(muni_model) )]
uliceBA$koef=ulice_model[match(as.character(uliceBA$STREET_ID),names(ulice_model) )]
#exp() ceny
muniSR$koef_exp<-exp(muniSR$koef)
uliceBA$koef_exp<-exp(uliceBA$koef)

#formatovanie, na factors a numerics
muniSR$lat<-as.numeric(as.character(muniSR$lat))
muniSR$long<-as.numeric(as.character(muniSR$long))
uliceBA$lat<-as.numeric(as.character(uliceBA$lat))
uliceBA$long<-as.numeric(as.character(uliceBA$long))
muniSR$MUNI_ID<-as.character(muniSR$MUNI_ID)
uliceBA$STREET_ID<-as.character(uliceBA$STREET_ID)
muniSR$lat<-as.numeric(as.character(muniSR$lat))
muniSR$long<-as.numeric(as.character(muniSR$long))
uliceBA$lat<-as.numeric(as.character(uliceBA$lat))
uliceBA$long<-as.numeric(as.character(uliceBA$long)) 


#pridaj pocty
for (i in 1:length(muniSR$MUNI_ID)){
  muniSR$pocet[i]=length(which(dmmuni$MUNI_ID==as.character(muniSR$MUNI_ID[i])) )
}
for (i in 1:length(uliceBA$STREET_ID)){
  uliceBA$pocet[i]=length(which(dmstreets$Xlok==uliceBA$STREET_ID[i]))
}
print(paste("zapis poctov v lokalitach: z trenovacich dat o pocte: poc_muni=",nrow(dmmuni)," poct_ulice=",nrow(dmstreets), 
  "sme do tabuliek NEzapisali tolkoto: poc_nez_muniSR=", nrow(dmmuni)-sum(muniSR$pocet)," poc_nez_uliceBA=",
  nrow(dmstreets)-sum(uliceBA$pocet)))


#smoothing BA
print("smothing uliceBA")
smoothed_exp<-smoothing(urovne=uliceBA$koef_exp, pocet=uliceBA$pocet, lat=uliceBA$lat, 
  long=uliceBA$long, cutoff_min=quantile(uliceBA$koef_exp, cutoff_quantile_uliceBA, na.rm=T), 
  cutoff_max=quantile(uliceBA$koef_exp, 1-cutoff_quantile_uliceBA, na.rm=T), 
  prahpocty=smoothing_prah_pocty_uliceBA, R=smoothing_R_uliceBA, vaha=smoothing_vaha_uliceBA )
uliceBA<-data.frame(uliceBA[,setdiff(names(uliceBA),"smoothed_exp") ], smoothed_exp)

#smoothing MUNI
print("smothing muniSR")
smoothed_exp<-smoothing(urovne=muniSR$koef_exp, pocet=muniSR$pocet, lat=muniSR$lat, 
  long=muniSR$long, cutoff_min=quantile(muniSR$koef_exp,cutoff_quantile_muniSR, na.rm=T), 
  cutoff_max=quantile(muniSR$koef_exp, 1-cutoff_quantile_muniSR , na.rm=T), 
  prahpocty=smoothing_prah_pocty_muniSR, R=smoothing_R_muniSR, vaha=smoothing_vaha_muniSR)
muniSR<-data.frame(muniSR[,setdiff(names(muniSR),"smoothed_exp") ], smoothed_exp)


##
#
#

#KNN doplnanie NA koef lokalit
koef_final<-log(doplnanie_na_lokalit(data=muniSR, k_knn))
muniSR<-data.frame(muniSR[,setdiff(names(muniSR), "koef_final")], koef_final)
koef_final<-log(doplnanie_na_lokalit(data=uliceBA, k_knn) ) 
uliceBA<-data.frame(uliceBA[,setdiff(names(uliceBA), "koef_final")], koef_final) 

#posledne upravy
muniSR$MUNI_ID<-as.integer(as.character(muniSR$MUNI_ID))
uliceBA$MUNI_ID<-as.integer(as.character(uliceBA$MUNI_ID))
uliceBA$STREET_ID<-as.integer(as.character(uliceBA$STREET_ID))

#transformacia tabuliek
print("muniSR zacala transformacia")
muniSRfinal<-data.frame(muniSR,vytvor_tabulku_koefi(mLOGmuni,muniSR,dmmuni) )
print("uliceBA zacala transformacia")
uliceBAfinal<-data.frame(uliceBA, vytvor_tabulku_koefi(mLOG,uliceBA,dm) )


####uloz files
#directory
directory_output<-paste(directory,"//outputs",sep="")
if(!dir.exists(directory_output))
  dir.create(directory_output, showWarnings = TRUE, recursive = FALSE)
setwd(directory_output)
#ulozTO
saveRDS(muniSRfinal,file="muniSRfinal.rds")
saveRDS(uliceBAfinal,file="uliceBAfinal.rds")
print(paste("konec! vystupy boli zapisane do priecinka", directory_output,sep=" "))
print("TODO:1) kontrola ci mame ulozene vsetko. pre validaciu este uloz komplet validset ked bude (vsetky byty za januar) 
  2) mozno tvorba tabuliek a smoothing checknut")
setwd(directory)


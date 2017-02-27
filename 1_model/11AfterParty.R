#AfterParty!!

#NASTAV parameter
logaritmus<-T;

#zmen podla toho aky model
if(logaritmus){
  dm<-zdmLOG; fit<-mLOG;
}else{
  dm<-zdm;fit<-klasik;
}
dv<-zdv;

#datasety pouzite v kode: 
#dv=validacny (bude upravovany),  validset=IFP validacny , dm=cely (bude upravovany)
#dmap=ulice v BA, sumarne stats z trenovacieho datasetu,  lokality=ulice v BA, sumarne stats z IFP datasetu

if(logaritmus){
  dm<-data.frame(EST=exp(fit$fitted.values), res=fit$residuals,perc_res=fit$residuals/dm$dPSQ ,dm[,setdiff(names(dm),c("EST","res"))])
}else{
  dm<-data.frame(EST=fit$fitted.values, res=fit$residuals,perc_res=fit$residuals/dm$dPSQ ,dm[,setdiff(names(dm),c("EST","res"))])
}
#zmena labels
levels(dv$REGION)<-c("BA","TT","TN","NR","ZA","BB","PO","KE");
if("PSQ" %in% names(dv))
  names(dv)[match("PSQ",names(dv))]<-"dPSQ"

#nacitaj valid data IFP
vzorka<-read.table( "C://Users//Lenovo//Disk Google//Data Mining//diplomka files//source//final data//vzorkaFINAL.csv" ,header=TRUE,sep=";");
names(vzorka)[c(1,2)]<-c("IDZAK","EST_PSQ");
vzorka[,"IDZAK"]=as.character(vzorka[,"IDZAK"]);
vzorka[,"EST_PSQ"]=as.numeric(vzorka[,"EST_PSQ"]) ;
vzorka<-vzorka[which(!is.na(vzorka$EST_PSQ)),];


#odstranovanie chybajucich dat
#TU POZOR ked davam iny dataset ako zdm
dv<-dv[,c("IDZAK",setdiff(names(dm),c("res","EST","perc_res")) )];#tu ma byt zdm
#odstran municipalities ktore neboli v povodnych datach
dv<-dv[dv$Xlok %in% levels(dm$Xlok),] # 4 idu prec
dv$Xlok<-as.factor(as.character(dv$Xlok));


#pridavanie statistik (bez IFP statistik)
data<-dv; #ked data=dm tak su problemy s pamatou :(
if(nrow(data)==nrow(dm) )
{
  if(logaritmus==F){
  data<-data.frame(EST=fit$fitted.values, res=fit$residuals, 
                   perc_res=fit$residuals/data$dPSQ*100 ,data[,setdiff(names(data),c("EST","res","perc_res"))])
  }else{
    data<-data.frame(EST=exp(fit$fitted.values), res=exp(fit$residuals), 
                     perc_res=exp(fit$residuals)/data$dPSQ*100 ,data[,setdiff(names(data),c("EST","res","perc_res"))])
  }
  
}else{
if(logaritmus==T) {
  data<-data.frame(EST=exp(predict(fit,newdata = data)), res=exp(predict(fit,newdata = data))-data$dPSQ, 
                 perc_res=(exp(as.numeric(predict(fit,newdata = data)) )-data$dPSQ)/data$dPSQ*100 ,data[,setdiff(names(data),c("EST","res","perc_res"))])
}else  {
  data<-data.frame(EST=predict(fit,newdata = data), res=predict(fit,newdata = data)-data$dPSQ,perc_res=(predict(fit,newdata = data)-data$dPSQ)/data$dPSQ ,data[,setdiff(names(data),c("EST","res","perc_res"))])}
}
unikaty<-unique(data$Xlok)
abs_lok_per<-rep(NA,nrow(data));
mRE_lok<-rep(NA,nrow(data));
pocty<-rep(NA,nrow(data));
for(i in 1:length(unikaty))
{
  ktore<-which(data$Xlok==unikaty[i]); #ktore patria do danej lokality
  ktory<-match(unikaty[i],data$Xlok ); #jeden z nich
  abs_lok_per[ktory]<-mean(abs(data$perc_res[ktore]),na.rm=T);
  mRE_lok[ktory]<-median(abs(data$perc_res[ktore]),na.rm=T);
  pocty[ktory]<-length(ktore)
}
data<-data.frame(abs_lok_per,mRE_lok,pocty, data[,setdiff(names(data),c("pocty","abs_lok_per","mRE_lok"))])
rm(abs_lok_per,mRE_lok,pocty, ktore,ktory)
dv<-data;


validset<-dv[match(vzorka[,1], dv$IDZAK),] #294 
validset<-validset[!is.na(validset$IDZAK),]#11 sa stratilo z toho (NA)

#nechaj iba tie co su aj vo vzorka aj vo validset
zle<-union(vzorka$IDZAK[which(!vzorka$IDZAK %in% validset$IDZAK )],vzorka$IDZAK[which(!validset$IDZAK %in%vzorka$IDZAK)]  );
validset<-validset[match(setdiff(validset$IDZAK ,zle ), validset$IDZAK),];
#zapis do R suboru
ulozma=validset[,names(dm)]
saveRDS(ulozma, file="validset.rds")

vzorka<-vzorka[match(setdiff(vzorka$IDZAK ,zle ), vzorka$IDZAK),];
validset<-data.frame(IFP_EST=vzorka$EST_PSQ,validset)

#validacia NAS MODEL
if(logaritmus==T) {
  validset<-data.frame(EST=exp(predict(fit, newdata=validset[,names(dm)][setdiff(names(validset[,names(dm)]),"dPSQ")])),validset)} else{ 
    validset<-data.frame(EST=predict(fit, newdata=validset[,names(dm)][setdiff(names(validset[,names(dm)]),"dPSQ")]),validset)
  }


#pridavanie statistik IFP style (sumarne pre lokality)
data<-validset;
#jednotkou je percentualna odchylka modelu od reality
deltaMY<-(data$EST-data$dPSQ)/data$dPSQ*100
deltaIFP<-(data$IFP_EST-data$dPSQ)/data$dPSQ*100
resMY<-data$EST-data$dPSQ
resIFP<-data$IFP_EST-data$dPSQ
data<-data.frame(deltaMY,deltaIFP,resMY,resIFP,data)
unikaty<-unique(data$Xlok)
pocty<-rep(NA,nrow(data))
score<-rep(NA,nrow(data))
sumabsMY<-rep(NA,nrow(data))
sumabsIFP<-rep(NA,nrow(data))
sumsqMY<-rep(NA,nrow(data))
sumsqIFP<-rep(NA,nrow(data))
sumabsdiff<-rep(NA,nrow(data))
sumsqdiff<-rep(NA,nrow(data))
for (i in 1:length(unikaty))
{  
  #ktory ma danu lokalitu
  ktory=match(unikaty[i],data$Xlok)
  #kolko ich je
  pocty[ktory]<-length(which(data$Xlok==unikaty[i]));
  #u kolkych z toho je nas model presnejsi
  score[ktory]<-length(which(data$Xlok==unikaty[i] & abs(data$deltaMY)<=abs(data$deltaIFP)));
  #sumarne statistiky modelov pre danu lokalitu (Nielen dotycny byt!)
  sumabsMY[ktory]<-sum(abs(data$deltaMY[which(data$Xlok==unikaty[i])]));
  sumsqMY[ktory]<-sum( (data$deltaMY[which(data$Xlok==unikaty[i])])^2 )
  sumabsIFP[ktory]<-sum(abs(data$deltaIFP[which(data$Xlok==unikaty[i])]));
  sumsqIFP[ktory]<-sum( (data$deltaIFP[which(data$Xlok==unikaty[i])])^2 );
  avgabsdiff<-(sumabsIFP-sumabsMY)/pocty
  avgsqdiff<-(sumsqIFP-sumsqMY)/pocty
}
avgabsdifftotal<-sum((sumabsIFP-sumabsMY)*pocty,na.rm=T)/nrow(data)
avgsqdifftotal<-sqrt(sum(sumsqIFP-sumsqMY,na.rm = T)/nrow(data))
print("oproti IFP sme lepsi o tolkoto percent v nasledujucich kriteriach: priemerna abs, sqrt(squared odchylka):")
print(c(avgabsdifftotal,avgsqdifftotal) )

validset<-data.frame(pocty,score,sumabsMY,sumabsIFP,sumsqMY,sumsqIFP,sumabsMY,sumabsIFP,data)
validset<-validset[,unique(names(validset))]
lokality<-validset[!duplicated(validset$Xlok),]
print("nas je lepsi alebo rovny v tolkoto percentach z validsetu s tolkymito bytmi")
print(c(sum(score,na.rm = T)/nrow(data)*100,as.integer(nrow(data)) ) )


#rezidua 
if(logaritmus){ 
  hist( (dm$dPSQ-exp(fit$fitted.values))[which( (dm$dPSQ-exp(fit$fitted.values)>-500) & (dm$dPSQ-exp(fit$fitted.values)<500) )] ,nclass=100)
}else{ 
  hist( (dm$dPSQ-fit$fitted.values)[which( (dm$dPSQ-fit$fitted.values>-500) & (dm$dPSQ-fit$fitted.values<500) )] ,nclass=100)
}


######
#porovnanie podla lokalit
vlokalite<-rep("IFP",nrow(lokality))
vlokalite[which((lokality$score/lokality$pocty)>0.5)]="MY"
vlokalite[which((lokality$score/lokality$pocty)==0.5)]="remiza"
lokality<-data.frame(vlokalite,lokality)
print("porovnanie v lokalitach: vitazimeMY:remiza:IFP")
print(c(length(which(vlokalite=="MY")),length(which(vlokalite=="remiza")),length(which(vlokalite=="IFP"))  ) )
rm(pocty,score,sumabsMY,sumabsIFP,sumsqMY,sumsqIFP,deltaMY,deltaIFP,data,vlokalite)

#Ako sme dopadli v Kosiciach? [dost dobre]
ktoreKE<-which(lokality$Xlok %in% REGPJ$MUNICIPALITY_ID[which(as.integer(as.character(REGPJ$DISTRICT_ID)) %in% c(802:805) )])
print("porovnanie v lokalitach: vitazimeMY:remiza:IFP")
lokKE<-lokality$vlokalite[ktoreKE]
print("vysledky v KE:")
print(c(length(which(lokKE=="MY")),length(which(lokKE=="remiza")),length(which(lokKE=="IFP"))  ) )

#Bratislava
ktoreBA<-which(nchar(as.character(lokality$Xlok))==9 )
#spravne by tam este mali byt tie co nemaju ulicu a spadaju iba pod MUNI:
#|(lokality$Xlok %in% REGPJ$MUNICIPALITY_ID[which(as.integer(as.character(REGPJ$DISTRICT_ID)) %in% c(101:105) )]))
lokBA<-lokality$vlokalite[ktoreBA]
print("vysledky v BA:")
print(c(length(which(lokBA=="MY")),length(which(lokBA=="remiza")),length(which(lokBA=="IFP"))  ) )


#sumarna analytika
#sqrt(MSE) percentualnych odchyliek
MSEIFP<-sqrt(sum((validset$deltaIFP)^2)/nrow(validset));
MSEMY<-sqrt(sum((validset$deltaMY)^2)/nrow(validset));
print("sqrt(MSE) percentualnych odchyliek: IFP, my");
print(c(MSEIFP,MSEMY));
avgabsIFP<-sum(abs(validset$deltaIFP))/nrow(validset);
avgabsMY<-sum(abs(validset$deltaMY))/nrow(validset);
mREIFP<-median(abs(validset$deltaIFP) )
mREMY<-median(abs(validset$deltaMY) )
mREdiff<-median(abs(validset$deltaMY- validset$deltaIFP) )
if(!logaritmus){
avgabsDV<-sum(abs(dv$perc_res))/nrow(dv);
avgabsTOTAL<-sum(abs(dm$perc_res))/nrow(dm)*100
}
print("avg z abs hodnot percentualnych odchyliek: IFP, my, my na celom validsete, my na celom datasete");
print(c(avgabsIFP,avgabsMY,avgabsDV,avgabsTOTAL ))
print(paste("medianova percentualna odchylka (mRE) IFP= ",mREIFP," mRE nasho=", mREMY))
print(paste("medianovy rozdiel perc odchylok v prospech nasho modelu v % =", mREdiff))

#total odchylka (jednotka=dPSQ)
#sqrt(MSE) IFP
MSEIFPtot<-sqrt(sum((validset$IFP_EST-validset$dPSQ)^2)/nrow(validset));
#sqrt(MSE) MY
MSEMYtot<-sqrt(sum((validset$EST-validset$dPSQ)^2)/nrow(validset));
if(logaritmus==T){
  MSEMYtotalvalidtot<-sqrt(sum((exp(predict(fit,newdata=dv[setdiff(names(dv),"dPSQ")]))-dv$dPSQ)^2)/nrow(dv));
  MSEMYtotaltot<-sqrt(sum((exp(fit$fitted.values)-dm$dPSQ)^2)/nrow(dm));
} else{  
  MSEMYtotalvalidtot<-sqrt(sum((predict(fit,newdata=dv[setdiff(names(dv),"dPSQ")])-dv$dPSQ)^2)/nrow(dv));
  MSEMYtotaltot<-sqrt(sum((fit$fitted.values-dm$dPSQ)^2)/nrow(dm));
}  
print("sqrt(MSE): IFP, my, my na celom validsete, my na celom datasete");
print(c(MSEIFPtot,MSEMYtot,MSEMYtotalvalidtot, MSEMYtotaltot))

#priemerne nadhodnotenie
print(paste("na validsete IFP nadhodnocuje o",mean(validset$deltaIFP),"  percenta. my nadhodnocujeme o ", 
            mean(validset$deltaMY),"percenta") )


#####
#####
#MAPA
#nacitaj GPS suradnice
GPS<-read.csv("C://Users//Lenovo//Disk Google//Data Mining//source//final data//ciselniky//ulicecsv.csv", header=TRUE, 
              sep=";", colClasses=c("integer", "double","double"));
names(GPS)<-c("ID_STREET","lat","long");

#MAPA vsetkych BA lokalit
dmap<-dm[which(nchar(as.character(dm$Xlok))==9 ),]; 
unikaty<-unique(dmap$Xlok);
pocty<-rep(NA, nrow(dmap));
abs_odch<-rep(NA, nrow(dmap));
abs_perc_odch<-rep(NA, nrow(dmap));
mRE_lok<-rep(NA, nrow(dmap));
for (i in 1: length(unikaty))
{
  pocty[match(unikaty[i],dmap$Xlok)]<-length(which(dmap$Xlok==unikaty[i]));
  ktore<-which(dmap$Xlok==unikaty[i])
  ktore1<-match(unikaty[i],dmap$Xlok)
  abs_odch[ktore1]<-sum(abs(dmap$dPSQ[ktore]-dmap$EST[ktore]))/pocty[ktore1];
  abs_perc_odch[ktore1]<-sum(abs(dmap$dPSQ[ktore]-dmap$EST[ktore])/dmap$dPSQ[ktore])/pocty[ktore1]*100;
  mRE_lok[ktore1]<-median(abs(dmap$dPSQ[ktore]-dmap$EST[ktore])/dmap$dPSQ[ktore])*100;
}
dmap<-data.frame(mRE_lok,abs_odch, abs_perc_odch,pocty,dmap);
dmap<-dmap[which(!is.na(dmap$pocty)),]
#BetaLOK pre tie byty
betalok<-fit$coefficients[paste("Xlok",as.character(dmap$Xlok),sep="" )]

#GPS<-GPS[which(GPS$ID_STREET %in% dmap$Xlok),];
GPSsur<-GPS[match(dmap$Xlok,GPS$ID_STREET), c("lat","long")] ;
#nacitanie dlzok zatial nefunguje (treba IDs)
#dlzky<-ulicedlzky[match(dmap$STREET,ulicedlzky$ID_STREET), "Length"] ;
#tu sa da vyhodit nejake ulice podla ich dlzky (data frame "ulicedlzky")
if(logaritmus){ 
  dmap<-data.frame(betalok=exp(betalok), dmap,GPSsur)}else{
    dmap<-data.frame(betalok, dmap,GPSsur)}
dmap<-dmap[setdiff(c(1:nrow(dmap)),which(is.na(dmap$lat)) ),]

#vs IFP(lokality)
#vyber iba tie co maju v Xlok ulicu. toto nas pripravuje o tie co maju v BA iba municipalitu....
lokality<-lokality[which(nchar(as.character(lokality$Xlok))==9 ),] ; #88
GPSsur<-GPS[match(lokality$Xlok,GPS$ID_STREET), c("lat","long")];
lokality<-data.frame(lokality,GPSsur);
lokality<-lokality[setdiff(c(1:nrow(lokality)),which(is.na(lokality$lat))),]
rm(GPSsur);

library(ggplot2) 
#heatmap 
cu<-read.csv(file ="C://Users//Lenovo//Disk Google//Data Mining//cenovauroven.csv", header=T, sep = ","  )
qplot(long, lat, data=cu, colour = cenova_uroven_3i) +   scale_colour_gradientn(colours=rainbow(5))
#qplot(long, lat, data=dmap, colour = betalok) +   scale_colour_gradientn(colours=rainbow(5)) #odstranit extremne hodnoty
#score vs IFP
qplot(long, lat, data=lokality, colour = vlokalite) 
#mRE nasho mod
qplot(long,lat,data=dmap[which(dmap$mRE_lok<25),], colour=mRE_lok)  +   scale_colour_gradientn(colours=rainbow(5))
hist(dmap$mRE_lok,nclass=50)
#ggplot(dmap[,c("long","lat","abs_perc_odch")], aes(x, y, z = z))+ geom_tile(aes(fill = z)) + stat_contour() 


hist(dmap$abs_perc_odch,nclass=30) 
print("medianova odchylka lokality v BA (ulice) na trenovacich datach") 
print(median(dmap$abs_perc_odch));



#SMOOTHED HEATMAP
smoothing<-function(urovne,pocty,lat,long) #zatial hardcoded okrajove ohranicenia aj konstanty smoothingu na ceny ba
{
  #nastav hranicne hodnoty. to co ich presiahne sa zarovna
  if(logaritmus){ 
  betalokmin=1400; betalokmax=4500;
}else{
  betalokmin=1200; betalokmax=4500;
}
  prahpocty<-10 #max pocet bytov na ulici aby sme ju zahrnuli do smoothingu
  R<-0.5 # radius
  vaha<-0.4 #kolko percent ceny bude ovplyvnenych ostatnymi cenami
  
#vyhod outliers
hist(urovne,nclass=min(100,length(urovne) ))
  urovne[which(urovne>betalokmax)]=betalokmax; 
  urovne[which(urovne<betalokmin)]=betalokmin; #pozor konstanty!
  print(paste("v smoothingu boli cenove urovne mimo [",betalokmin," , ",betalokmax,"] zarovnane na okraje intervalu") )
  
#smoothing
hist(urovne,nclass=100)
start<-proc.time()
n=length(pocty)
smoothed<-urovne
for(j in 1:n){
  if(pocty[j]>prahpocty)
    next
  sumvahy=0; sumcena=0; dlat=lat[j]; dlong=long[j];
  for(i in setdiff(1:n,j)){
    d=max(0,0.5-sqrt(((lat[i]-dlat)*110)^2+((long[i]-dlong)*75 )^2) ) #konverzia na kilometre
    #podla formule na Wiki pre 45lat (*110),17long (*75)
    sumvahy=sumvahy+d*sqrt(pocty[i])
    sumcena=sumcena+d*sqrt(pocty[i])*urovne[i]
  }
  smoothed[j]=(1-vaha)*urovne[j]+vaha*sumcena/sumvahy;
}
print(proc.time()-start)
smoothed[which(is.na(smoothed))]=urovne[which(is.na(smoothed))]; #spinavy docasny krok na NAcka
hist(smoothed,nclass=min(length(urovne),100) )
return(smoothed)
} 
betaloksmooth<-smoothing(urovne=dmap$betalok,pocty=dmap$pocty,lat=dmap$lat,long=dmap$long )

#nahod smoothed do modelu
smoothmLOG<-mLOG
smoothmLOG$coefficients[paste("Xlok",as.character(dmap$Xlok),sep="")] <- log(betaloksmooth)

qplot(long, lat, data=dmap, colour = betaloksmooth) +   scale_colour_gradientn(colours=rainbow(5)) #smoothed map





#KONIEC

#d[match("529338067",d$Xlok),"STREET"]

#CROSS (nefunguje lebo new levels of Xlok...)
#library(DAAG); 
#cv<-cv.lm(data=dm, fit, m=3) # 3 fold cross-validation

#cas? (zatial na provizornych datach)
reg="TT"
cas<-lm(PSQ ~ AGE_SELL, data=dPoNAodvazne[dPoNAodvazne$REGION==reg,]) 
print(paste("v",reg,"za rok 2015 narastli ceny bytov o",round(-100*cas$coefficients[2]*365/cas$coefficients[1],digits =3), "percent",sep=" " ))

#interpretacia modelu
print("jednotkovy narast danej veliciny ma za nasledok tolkoto percent narast ceny ")
print( round((exp(fit$coefficients[(length(fit$coefficients)-26):length(fit$coefficients)])-1)*100,digits=3)  )
#spojite premenne

#predikcia
mena=c("Xlok","Garsonka","PocetIzieb","vlast","park_garaz","vytah","rok","podlazie","stav","konstr",
       "vybav","stary_rek","RozlohaIzby","balkon_rozl","lodzia_rozl","terasa_rozl")
#ulice (zatial iba BA)

dBA<-dPoNAodvazne[which(nchar(as.character(dPoNAodvazne$Xlok))>7),]
#vysvtelivky k BA uliciam
uliceBA<-dBA[match(unique(d$ID_STREET),d$ID_STREET ),c("STREET","ID_STREET")]



#cenova hladina pre Roba (3i socik atd)
zmeny<-sum(fit$coefficients[c("PocetIzieb3","vytah1","roksociknovy","konstrpanel","vybavna")])+70/3*fit$coefficients["RozlohaIzby"] 

cenova_uroven_3i<-dmap$betalok*exp(zmeny)
cenuroven<-data.frame(cenova_uroven_3i,lat=dmap$lat,long=dmap$long,STREET=dPoNAodvazne$STREET[match(dmap$Xlok, dPoNAodvazne$Xlok)])
hist(cenova_uroven_3i, nclass=50)

write.table(cenuroven, file = "C://Users//Lenovo//Disk Google//Data Mining//cenovauroven.csv",
            sep = ",", col.names = NA,qmethod = "double")
print("cenova uroven na 9te narodenininy CMN je ulozena v cenuroven a zapisana do suboru cenovauroven.csv.")
print("je to cenova uroven 3i panel bytu z rokov 1968-1994, medziposchodie, 70 m2, pov stav, vybavenost NA, vytah")

predikcie<-data.frame(SKUTOCNA_CENA=validset$dPSQ, IFP=validset$IFP_EST, MY=validset$EST)
write.table(predikcie, file = "C://Users//Lenovo//Disk Google//Data Mining//predikcie.csv",
            sep = ",", col.names = NA,qmethod = "double")

#porovnanie vs IFP pre Roba
qplot(long, lat, data=lokality, colour = vlokalite) 
write.table(data.frame(pocet=lokality$pocty, scoreMY=lokality$score, KtoVyhral=lokality$vlokalite, STREET=dPoNAodvazne$STREET[match(lokality$Xlok, 
  dPoNAodvazne$Xlok)],lat=lokality$lat,long=lokality$long ), file = "C://Users//Lenovo//Disk Google//Data Mining//vsIFPpreRoba.csv",
            sep = ";", col.names = NA,qmethod = "double")


#SHINY


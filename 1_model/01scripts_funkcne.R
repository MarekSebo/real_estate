#####
#HOTOVE SCRIPTS
#####
#vytvor_tabulku koefi, smoothing, doplnanie_na_lokalit
#REGPJ
#kriteria, kriterialog
#sl, pocetnosti, ukazvztahy
#zhodneHOD, zhodnostCheck
#PriceIndex, popiskykonecne
#######################################

vytvor_tabulku_koefi<-function(model,data, model_data){
#vystup = data.frame s koeficientmi pre jednotlive lokality. mena vo formate ako mena koef modelu
  
#model = z ktoreho modelu chceme tabulku  
#data= ku ktorym lokalitam chceme tabulku koeficientov = ku ktorej tabulke ju potom chceme pripojit
#modeldata= z ktorych dat bol zostrojeny model (dm). cerpame z nich urovne

#!!: vytazi koef iba pre tie urovne, ktore su v model_data
#!!: response v modeli sa musi volat "log(dPSQ)" alebo "dPSQ"
#!!: lokalita sa musi volat "Xlok","MUNI_ID","lok" alebo "MUNI"

  #vsetky urovne premennych v modeli
  premenne<-setdiff(as.character(attr(model$terms, "variables")), c("list","log(dPSQ)", "Xlok",
    "lok","MUNI_ID","MUNI",
    "MUNICIPALITY","REGION","REGION_ID","DISTRICT","DISTRICT_ID","CADASTRE","CADASTRE_ID","dPSQ") )
  nlev=0
  #toto je zle zatial
  for(i in 1:length(premenne))
    nlev=nlev+length(levels(premenne))
  premenne_lvl<-rep(NA,nlev)
  k=0;
  for(i in 1:length(premenne))
    for(j in 1:length(levels(model_data[,premenne[i]])))
    {
      k=k+1
      premenne_lvl[k]=paste(premenne[i], levels(model_data[,premenne[i]])[j],sep="" )
    }
  #vyhodi duplikaty (tie sa vytvaraju pre spojite premenne)
  premenne_lvl<-unique(premenne_lvl)
  #identifikacia spojitych premennych
  cont<-which(premenne_lvl %in% names(model_data))

  #vytvor df s koeficientmi
  koefi<-as.data.frame(matrix(0L,nrow(data),length(premenne_lvl)))
  names(koefi)<-premenne_lvl
  #vsetky su z daneho modelu (muni je muni model a )
  hodnoty<-replace(model$coefficients[premenne_lvl],which(is.na(model$coefficients[premenne_lvl])),0)
  if(ncol(koefi)!=length(hodnoty) )
    return(print(paste("ERROR: nerovnake rozmery koeficientov modelu a hodnot ktore chcem zapisat do tabulky:",
            ncol(koefi),length(hodnoty),sep=" ")))
  if(length(which(is.na(as.numeric(hodnoty))))>0)
    return(print("ERROR: niektory z koeficientov modelu, ktory chcem zapisat do tabulky, je NA"))
  #skuska spravnosti
  koef_ine<-names(mLOG$coefficients[-c(1:(which(
    names(mLOG$coefficients)==hranicny_koeficient_modelu)-1) )])
  if(length( which(!(koef_ine %in% names(hodnoty) ) ) )>0  )
    return(print(paste("ERROR: niektore z nenulovych koeficientov modelu neboli prepisane 
      do tabulky:", koef_ine[which(!(koef_ine %in% hodnoty) )]   ),sep=" "))
  for (i in 1:nrow(koefi))
    koefi[i,]<-hodnoty
  print("vytvor_tabulku_koefi: uspesne prebehla transformacia tabulky na tvar pre pricing")
  return(koefi)
}


#SMOOTHING: vstupy-> smoothed urovne (koefcienty)
smoothing<-function(urovne,pocet,lat,long, cutoff_min, cutoff_max, prahpocty, R, vaha) #zatial hardcoded okrajove ohranicenia aj konstanty smoothingu na ceny ba
{
  #pocet = pocet nehnutelnosti v jednotlivych lokalitach
  #cutoff_min, cutoff_max = hranice. outlieri mimo nich budu zarovnani na hranice.  
  #prahpoc = max pocet bytov v lokalite, aby sme ju zahrnuli do smoothingu
  #R = radius do ktoreho v smoothingu zohladnujeme okolite ceny
  #vaha = kolko percent ceny bude ovplyvnenych ostatnymi cenami
  
  start<-proc.time()
  #odsekni outlierov
  ktore_min=which(urovne<cutoff_min)
  ktore_max=which(urovne>cutoff_max)
  urovne[ktore_min]==cutoff_min
  urovne[ktore_max]=cutoff_max
  print(paste("v smoothingu boli cenove urovne mimo [",cutoff_min," , ",cutoff_max,
    "] zarovnane na okraje intervalu. Upravili sme tolkoto hodnot", length(c(ktore_min,ktore_max))) )
  
  #smoothing
  start<-proc.time()
  index=which(!is.na( urovne ))
  n=length(index)
  smoothed<-urovne
  for(k in 1:n){
    j=index[k] 
    if(pocet[j]>prahpocty)
      next
    sumvahy=0; sumcena=0; dlat=lat[j]; dlong=long[j];
    for(l in setdiff(1:n,j)){
      i=index[l]
      #konverzia na kilometre pre 45lat (*110),17long (*75)
      d=sqrt(((lat[i]-dlat)*110)^2+((long[i]-dlong)*75 )^2) 
      if(d>R)
        next
      sumvahy=sumvahy+(R-d)*sqrt(pocet[i])
      sumcena=sumcena+(R-d)*sqrt(pocet[i])*urovne[i]
    }
  smoothed[j]=(1-vaha)*urovne[j]+vaha*sumcena/sumvahy;
  }
  print(paste("prebehol smoothing s parametrami: cutoff_min=",cutoff_min,"cutoff_max=",
    cutoff_max,"prahpocty=",prahpocty,"R=",R,"vaha=",vaha, sep=" "))
print(paste("smoothing trval:",proc.time()-start,sep=" ") )
smoothed[which(is.na(smoothed))]=urovne[which(is.na(smoothed))];
kolko_smoothed<-length(which(abs(smoothed-urovne)>0))-length(c(ktore_min,ktore_max)) 
print(paste("smoothing sa aplikoval na",kolko_smoothed,"entit v datasete, co zodpoveda"
  ,kolko_smoothed/length(which(!is.na(urovne)))*100, "% z nonNA hodnot", sep=" " ))
print("sqrtMS smoothingu je zatial neznama (doplnit? :) )")
return(smoothed)
}

#doplnanie chybajucich koeficientov pomocou KNN
doplnanie_na_lokalit<-function(data, k){
  #doplni chybajuce hodnoty "smoothed_exp" pomocou "lat" a "long"
  #KNN regresia, L2 norma
  #pars:    k=pocet pars
  ktore<-which(is.na(data$smoothed_exp))
  output<-data$smoothed_exp
  library(kknn)
  knn<-kknn(formula=smoothed_exp~lat+long, train=data, test = data[ktore, ],na.action=na.omit(),distance=2, k=k) 
  output[ktore]<-knn$fitted.values
  print(paste("funkcia doplnanie_na_lokalit doplnila tolkoto koeficientov:",length(ktore)))
  print(paste("KNN s k=",k))
  if(length(which(is.na(output))!=0))
    print(paste("Pozor! Este stale nebolo doplnenych tolkoto 
      NA cenovych urovni:",length(which(is.na(output)) ),sep=" ") )
  return(output)
}


#REGPJ csv uzemne clenenie -> RDS file
register_ulic<-function()
{
  #uzemne clenenie
  REGPJ<-read.csv("REGPJ.csv", header=TRUE, sep=";",
    colClasses=c("character","character","character","integer","character","integer","character","character","character",
      "integer","character","integer","character"));
  REGPJ<-REGPJ[,c(4,5,6,8,10,11,12,13)];
  REGPJ<-REGPJ[which(!duplicated(REGPJ)) ,]
  names(REGPJ)<-c("REGION_ID","REGION_NAME","DISTRICT_ID","DISTRICT_NAME","MUNICIPALITY_ID","MUNICIPALITY_NAME","CADASTRE_ID","CADASTRE_NAME")
  #for(i in 1:length(REGPJ[1,]))
  #  REGPJ[,i]<-as.character(REGPJ[,i]);
  return(REGPJ) 
} 


#rychle vyhodnotenie modelu na trenovacich datach
kriteria<-function(model,data){
RSS=sum((model$fit-data$dPSQ)^2);
TSS=sum((data$dPSQ-mean(data$dPSQ) )^2);
ESS=sum((model$fit-mean(data$dPSQ)  )^2)
n=nrow(data)
k=length(model$coef)
Radj=(1-(RSS/(n-k))/(TSS/(n-1)))*100
MSE=RSS/n;
MAE=mean(abs(data$dPSQ-model$fit))
mRE=median(abs((model$fit-data$dPSQ)/data$dPSQ ) )*100
MRE=mean(abs((exp(model$fit)-data$dPSQ)/data$dPSQ ) )*100
AIC=log(RSS/n)-2*k/n;
print(paste("n=",n," k=",k,"  ESS/TSS=",ESS/TSS*100," sqrt(MSE)=", sqrt(MSE)," MAE=",MAE," MRE=",MRE," mRE=",mRE, " Radj= ",Radj," AIC= ",AIC))
}

#log model a ine data=> zmeneny script na kriteria
kriterialog<-function(model,data){
n=nrow(data)
k=length(model$coef)
RSS=sum((exp(model$fit)-data$dPSQ)^2);
TSS=n*var(data$dPSQ);
ESS=sum( (exp(model$fit)-mean(data$dPSQ)  )^2)
Radj=(1-(RSS/(n-k))/(TSS/(n-1)))*100
MSE=RSS/n;
MAE=mean(abs(data$dPSQ-(exp(model$fit))))
MRE=mean(abs((exp(model$fit)-data$dPSQ)/data$dPSQ ) )*100
mRE=median(abs((exp(model$fit)-data$dPSQ)/data$dPSQ ) )*100
AIC=log(RSS/n)-2*k/n;
print(paste("n=",n," k=",k,"  ESS/TSS=",ESS/TSS*100," sqrt(MSE)=", sqrt(MSE)," MAE=",MAE," mRE=",mRE," MRE=",MRE, " Radj= ",Radj," AIC= ",AIC))
}

#PRICE INDEX, vystupom je novy dataset ktory obsahuje dPSQ
PriceIndex<-function(data,kraj)
{
  d<-data[which(data$REGION==as.character(kraj)),];
  
  #script funguje iba pre cisla kvartalov=cisla riadkov!!
  popiskyPI<-data.frame(quartal=c(1,2,3,4,5), ZacDat=as.Date(c("2015-01-01","2015-04-01","2015-07-01","2015-10-01","2016-01-01")),
                          KonDat=as.Date(c("2015-03-31","2015-06-30","2015-09-30","2015-12-31","2099-03-31")), 
                          zaciatok=c(365,274,182,91,0), koniec=c(275,183,92,1,-1000), pocet=rep(NA,5), 
                        P=rep(NA,5), delta=rep(NA,5), diskont=rep(NA,5));
  qmax=max(popiskyPI$quartal); 
  if(qmax!=length(popiskyPI$quartal))
  {return(print("script funguje iba pre cisla kvartalov=cisla riadkov!!"));}
 
  #do ktoreho kvartalu patria jednotlive merania?
  Q=rep(NA,nrow(d));
  for (i in 1:length(popiskyPI$koniec))
  {
    ktore=which(d$AGE_SELL<=popiskyPI$zaciatok[i] & d$AGE_SELL>=popiskyPI$koniec[i]);
    kv=popiskyPI$quartal[i];
    
    Q[ktore]=kv;
    popiskyPI$pocet[i]<-length(ktore); #zapis do popiskov
    popiskyPI$P[i]<-mean(d$PSQ[ktore]);
  }
  
  #vytvor zmeny
  for(i in 2:(qmax-1))
  {
    popiskyPI$delta[i]=(popiskyPI$P[i]-popiskyPI$P[i-1])/popiskyPI$P[i-1];
  }
  #krajne hodnoty delty=priemer casoveho radu
  popiskyPI$delta[1]=mean(popiskyPI$delta[2:(qmax-1)]);
  popiskyPI$delta[qmax]=mean(popiskyPI$delta[2:(qmax-1)]);
  #zo zmien nanasob diskonty
  for(i in 1:(qmax-1))
  {
    sucin=1;
    for (j in c((i+1):qmax))
    {
      sucin=sucin*(1+popiskyPI$delta[j]);
    }
    popiskyPI$diskont[i]=sucin;
  }
  popiskyPI$diskont[qmax]=1
  
  rm(ktore,kv,Q)
  print(paste("kraj",kraj) )
  return(popiskyPI)
}

PItabulka<-function(data){
#prvy kraj (vytvorenie PI tabulky)
kraj=levels(data$REGION)[1]
PI<-PriceIndex(data,kraj)
PI<-rbind(data.frame(REGION=rep(kraj,nrow(PI)), PI)) 
#ostatne kraje (dokoncenie tabulky)
for (j in 2:length(levels(data$REGION)))
{
  kraj=levels(data$REGION)[j]
  PInovy<-PriceIndex(data,kraj)
  PI<-rbind(PI, data.frame(REGION=rep(kraj,nrow(PInovy)), PInovy)) 
}
return(PI)
}

#zapis PI do dat
zapisPI<-function(data, PI)
{
Q=rep(NA,nrow(data));
diskont=rep(NA,nrow(data));

for (i in 1:length(popiskyPI$koniec))
{
  #prirad kvartal
  ktore=which(data$AGE_SELL<=PI$zaciatok[i] & data$AGE_SELL>=PI$koniec[i]);
  kv=popiskyPI$quartal[i];
  Q[ktore]=kv;
  
  #prirad diskont
  for (j in 1:8)
  {
    kraj=levels(data$REGION)[j]
    ktore2<-which(Q==kv & data$REGION==kraj)
    diskont[ktore2]=PI$diskont[which(PI$REGION==kraj & PI$quartal==kv)]
  }
}
dPSQ=data$PSQ*diskont;
if(length(which(is.na(dPSQ)))>0 | length(which(is.na(Q)))>0)
  print("nejake kvartaly alebo diskonty neboli priradene! vznikli NAcka!")
return(data.frame(dPSQ,Q,diskont,data))
}

#imitacia IFP modelu
imitIFP<-function(){
  d<-dPredObohatenim;
  dBez<-d
  vyhod<-rep(FALSE,length(d$PSQ));
  vyhod[which(d$IDZAK %in% zovalidv$IDZAK)]=TRUE;
  dBez<-d[!vyhod,]
  d<-dBez
  d<-data.frame(Xlok=rep(NA,length(d$PSQ)),d )
  #BA
  muniBA<-REGPJBA$MUNICIPALITY_ID;
  d$Xlok[which(d$MUNICIPALITY %in%muniBA)]=as.character(d$ID_STREET[which(d$MUNICIPALITY %in%muniBA)]);
  d$Xlok[which( !(d$MUNICIPALITY %in%muniBA))]=as.character(d$MUNICIPALITY[which( !(d$MUNICIPALITY %in%muniBA) )]);
  d$Xlok[which(d$MUNICIPALITY %in%muniBA & is.na(d$ID_STREET))]=as.character(d$MUNICIPALITY[which(d$MUNICIPALITY %in%muniBA & is.na(d$ID_STREET))]);
  #KE
  muniKE<-REGPJ$MUNICIPALITY_ID[which(as.integer(as.character(REGPJ$DISTRICT_ID)) %in% c(802:805) )]
  d$Xlok[which(d$MUNICIPALITY %in%muniKE)]=as.character(d$ID_STREET[which(d$MUNICIPALITY %in%muniKE)]);
  d$Xlok[which(d$MUNICIPALITY %in%muniKE & is.na(d$ID_STREET))]=as.character(d$MUNICIPALITY[which(d$MUNICIPALITY %in%muniKE & is.na(d$ID_STREET))]);
  d$Xlok<-as.factor(d$Xlok);
  
  d<-d[,c("PSQ","Xlok","AREA","id214","id215","id216","id116","id125","id212","id223")];
  levels(d$id212)[5:6]<-NA
  vyhod<-rep(FALSE,length(d$PSQ));
  for(i in 1:ncol(d))
    vyhod[which(is.na(d[,i]) )]=TRUE;
  print(paste("vyhodime",sum(vyhod),"bytov",sep=" "))
  d<-d[!vyhod,]
  #preskup faktory
  d$Xlok<-as.factor(as.character(d$Xlok))
  #hodnoty 2 premen na 0
  d$id116<-(as.integer(replace(as.character(d$id116),as.character(d$id116)=="2","0") ) );
  d$id214<-(as.integer(replace(as.character(d$id214),as.character(d$id214)=="2","0") ) );
  d$id215<-(as.integer(replace(as.character(d$id215),as.character(d$id215)=="2","0") ) );
  d$id216<-(as.integer(replace(as.character(d$id216),as.character(d$id216)=="2","0") ) );
  d$id116<-as.factor(d$id116)
  d$id214<-as.factor(d$id214)
  d$id215<-as.factor(d$id215)
  d$id216<-as.factor(d$id216)
  #premenuj
  levels(d$id223)<-c("panel","tehla","zmies");
  levels(d$id212)<-c("pov_stav","ciast_rek","kompl_rek","novo");
  levels(d$id125)<-c("prizem","ine","podstr");
  names(d)[1]<-"dPSQ"
  names(d)[4:10]<-c("balkon","lodzia","terasa","vytah","podlazie","stav","konstr")
}

#summary PSQ podla levels daneho faktora
sl<-function(name)
{
  plot(d$PSQ ~(as.factor(d[,as.character(name)])));
  print(tapply(d$PSQ, as.factor(d[,as.character(name)]), summary));
  print(summary(as.factor(d[,as.character(name)])));
}

velka_summary<-function(data)#fa
{
  faktory<-names(data[,which(sapply(data,is.factor))]);
  integery<-names(data[,which(sapply(data,is.integer))]);
  numerics<-names(data[,which(sapply(data,is.numeric))]);
  charaktery<-names(data[,which(sapply(data,is.character))]);
  if(length(unique(c(faktory,integery,numerics,charaktery)))<length(data[1,]))
    {print("tolkoto premennych ma iny data typ ako faktor, num, int, char");
     print( length(data[1,]) - length(unique(c(faktory,integery,numerics,charaktery))));
  }
  for (i in 1:length(faktory))
  {
     print(faktory[i]);
     print(summary(data[,faktory[i]],maxsum=12));
  }
  apply(data[,charaktery],2,print(summary));
  apply(data[,integery],2,print(summary)  );
  apply(data[,numerics],2,print(summary)  );
  rm(faktory,integery,numerics,charaktery);
}


#POCETNOSTI = vytvori data frame rozmeru popisky*2 s abs a rel pocetnostami premennych v data. 
#ked premenna nie je v data, da NA.
pocetnosti<-function(name_abs,name_rel,data,popis)
{
  for(j in 2:5)
    popis[,j]<-as.character(popis[,j])
  dlzka=length(data[,1]);
  poc_rel<-rep(NA,length(popis$char_numberz) );
  poc_abs<-rep(NA,length(popis$char_numberz) );
  for(i in 1:length(popis$char_numberz) )
  {
    premenna=as.character(popis$char_names[i]);
    if(! (premenna %in% names(data)) )
    {poc_abs[i]=NA;}
    else
    {poc_abs[i]<-length(which(!is.na(data[,as.character(popis$char_names[i])])));}
    
    poc_rel<-round(poc_abs/dlzka*100, digits=3);
  }  
  vystup<-data.frame(poc_abs,poc_rel);  
  #vystup<-vystup[-which(is.na(vystup[,poc_rel])),]
  names(vystup)<-c(as.character(name_abs),as.character(name_rel));
  rm(premenna, name_rel, name_abs, poc_rel, poc_abs,dlzka)
  return (vystup)
}

#register_ulic<-function()
#{
#  #uzemne clenenie
#  REGPJ<-read.csv("C://Users//Lenovo//Disk Google//Machine Learning//DTLN//integracia//data//REGPJ2015csv.csv", header=TRUE, sep=";");
#  REGPJ<-REGPJ[,c(4,5,6,8,10,11,12,13)];
#  REGPJ<-REGPJ[which(!duplicated(REGPJ)) ,]
#  names(REGPJ)<-c("REGION_ID","REGION_NAME","DISTRICT_ID","DISTRICT_NAME","MUNICIPALITY_ID","MUNICIPALITY_NAME","CADASTRE_ID","CADASTRE_NAME")
#  for(i in 1:length(REGPJ[1,]))
#    REGPJ[,i]<-as.character(REGPJ[,i]);
#  return(REGPJ) 
#} 

## NACITAVANIE IDciek ULIC namiesto NA , implicitne z tabulky dat
doplnIDulic<-function(data)
{
  data[,"STREET"]<-factor(data[,"STREET"])
  
  lok<-data[,c("REGION",  "DISTRICT" , "MUNICIPALITY" , "CADASTRE", "ID_STREET","STREET")];
  #cisla tych ulic co ne/maju ID
  lokNA<-which(!is.na(lok$STREET) & is.na(lok$ID_STREET));
  lokNNA<-which(!is.na(lok$STREET) & !is.na(lok$ID_STREET)); 
  
  lok$MUNICIPALITY<-as.factor(lok$MUNICIPALITY); 
  lok$STREET<-as.factor(lok$STREET);
  
  #cela tabulka tych co nemaju ID
  lokNApole<-lok[lokNA,]; 
  
  cas<-proc.time()
  for(i in 1:length(lokNNA))
  {
    vektor=lok[lokNNA[i],c("MUNICIPALITY", "STREET")]; #spravny vektor STREET, MUNICIPALITY
    lokNApole$ID_STREET[which(c(lokNApole$MUNICIPALITY,lokNApole$STREET)==vektor)]=lok$ID_STREET[lokNNA[i]];
  }
  lok[lokNA,]<-lokNApole;
  
  print(proc.time()-cas);
  print("algoritmus doplnil namiesto NA tolkoto ID_STREET, cely novy vektor ID_STREET je vo vystupe");
  print(length(which(is.na(d$ID_STREET)))-length(which(is.na(lok$ID_STREET))));
  rm(lokNA,lokNNA,cas)
  return(lok$ID_STREET)
}
###

#nahodny vyber z hodnot danych premennych z riadkov kde aspon jedna z danych premennych ma nieNA hodnotu
ukaz_vztahy<-function(premenne,kolko,data)
{
  nena=rep(FALSE,length(data[,1]) );
  for(j in 1:length(premenne))
  {
    meno=as.character(premenne[j]);
    nena[which(!is.na(data[,meno]) )]=TRUE;
  }
  return( data[which(nena)[sample.int(length(which(nena)),kolko )],as.character(premenne)] )
}

##ZHODNOST DUPLICITNYCH RIADKOV
zhodneHOD<-function(data,prvychX, var) 
  #data=data frame ,  prvychX=kolko prvych riadkov chcem hladat v celom datasete, var=premenna v ktorej hladam zhody
{
  data1<-as.character(data[, which(names(data)==as.character(var))] )
  zhodne<-rep(NULL,prvychX);
  zhody<-0;
  for(i in 1:prvychX)
  {
    ajdi=data1[i];
    if(length(which( data1==ajdi) )>1 )
    {
      zhody=zhody+1;
      zhodne[zhody]=data1[i];
    } 
  }
  return(zhodne[c(1:zhody)])
  
}

#KONTROLA ZHODNOSTI
#pozor na $o.IDZAK }meno stlpca!
zhodnost_check<-function(data,zhodneID,k)
{
  if(length(zhodneID)<k)
    ajdi=zhodneID
  else
    ajdi=zhodneID[sample(1:length(zhodneID),k)]
  nerovnake<-rep(NA,length(ajdi));
  nnerovnake<-0;
  for(i in 1:length(ajdi))
  {
    riadky<-data[as.character(data$o.IDZAK)==ajdi[i], ]
    dlzka<-length(riadky[,1])
    if(dlzka<2) {print("dlzka<2 ! pre "); print(i);}
    for(j in 1:(dlzka-1) ) 
    {
      riadok=riadky[j,];
      if(riadok!=riadky[j+1,])
      {
        nnerovnake=nnerovnake+1;
        nerovnake[nnerovnake]<-ajdi[i]
        break;
      }
    }
  }
  if(nnerovnake==0) 
  {print("ziadne nerovnake riadky pre dane ID")}
  else 
  {
    print("nerovnake riadky pre ID"); 
    print(which(!is.na(nerovnake)) );
  }
  return;
}
popiskykonecne<-function()
{
  popisky<-data.frame(popisky[,1:11], pocetnosti("Po_NACKUJ_abs","Po_NACKUJ_rel",d) );
  popisky<-popisky[match(setdiff(as.character(popisky$char_names),
                               setdiff(as.character(popisky$char_names), names_current)), popisky$char_names),];
  popiskyKonecne<-popisky[,c(1,4,5,7,9,11,13)];
  names(popiskyKonecne)<-c("vlastnos è.","popis","jednotka","poè. pôvodné dáta", "poè. po obohateni","poè. po èistení", "poè. po NA")
  popiskyKonecne[,3]<-as.character(popiskyKonecne[,3]);
  popiskyKonecne[,2]<-as.character(popiskyKonecne[,2]);
  #### umely krok!
  popiskyKonecne[6,3]="eur/mesiac"; # :)
  return();
}

cistenie_light<-function(data)
{
  d<-data;
  
  N<-length(d$ID2)
  vyhod<-rep(FALSE,length(d[,1]));
  #niektore budeme nahradzat NAckami namiesto vyhadzovania
  vyh<-0;
  k<-0;
  
  vyhod[which((d$val230>0.7*d$AREA) | (d$val230<0))]=TRUE #setting hradieb
  k<-length(which(vyhod==TRUE))-vyh;
  vyh<-vyh+k;
  print("rozloha balkona (val230) vyhodi");
  print(k);
  
  #boxplot(val245);
  vyhod[which((d$val245>0.7*d$AREA) | (d$val245<0))]=TRUE #setting hradieb
  k<-length(which(vyhod==TRUE))-vyh;
  vyh<-vyh+k;
  print("rozloha loggie val 245 vyhodi");
  print(k);
  
  #boxplot(val248);
  vyhod[which((d$val248>2*d$AREA) | (d$val248<0))]=TRUE #setting hradieb
  k<-length(which(vyhod==TRUE))-vyh;
  vyh<-vyh+k;
  print("rozloha terasy val248 vyhodi");
  print(k);
  
  #hodnoty 2 premen na 0
  d$id116<-(as.integer(replace(as.character(d$id116),as.character(d$id116)=="2","0") ) );
  d$id214<-(as.integer(replace(as.character(d$id214),as.character(d$id214)=="2","0") ) );
  d$id215<-(as.integer(replace(as.character(d$id215),as.character(d$id215)=="2","0") ) );
  d$id216<-(as.integer(replace(as.character(d$id216),as.character(d$id216)=="2","0") ) );
  
  #balkonovanie
  #balkony s nulovou rozlohou nahradime ziadnymi balkonmi
  d$id214[which(d$val230==0 ) ]=0; #332
  d$id215[which(d$val245==0 ) ]=0; #365
  d$id214[which(d$val248==0 ) ]=0; #179
  #balkony s chybajucou (NA) rozlohou nahradime percentilom 25 percent
  d$val230[which(d$id214==1 & is.na(d$val230) ) ]=as.numeric(quantile(d$val230, na.rm=TRUE)[2] ); #10517
  d$val245[which(d$id215==1 & is.na(d$val245) ) ]=as.numeric(quantile(d$val245, na.rm=TRUE)[2] ); #7293
  d$val248[which(d$id216==1 & is.na(d$val248) ) ]=as.numeric(quantile(d$val248, na.rm=TRUE)[2]); #1295
  #nema balkon a NA rozloha => 0 rozloha
  d$val230[which(d$id214==0 & is.na(d$val230) ) ]=0; #?
  d$val245[which(d$id215==0 & is.na(d$val245) ) ]=0; #?
  d$val248[which(d$id216==0 & is.na(d$val248) ) ]=0; #?
  #rozloha je uvedena, tak ma balkon
  d$id214[which(d$val230>0) ]=1;
  d$id215[which(d$val245>0) ]=1;
  d$id216[which(d$val248>0) ]=1;
  #este stale NA => 0
  d$id214[which( is.na(d$id214)) ]=0;
  d$val230[which(is.na(d$val230) ) ]=0;
  d$id215[which(is.na(d$id215)) ]=0;
  d$val245[which(is.na(d$val245)) ]=0;
  d$id216[which(is.na(d$id216)) ]=0;
  d$val248[which(is.na(d$val248)) ]=0;
  
  #Vyhodenie riadkov urcenych na vyhodenie
  print("vyhodime tolkoto riadkov:");
  print(sum(vyhod));
  
  #vyhadzujeme
  nechavame=which(vyhod==FALSE);
  d<-d[nechavame,names(d)];
}



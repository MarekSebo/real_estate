transf<-function(realestateTypeId,offerTypeId, streetId, municipalityId, streetNumber, area, 
  objectStatusId, buildingApprovalEndYear, realised, hasBalcony, hasTerrace, hasLoggia, hasElevator)
{
  #transf: transformuje input z CMN do formatu vhodneho do modelu. 
  #typ_modelu<-function()
  
  #povinne polia: realestateTypeId,offerTypeId, streetId, municipalityId
  
  #default rozlohy (nemame z CMN rozlohy)
  balkon_rozl_default=0.042; #mean 0.046 median 0.036
  terasa_rozl_default=0.170; #mean 0.208 median 0.116
  lodzia_rozl_default=0.055; #mean 0.063 median 0.046
  
  #NA ak to nie je cislo
  STREET_ID=suppressWarnings(as.integer(as.character(streetId)))
  MUNI_ID=suppressWarnings(as.integer(as.character(municipalityId)))

  #typy bytov - Garsonka, PocetIzieb, RozlohaIzby
  # zjednotit s kodom v 07cisti.R
  realestateTypeId<-as.integer(as.character(realestateTypeId));
  if(!(realestateTypeId %in% c(11,12,13,14,15,16,122) )){
    print(paste("nespravne zadany typ nehnutelnosti:", realestateTypeId, 
      ". predpokladam dvojizbovy byt",sep=" "))
    realestateTypeId=13
  }
  Garsonka<-"0";
  Garsonka[which(as.character(realestateTypeId)=="11")]="1";
  Garsonka[which(as.character(realestateTypeId)=="122")]="1";
  PocetIzieb<-as.character(realestateTypeId);
  PocetIzieb<-replace(PocetIzieb, PocetIzieb=="11" | PocetIzieb=="12", 1);
  PocetIzieb<-replace(PocetIzieb, PocetIzieb=="13" | PocetIzieb=="122", 2);
  PocetIzieb<-replace(PocetIzieb, PocetIzieb=="14", 3);
  PocetIzieb<-replace(PocetIzieb, PocetIzieb=="15", 4);
  PocetIzieb<-replace(PocetIzieb, PocetIzieb=="16", 5);
  RozlohaIzby=as.numeric(area/as.integer(PocetIzieb));
  if(is.na(RozlohaIzby)) {
    print(paste("Rozloha izby je NA! Dosadil som 70 m2. Vstup area = ",area,sep=" "))
    RozlohaIzby=70  
    }
  PocetIzieb<-as.character(PocetIzieb);
  
  #vlastnictvo (nie je medzi inputmi)
  vlast="osobne" 
  
  #parkovanie garaz
  park_garaz="0"
  
  #vytah
  vytah="0"
  vytah[which(as.logical(hasElevator))]="1"
  
  #rok
  rok=as.integer(as.character(buildingApprovalEndYear)) 
  if(!(rok %in% c(1,2,3,4,5,6,7,8,9)) )  {
    print(paste("Rok je nespravne zadany:", rok, "predpokladam novy (1994-2006)"),sep=" " )
    rok="novy"
    }
  if(rok %in% c(1,2)) rok="novy"
  if(rok >6) rok="uplnenovy"
  if(rok %in% 3) rok="sociknovy"
  if (rok %in% 4) rok="socikstary"
  if(rok %in% 5) rok="stary"
  
  #stary rek
  #TODO: vyhodit premennu z modelu
  stary_rek="0"; 
  
  #podlazie -> "ine", "podstr", "prizem"
  podlazie="ine"
  
  #stav 
  #08NA.R : d$id212[which(d$id212==5 | d$id212==6)]=3;
  #09NAodvazne.R : levels(d$id212)<-c("pov_stav","ciast_rek","kompl_rek","novo");
  stav=as.integer(objectStatusId)
  stav[which(stav==5 | stav==6)]="kompl_rek"
  stav[which(stav==1)]="pov_stav"
  stav[which(stav==2)]="ciast_rek"
  stav[which(stav==3)]="kompl_rek"
  stav[which(stav==4)]="novo"
  if( !(as.integer(objectStatusId) %in% c(1,2,3,4,5,6) ) ) {
    print(paste("zly format stavu. stav =", stav,"predpokladam povodny stav.", sep=" "))
    stav="pov_stav"
  }
  
  #konstr->"tehla", "panel", "zmies"
  #TODO: Zostroj model bez tohto parametra
  konstr="zmies"
  
  #vybav -> "standard", "na", "zar_spotr"
  #todo vyhod z modelu alebo dopln do formu
  vybav="na"
  
  #balkon_rozl 
  balkon_rozl=0
  terasa_rozl=0
  lodzia_rozl=0
  #ak ma alebo je NA tak dopln default
  if(is.na(hasBalcony) | hasBalcony)  balkon_rozl=balkon_rozl_default
  if(is.na(hasTerrace) | hasTerrace)  terasa_rozl=terasa_rozl_default
  if(is.na(hasLoggia)  | hasLoggia )  lodzia_rozl=lodzia_rozl_default
  
  #Realizovanost (wp)
  wp=0
  if(realised) wp=1 
  
  #wrap up
  output<-data.frame(MUNI_ID,STREET_ID, Garsonka,PocetIzieb,vlast,park_garaz,vytah,rok,podlazie,
    stav,konstr,vybav,stary_rek,RozlohaIzby,balkon_rozl,lodzia_rozl,terasa_rozl,wp)
  return(output)
}

vyber_model<-function(municipalityId, streetId){
  #default je model s ulicami
  ktory_model="uliceBA"
  
  if(is.na(streetId) | !(as.integer(municipalityId) %in% BAmuni) )
    ktory_model="muniSR" 
  
  #dalsie kontroly pre StreetModel
  if(ktory_model=="uliceBA")
  {
    if(is.na(as.integer(streetId))){
      ktory_model<-"muniSR"
      print(paste("Error: zadane streetId nie je cislo. Pokusim sa pouzit muniSR model. zadane streetId =",streetId,sep = " ") )
    }
    #je spravna dlzka streetId (9 cif)?
    if(streetId<1*10^8 || streetId>1*10^9 ){
      ktory_model<-"muniSR"
      print(paste("Error: nespravny format streetId:", streetId, "Pokusim sa pouzit muniSR model",sep = " "))
    }
    if(!(streetId %in% uliceBA$STREET_ID))
    {
      ktory_model="muniSR"
      print(paste("Error: zadane streetId nie je v databaze:", streetId, "Pokusim sa pouzit muniSR model",sep = " "))
    }
    
  }
  if(ktory_model=="muniSR")
  {
    if(is.na(as.integer(municipalityId)))
      return(print(paste("Error: zadane municipalityId nie je cislo",municipalityId) ))
    #je spravna dlzka municipalityId (6 cif)?
    if(municipalityId<1*10^5 || municipalityId>1*10^6 )
      return(paste("Error: nespravny format municipalityId:", municipalityId)) 
        if(!(municipalityId %in% muniSR$MUNI_ID))
    {
      ktory_model="muniSR"
      return(paste("Error: zadane municipalityId nie je v databaze:", municipalityId, "Returned!!!",sep = " "))
    }
  }
  print(paste("pricingAPI>  vyber_model() vybral tento model: ", ktory_model,sep = "") )
  return(ktory_model)
}

vyber_Xlok<-function(ktory_model, municipalityId,streetId)
{
  if(!(ktory_model %in% c("uliceBA","muniSR")) ) 
    return(print(paste("neznamy typ modelu:",ktory_model, sep=" ")))
  if(ktory_model=="uliceBA")
    Xlok<-data.frame(Xlok=streetId)
  if(ktory_model=="muniSR")
    Xlok<-data.frame(Xlok=municipalityId)
  return(Xlok)
}

indexy_kalkulacka<-function(ktory_model, Xlok)
{
  dlzka<-length(Xlok)
  indexy<-rep(NA,dlzka)
  if(ktory_model=="uliceBA")
    indexy<-match(Xlok,uliceBA$STREET_ID)
  if(ktory_model=="muniSR")
    indexy<-match(Xlok,muniSR$MUNI_ID)
  return(indexy)
}

#indexy_v_polygone ---- priamo v polygonAPI.R

  #tato cast sa da zlepsit, pozri vytvor_tabulku_koefi()
  #vytvor nazvy premenna+level
vytvor_nazvy_premenna_a_level<-function(thrash_premenne,spojite_premenne,x){
  levely<-rep(NA,length(kategoricke_premenne))
  for (i in 1:length(kategoricke_premenne))
    levely[i]<-as.character(x[1,kategoricke_premenne[i]]) 
  zlepenec<-paste(kategoricke_premenne,levely,sep="")
  #print(paste("funkcia vytvor_nazvy_premenna... vytvorila tieto nazvy:",sep=" "))
  return(zlepenec)
}

#zrataj f(x)
zrataj_fx<-function(indexy,zlepenec,spojite_premenne,x,data){
  n<-length(indexy)
  kat_beta<-rep(NA,n)
  spoj_beta<-rep(NA,n)
  #print("for i in ....... data[index[i],spojite_premenne]*x[i,spojite_premenne]:")
  for(i in 1:n){
    #print( data[index[i],zlepenec] )
    kat_beta[i]<-sum(data[indexy[i],zlepenec])
    #print(data[index[i],spojite_premenne]*x[1,spojite_premenne])
    spoj_beta[i]<-sum(data[indexy[i],spojite_premenne]*x[1,spojite_premenne])
  }  
  #print("x:")
  #print(x)
  #print(paste("kategoricke koeficienty: ",kat_beta,sep=""))
  #print(paste("spojite_koeficienty*spojite premnenne: ",spoj_beta,sep=""))
  #print(paste("koef lokality: ",data[index,"koef_final"],sep=""))    
  #print("funkcia zrataj_fx vyplula tieto bety: ")
  #print( data.frame(kat_beta,spoj_beta, Xlok_beta=data[index,"koef_final"]) )
  return(data.frame(kat_beta,spoj_beta))
}

#prvy krok ocenovania: vypluje koef pre vsetko okrem lokality
pricing<-function(ktory_model,indexy,x)
{ 
  if(ktory_model=="muniSR")
    data<-muniSR
  if(ktory_model=="uliceBA")
    data<-uliceBA
  
  zlepenec<-vytvor_nazvy_premenna_a_level(thrash_premenne,spojite_premenne,x)
  bety<-zrataj_fx(indexy,zlepenec,spojite_premenne,x,data)
  kat_beta<-bety$kat_beta
  spoj_beta<-bety$spoj_beta
  
  #kategoricke+spojite+Xlok
  total_beta_bez_Xlok<-kat_beta+spoj_beta 
  return(total_beta_bez_Xlok)
}


#VALIDACIA
#########

#vypocitaj_EST(...)
#zrataj odhady z daneho modelu pre validacny set
vypocitaj_EST<-function(ktory_model){
  #nacitaj spravne data k zadanemu modelu
  if(ktory_model=="muniSR")  {data<-muniSR; valid<-valid_muni; stlpec_Xlok<-"MUNI_ID"} else 
    if(ktory_model=="uliceBA") {data<-uliceBA; valid<-valid_streets; stlpec_Xlok<-"STREET_ID"} else
      return("zly format 'ktory_model' ! ")
  
  #vypocitaj odhady pre vsetky riadky daneho validsetu
  EST<-rep(NA,nrow(valid)) 
  #pricing ulic a pricing muni
  for(i in 1:nrow(valid)){
    #v ktorom riadku je koef danej lokality?
    lok<-match(valid$Xlok[i],data[,stlpec_Xlok])
    #scitaj vsetky ostatne koeficienty
    ESTother<-pricing(ktory_model,lok,valid[i,setdiff(names(valid),"dPSQ")])
    ESTlok<-data$koef_final[lok]
    #print(paste("ESTother je",ESTother,"ESTlok je ",ESTlok,sep = " "))
    #vysledny odhad
    EST[i]<-ESTother+ESTlok
  } 
  
  return(exp(EST))  
}


#prehladne vypis vysledky validacie (do suboru?)
#ktory_model moze mat hodnoty "muniSR","uliceBA" alebo "spolu"
vysledky_validacie<-function(ktory_model)
{
  if(ktory_model=="muniSR")  {valid<-valid_muni;} else 
    if(ktory_model=="uliceBA") {valid<-valid_streets;} else
      if(ktory_model=="spolu") {valid<-rbind(valid_streets,valid_muni) } else
        return("zly format 'ktory_model' vo funkcii 'vysledky validacie' ! ")
  if(is.null(valid))
    return("Error: vo vysledky_validacie je 'valid'  prazdny dataset")
  if(!("EST" %in% names(valid)) | !("dPSQ" %in% names(valid)))  
    return("v upravenom validacnom datasete chyba stlpec 'dPSQ' alebo 'EST' ")
  odhad<-valid$EST;
  g_truth<-valid$dPSQ
  rezidua<-odhad-g_truth
  #print(data.frame(odhad,g_truth,rezidua))
  if(length(which(is.na(rezidua)))>0) print("Warovanie: rezidua obsahuju NA. dPSQ alebo EST obsahuju NA!")

  
  print(paste("#####vyhodnotenie modelu:",ktory_model, ":na valid datach o pocte:",nrow(valid),sep=" "))

  perc_res<-rezidua/g_truth*100
  print("percentualne rezidua v absolutnej hodnote maju toto rozdelenie (median je mRE):")
  print(summary(abs(perc_res)))
  print("perc rezidua bez absolutnej hodnoty maju toto rozdelenie:")
  print(summary(perc_res))
  print("nasledujuce metriky su na perc reziduach:")
  RSS<-sum(perc_res^2)
  print(paste("sqrtMSE modelu je:",sqrt(RSS/nrow(valid)), ":MAE je:",mean(abs(perc_res),na.rm=T), sep=" "))
  print("R2 je na absolutnych reziduach:")
  RSSabs<-sum(rezidua^2)
  TSSabs<-sum( (g_truth-mean(g_truth))^2 )
  print(1-RSSabs/TSSabs)
  print(paste("priemerne nadhodnotenie: ", mean(perc_res)))
  print("##KONIEC vyhodnotenia modelu##                                                      ")
  print("                                                                                     ")
} 

#validuj model: summary pre ulice, muni a spolu
#zvol parametre:
#- validset = validacny dataset vo formate ako dm, validset. nacitany bol v init.R
validacia<-function()
{
  #pre porovnanie vyhodnotime aj povodny model vypluty Rkom
  Rmodel<<-readRDS(file="mLOG.rds")
  validset<<-readRDS(file="validset.rds")
  #predikcie modelu od IFP
  IFP<<-as.numeric(as.character(readRDS("IFP_EST.rds")))
  #dm<-readRDS(file="dm.rds")
  #vyhod z validsetu tie co nie su v modeli
  valid_all<-data.frame(Xlok=validset$Xlok, validset[, union("dPSQ",vsetky_premenne[vsetky_premenne %in% names(validset)]) ])
  ### TESTOVACI KROK. VYMAZAT!
  #valid_all<-valid_all[sample.int(250,size=15),]
  #


  ktore_streets<-which(valid_all$Xlok %in% uliceBA$STREET_ID ) 
  ktore_muni<-which(valid_all$Xlok %in% muniSR$MUNI_ID) 
  ktore_nic<-which(!(valid_all$Xlok %in% uliceBA$STREET_ID ) & !(valid_all$Xlok %in% muniSR$MUNI_ID) )
  #hlasenie: ktore boli vyradene z validacie
  if(length(ktore_nic>0)){ 
    print("z validacie boli vyradene tieto uzemne jednotky: ")
    ktore_ee<-as.character(unique(valid_all$Xlok[ktore_nic ]))
    print(ktore_ee)
    print(paste("celkom sa jedna o tolkoto nehnutelnosti: ",length(valid_all$Xlok[ktore_nic])," odporucam doplnit do modelu",sep=" " ))
  } 

  #rozdel na muni a ulice
  valid_muni<<-valid_all[ktore_muni,]
  valid_streets<<-valid_all[ktore_streets,]

  #zrataj odhady z daneho modelu pre validacny set

  #vytvor od
  EST<-vypocitaj_EST("muniSR")
  valid_muni<<-data.frame(EST, valid_muni[,setdiff(names(valid_muni),"EST")])
  EST<-vypocitaj_EST("uliceBA")
  valid_streets<<-data.frame(EST, valid_streets[,setdiff(names(valid_streets),"EST")])

  #prehladne vypis vysledky validacie (do suboru?)
  #ktory_model moze mat hodnoty "muniSR","uliceBA" alebo "spolu"
  
  #directory
  directory_output<-paste(directory,"//outputs",sep="")
  if(!dir.exists(directory_output))
  dir.create(directory_output, showWarnings = TRUE, recursive = FALSE)
  setwd(directory_output)
  print("report bol presmerovany do vysledky_validacie.txt v podpriecinku 'outputs'. Appendnuty na koniec suboru.")
  sink(file="vysledky_validacie.txt",append = T)
  
  print("###########  VALIDACIA AKTUALNEHO MODELU##########")
  print(paste("na validacnom datasete s tolkoto riadkami:",nrow(validset),sep=" "))
  print("                                                  ")
  print("                                                  ")
  vysledky_validacie("muniSR")
  vysledky_validacie("uliceBA")

  #spolu musi byt posledne, lebo ukladame
  vysledky_validacie("spolu") 
  #vsetky platne ulice aj muni vo validsete (tie co su vyhodnocovane v 'spolu' )
  valid_all_valid<-rbind(valid_streets,valid_muni)
  
  #nech nemusim zapisovat globalne premenne (uryvok z vysledky_validacie)
  odhad<-valid_all_valid$EST;
  g_truth<-valid_all_valid$dPSQ
  rezidua<-odhad-g_truth
  perc_res<-rezidua/g_truth*100
  
  validset_s_odhadmi<<-data.frame(EST_Rmodel=exp(predict(Rmodel,valid_all_valid)), rezidua,perc_res,valid_all_valid)
  print("validset s odhadmi a reziduami bol ulozeny do globalnej premennej validset_s_odhadmi")
  print("                                                                                    ")
  print("                                                                                    ")
  print("                                                                                    ")
  print("                                                                                    ")
  sink()
  #vrat povodnu directory
  setwd(directory)
}  
#script{odvazne doplnanie} + script{posledne upravy} + script{popiskuj} + implementacia

#!!!
#!!! PREMENOVANIE PREMENNYCH !!!!

#tu robim aj odbery na projekt VSA (zatial zacommentovane 
#---> dat ako volbu parametra dopln={VSA,mice,...})

#premenovanie a prelevelovanie dat
premenovanie<-function(d, faktory)  {
  staremena<-c("id26", "val35","val36","id116","id125","val202","id212","id213","id214",
               "id215","id216","id223","val230","val245", "val248","id249","id261");
  novemena<-c("vlast","park_garaz","park_park","vytah","podlazie","naklady","stav","rok","balkon",
              "lodzia","terasa","konstr","balkon_rozl","lodzia_rozl","terasa_rozl","zatepl","vybav");
  #PREMENOVAVANIE
  #factor levels
  for(i in 1:length(faktory))
    d[,faktory[i]]<-as.factor(d[,faktory[i]]); 
  levels(d$id261)<-c("standard","zar_spotr","na");
  levels(d$id249)<-c("1","0","na");
  levels(d$id223)<-c("panel","tehla","zmies");
  levels(d$id212)<-c("pov_stav","ciast_rek","kompl_rek","novo");
  levels(d$id125)<-c("prizem","ine","podstr");
  #levels(d$id123)<-c("subez_s_UK");
  #levels(d$id122)<-c("kotol","plyn");
  #levels(d$id121)<-c("ustr_kur","lok_kur");
  levels(d$id26)<-c("osobne","druzst","firem");
  d$PocetIzieb<-as.factor(as.character(d$PocetIzieb));
  
  #premenovanie factor premennych
  names(d)[match(staremena, names(d)) ]<-novemena;
  levels(d$REGION)<-c("BA","TT","TN","NR","ZA","BB","PO","KE");
  
  #rok vystavby. zlucenie levelov
  d$rok<-as.factor(d$rok);
  levels(d$rok)=c("novy","novy","sociknovy","socikstary","stary","uplnenovy","uplnenovy","uplnenovy") ;
  #VSETKY PREMENNE PREMENOVANE (okrem val12,val13 a inych co nepouzivam)
  
  #nastav zakladne levely (pre cenove mapy je to dolezite)
  d<-within(d, vlast <- relevel(vlast, ref = "osobne"))
  d<-within(d, vybav <- relevel(vybav, ref = "standard"))
  d<-within(d, konstr <- relevel(konstr, ref = "tehla"))
  d<-within(d, stav <- relevel(stav, ref = "pov_stav"))
  d<-within(d, PocetIzieb <- relevel(PocetIzieb, ref = "2"))
  
  return(d)
}

NAodvazne<-function(d,names_current,faktory, je_validacny)
{
  #VSA nahradzanie hodnot (zatial je iba zostrojeny model, nie doplnanie)
  #source("C:/Users/Lenovo/Disk Google/Data Mining/r codes/VSA.r") 
  
  
  
  #ZACINA NAHRADZANIE NA
  #balkony s chybajucou (NA) rozlohou nahradime percentilom 25 percent
  d$balkon_rozl[which(d$balkon==1 & is.na(d$balkon_rozl) ) ]=as.numeric(quantile(d$balkon_rozl[d$balkon_rozl>0], na.rm=TRUE)[2] ); #10517
  d$lodzia_rozl[which(d$lodzia==1 & is.na(d$lodzia_rozl) ) ]=as.numeric(quantile(d$lodzia_rozl[d$lodzia_rozl>0], na.rm=TRUE)[2] ); #7293
  d$terasa_rozl[which(d$terasa==1 & is.na(d$terasa_rozl) ) ]=as.numeric(quantile(d$terasa_rozl[d$terasa_rozl>0], na.rm=TRUE)[2]); #1295
  
  #este stale NA => 0
  d$balkon[which( is.na(d$balkon)) ]=0;
  d$balkon_rozl[which(is.na(d$balkon_rozl) ) ]=0;
  d$lodzia[which(is.na(d$lodzia)) ]=0;
  d$lodzia_rozl[which(is.na(d$lodzia_rozl)) ]=0;
  d$terasa[which(is.na(d$terasa)) ]=0;
  d$terasa_rozl[which(is.na(d$terasa_rozl)) ]=0;
  
  #SLUZBY A ENERGIA 202
  #regresia
  #lm(formula = naklady ~ as.factor(stav), data = d) dava super vysledky. zoberiem si z toho ze stav=1 (povodna konstrukcia) ma 2x nizsie 202 naklady
  d$naklady[which( is.na(d$naklady)& d$stav=="pov_stav")]=mean(d$naklady[d$stav=="pov_stav"],na.rm=TRUE); #7 percent
  d$naklady[which( is.na(d$naklady)& d$stav!="pov_stav")]=mean(d$naklady[d$stav!="pov_stav"],na.rm=TRUE); #80 percent
  d$naklady[which( is.na(d$naklady))]=mean(d$naklady,na.rm=T);
  
  #zateplenie
  d$zatepl[which(is.na(d$zatepl)& (as.character(d$konstr)=="panel"))]="0";
  d$zatepl[which(is.na(d$zatepl))]="na";
  
  #vybavenie
  d$vybav[which(is.na(d$vybav))]="na";
  
  #KURENIA and STUFF
  #zatial kaslem na to.
  
  #VYTAH
  d$vytah<-replace(d$vytah,is.na(d$vytah),"1") #predpokladame ze ma vytah
  
  #NADZEMNE PODLAZIE categorical
  #summary(d$podlazie)
  d$podlazie<-(as.factor(replace(as.character(d$podlazie),is.na(as.character(d$podlazie)),"2") ) ) #predpokladame ze je na medziposchodi
  
  
  #DATUMY
  d$DAYS[which(is.na(d$DAYS))]=median(d$DAYS, na.rm=TRUE);
  d$AGE_SELL[which(is.na(d$AGE_SELL))]=(d$AGE_INSERT-d$DAYS)[which(is.na(d$AGE_SELL))];
  
  
  
  # if(!je_validacny)
  #  {
  #MICE doplnanie
  library(mice);
  doplnacka<-d[,c("stav","konstr","AREA","rok","vlast")]
  for (i in c(1,2,4,5))
    doplnacka[,i]=as.factor(doplnacka[,i]);
  start<-proc.time();
  doplnackaModel<-mice(doplnacka, meth=c('polyreg','polyreg','pmm','polyreg','polyreg'));
  print(proc.time()-start);
  d[,names(doplnacka)]<-complete(doplnackaModel,1);
  #   doplnackaPovodneData<-doplnacka;
  rm(doplnacka);
  #dPoMICE<-d;
  
  
  
  
  ###################UPRAVA DAT####
  #d<-dPoMICE;
  #NUMERIKA
  d$wa<-(0.7001+0.3/(d$POC_DOPLN+1) );
  d$RozlohaIzby<-round(d$RozlohaIzby, digits=2);
  d$naklady<-round(d$naklady, digits=3);
  d$balkon_rozl<-round(d$balkon_rozl, digits=3);
  d$lodzia_rozl<-round(d$lodzia_rozl, digits=3);
  d$terasa_rozl<-round(d$terasa_rozl, digits=3);
  
  #finalne vahy
  w=as.double(d$wp)*as.double(d$wa);
  d=data.frame(w,d[,setdiff(names(d),"w")]);
  d$w<-round(d$w, digits=3);
  
  
  
  d<-d[,intersect(names_current,names(d) )];
  #popiskykonecne(); #scripts
}



posledne_upravy<-function(d)
{
  #nove prem
  #NOVE PREMENNE
  #=1 pre byty kolaudovane pred 1994 a rekonstruovane po 2006 ;=0 pre ine
  stary_rek=rep(0,length(d[,1]));
  stary_rek[which(( (d$rok%in% c("stary","sociknovy","socikstary")) & (d$val13>2006 ) ))]=1; #668
  names_current<-setdiff(names(d),"stary_rek")
  d<-data.frame(d[,names_current],stary_rek);
  names_current<-union(names(d),"stary_rek")
  rm(stary_rek);
  
  #=1 pre byty kolaudovane pred 1994 a zateplene ;=0 pre ine
  stary_zatepl<-rep(0,length(d[,1]));
  stary_zatepl[which( (d$rok%in% c("stary","sociknovy","socikstary")) & (as.integer(d$zatepl)==1) )]=1; #1340
  names_current<-setdiff(names(d),"stary_zatepl")
  d<-data.frame(d[,names_current],stary_zatepl);
  names_current<-union(names(d),"stary_zatepl")
  rm(stary_zatepl);
  d<-d[,setdiff(names(d),c("val13","val12") )];
  
  #length(d[which(d$REGION=="BA"),1]);#24777 bytov v BA
  #length(d[which(d$REGION=="BA" & is.na(d$ID_STREET)),1]);#5025 z nich nema STREET ID
  #doplnime Xlok
  muniBA<-REGPJBA$MUNICIPALITY_ID;
  d$Xlok=rep(NA,length(d$Xlok));
  d$Xlok[which(d$MUNICIPALITY %in%muniBA)]=as.character(d$ID_STREET[which(d$MUNICIPALITY %in%muniBA)]);
  d$Xlok[which( !(d$MUNICIPALITY %in%muniBA))]=as.character(d$MUNICIPALITY[which( !(d$MUNICIPALITY %in%muniBA) )]);
  d$Xlok[which(d$MUNICIPALITY %in%muniBA & is.na(d$ID_STREET))]=as.character(d$MUNICIPALITY[which(d$MUNICIPALITY %in%muniBA & is.na(d$ID_STREET))]);
  d$Xlok<-as.factor(d$Xlok);
  #vsetky Xlok maju hodnotu 
  
  
  ##############################################
  
  faktory<-c("Xlok","IDZAK","REGION","DISTRICT","MUNICIPALITY","CADASTRE","STREET","ID_STREET","Garsonka","PocetIzieb",
             "vlast","park_garaz","park_park","vytah","rok","podlazie","stav","balkon","lodzia","terasa","konstr",
             "zatepl","vybav","stary_rek","stary_zatepl")
  numerics<-c("w", "AREA","DAYS", "AGE_SELL","AGE_INSERT","RozlohaIzby","naklady","balkon_rozl", "lodzia_rozl","terasa_rozl");
  for (i in 1:length(faktory))
  {
    d[,faktory[i]]<-as.factor(as.character(d[,faktory[i]]));
  }
  menaa<-setdiff(names(d),c("PSQ","wp","Xlok","AGE_SELL","REGION","DISTRICT","MUNICIPALITY","CADASTRE","ID_STREET",
                            "dPSQ","SELL_PRICE", "wa","wv","wu","POC_DOPLN","IDZAK","STREET","INSERT_DAY","SELL_DAY","AGE_INSERT"));
  menaaSPOJ<-c("AREA","DAYS","RozlohaIzby","naklady","balkon_rozl","lodzia_rozl","terasa_rozl");
  menaaKAT<-setdiff(menaa,menaaSPOJ);
  
  #summary(d$park_park);summary(d$park_garaz);
  levels(d$park_park)=c("0","1","1");
  levels(d$park_garaz)=c("0","1","1");
  return(d)
  #vyhod<-rep(FALSE,)
}

popiskuj<-function()
{
  popisky<-zpopisky;
  popisky<-data.frame(zpopisky[,1:5], pocetnosti("Pred_alg_abs","Pred_alg_rel",dPredObohatenim,popisky));
  popisky<- data.frame(popisky[,1:7], pocetnosti("Po_alg_abs","Po_alg_rel", dPoSameAs,popisky) )
  popisky<-popisky[!is.na(popisky$Po_alg_abs),];
  
  zpopiskyPoCisti<-data.frame(popisky[,unique(names(popisky))], 
                              pocetnosti("Po_cisti_abs","Po_cisti_rel",dPoCisti,zpopiskyPoCisti) );
  zpopiskyPoNA<-data.frame(zpopiskyPoCisti[,1:11],
                           pocetnosti("Po_NA_abs","Po_NA_rel",dPoNA,zpopiskyPoCisti) );
  zpopiskyPoNA[,c(2:5)]<-apply(zpopiskyPoNA[,c(2:5)], 2,function(x) as.character(x) );
  
  
  # popisky<-data.frame(popisky[,1:13], pocetnosti("PoNAodv_abs","PoNAodv_rel",dPoNAodvazne) );
  
  #treba spatne zamenit mena
  popiskyEND<-zpopiskyPoNA;
  for (i in 1:length(novemena))
    popiskyEND$char_names[match(staremena[i],popiskyEND$char_names)]<-novemena[i];
  popiskyEND<-data.frame( popiskyEND[,1:13], pocetnosti("final_abs","final_rel",dPoNAodvazne,popiskyEND) );
  popiskyEND<-popiskyEND[which(!is.na(popiskyEND$final_abs)),]
  #docasne. odstranit ked bude script "popiskykonecne"^^
  #nevyhadzujem faktory
  return(popiskyEND);
} 

premenne<-unique(c("SELL_PRICE","PSQ","wp","wa","wv","wu","POC_DOPLN","Xlok","IDZAK","REGION","DISTRICT","MUNICIPALITY",
                   "CADASTRE","STREET","ID_STREET",
                   "AREA","INSERT_DAY","SELL_DAY","DAYS","AGE_SELL","AGE_INSERT","Garsonka","PocetIzieb","RozlohaIzby","val12","val13",
                   "vlast","park_garaz","park_park","vytah","rok","podlazie","naklady","stav","balkon","lodzia","terasa","konstr",
                   "balkon_rozl","lodzia_rozl","terasa_rozl","zatepl","vybav"));
faktoryy<-c("id26","id116","id125","id212","id213","id214","id215","id216","id223","id249","id261","Garsonka");
#"id121","id122","id123",
dPoNAodvazne<-NAodvazne(d=premenovanie(d=dPoNA,faktory = faktoryy),faktory=faktoryy,names_current=premenne,je_validacny=F);
dPoNAodvazne<-posledne_upravy(dPoNAodvazne)

#PRICE INDEX
PIndex<-PItabulka(dPoNAodvazne)
d<-zapisPI(dPoNAodvazne,PIndex)
dPoNAodvazne<-d

popiskyEND<-popiskuj()

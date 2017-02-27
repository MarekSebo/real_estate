#IFP vzorku vytvorit znova? (NIE!!!)
ifp=F;

#mozno nahradit popiskovy script tym z NacitajUpravene.r
nother=16;
popisky<-zpopisky;
startcas<-proc.time()
popisky_povodne<-zpopisky[,1:2];
#NACITANIE
directory<-"C://Users//Lenovo//Disk Google//Machine Learning//DTLN//integracia//data//"
print(paste("terajsia directory je:'", directory, "'ak treba, prepiste ju v kode (deklaracia premennych)", 
  sep=" "))
setwd(directory)  #getwd()
t<-read.csv(paste(directory,"valid.csv",sep = ""), header=TRUE, sep=";", 
            colClasses=c(rep("character",11),"numeric", rep("character", 2), rep("integer",2), "character", 
              "integer", "character","character"));
names(t)[c(1,7:9,12,13,14,15,18,19,20)]<-c("ID2","DISTRICT","MUNICIPALITY","CADASTRE","AREA","INSERT_DAY","SELL_DAY","SELL_PRICE","CHAR_VARIETY","CHAR_ID","CHAR_VALUE");
n<-length(t$ID2);

#MASTER VALUE namiesto VAL a ID VALUES
t$CHAR_VALUE<-gsub(",",".",t$CHAR_VALUE);
t$CHAR_VALUE<-replace(t$CHAR_VALUE, is.na(as.character(as.numeric(t$CHAR_VALUE))),"");
t$CHAR_ID<-replace(as.character(as.numeric(t$CHAR_ID)), is.na(as.character(as.numeric(t$CHAR_ID))),"");
masterval<-as.numeric(paste(t$CHAR_VALUE,t$CHAR_ID),sep="")
t<-data.frame(t[,c(1,2,4:16,18)],masterval)

t$IDZAK<-as.character(t$IDZAK);
t$IDZAK<-gsub("-","",t$IDZAK);
#ULOZ BACKUP
ztv<-t;

#ZRATAJ N
ajdi=as.character(1);
N<-0;
for(i in 1:n)
  if(ajdi!=t$ID2[i])
  {
    N=N+1;
    ajdi=t$ID2[i];
  }

#POZOR, TOTO ROBI PROBLEMY KED su bud chybne IDcka alebo nie su dokonale zoradene!! problem v nacitani premennych, fix needed || vyhodit riadky!!

#VALS A IDS , typy su unikatne pre konretne data, script je univerzalny
t<-ztv;
char_numberz<-sort(unique(c(zpopisky$char_numberz,sort(unique(t$CHAR_VARIETY)), sort(unique(t$ID_REALTY_CHAR)) )) );
char_types<- as.character(rep(NA,length(char_numberz)))
char_types<-as.character(zpopisky$char_types[match(char_numberz,zpopisky$char_numberz)  ] );
#unikatny prvok
char_types[is.na(char_types)]<-c("val","val","id","id","id","id","id","id","id","id","id")
char_names<-paste(char_types,as.character(char_numberz),sep="");

#POPISKY
char<-read.csv(paste(directory,"CMN_ciselnik_vlastnosti.csv",sep = ""), header=TRUE, sep=";");
popisky<-data.frame(char_numberz, char_types,char_names, 
                    char$CHAR_VARIETY_VALUE[match(as.integer(char_numberz),as.integer(char$CHAR_VARIETY))], 
                    char$MEASURE_UNIT[match(as.integer(char_numberz),as.integer(char$CHAR_VARIETY))]);
names(popisky)[c(4,5)]<-c("popis","unit")
zpopisky<-popisky;

#VYTVOR PREMENNE
ids_names<-zpopisky$char_names[which(zpopisky$char_types=="id")];
vals_names<-zpopisky$char_names[which(zpopisky$char_types=="val")];
id<-as.data.frame(matrix(as.integer(NA),N,length(ids_names)) );
val<-as.data.frame(matrix(as.numeric(NA),N,length(vals_names)) );

o<-as.data.frame(matrix(NA,N,nother));
names(o)<-names(t[1:nother])
names(id)<-ids_names;
names(val)<-vals_names;

#NACITAJ PREMENNE
byt=0;
ajdi=1;
name=as.name("jozko");
counter<-0;
t$ID2<-as.integer(t$ID2)

for(i in 1:n) #1000 riadkov za 6 sekund
{
  
  #NAPLN OTHERS  
  if(ajdi!=t$ID2[i] )
  {
    byt=byt+1;
    o[byt,1:nother]=t[i,1:nother];
    ajdi=t$ID2[i];
  }   
  #VALs
  if( zpopisky$char_types[match(t$CHAR_VARIETY[i],zpopisky$char_numberz)] =="val")
  {
    name<-as.character(paste("val",t$CHAR_VARIETY[i],sep=""))
    val[byt,name]=t$masterval[i];
  }
  #IDs
  if( zpopisky$char_types[match(t$CHAR_VARIETY[i],zpopisky$char_numberz)] =="id")  
  { 
    name<-as.character(paste("id",t$CHAR_VARIETY[i],sep=""))
    id[byt,name]=t$masterval[i];
  }  
}
#-----------------------------

###STRUKTURA DAT
#STRUKTURA POVODNYCH PREMENNYCH

for(i in 1:10)
  o[,i]=as.character(replace(o[,i],o[,i]=="0",NA));
o[,11]=as.integer(o[,11]);
o[,12]=as.Date(o[,12],format="%d.%m.%y");
o[,13]=as.Date(o[,13],format="%d.%m.%y");
o[,14]=as.integer(o[,14]);

#struktura id
for(i in 1:length(id[1,]))
  id[,i]=as.character(id[,i]);

#DATUMY ->AGE
AGE_INSERT<-as.integer(as.Date("01.01.2016",format="%d.%m.%Y")-o$INSERT_DAY)
#Price per sq meter
PSQ<-o$SELL_PRICE/o$AREA

#nova tabulka
o<-data.frame(o,AGE_INSERT,PSQ)

#################HOTOVE DATA#################
zovalidv<-data.frame(o,val,id);

#agregovanie> VYLEPSIT
zovalidvPoSpojeni<-zovalidv[order(zovalidv[,"IDZAK"], zovalidv[,"AGE_INSERT"]),];
zovalidvPoSpojeni<-zovalidvPoSpojeni[which(!duplicated(zovalidvPoSpojeni$IDZAK)),];

#REVIVE FROM BACKUP
t<-zovalidvPoSpojeni;
idp<-t[,match("id26",names(t)):length(names(t))] 
valp<-t[,(match("PSQ",names(t))+1):(match("id26",names(t))-1)]; 
op<-t[,1:match("PSQ",names(t))];
popisky<-zpopisky;

#uprav IDZAK
op$IDZAK<-as.character(op$IDZAK); 
op$IDZAK<-gsub("-","",op$IDZAK);

Np<-length(op$IDZAK);
menaa<-c(names(op)[1:14],          "STRONGVALIDATION",       "SAMEAS",   "SESTATE",      "DAYS", "AGE_SELL",  "AGE_INSERT", "PSQ" );
op<-data.frame(op[,c(1:14)],          rep(NA,Np),            rep(NA,Np),rep(NA,Np),       op[,c(15,16)],       rep(NA,Np),   op$PSQ);
names(op)<-menaa;
op$STRONGVALIDATION=1;
op$AGE_INSERT=op$AGE_SELL+op$DAYS;

#predane byty
wpr<-data.frame(wp=rep(1,Np), wa=rep(NA,Np),wv=rep(NA,Np),wu=rep(NA,Np))
dp<-data.frame(wpr,op,valp,idp)
names(dp)<-c(names(wpr),names(op),names(valp),names(idp));


#Vymazavanie 
######TU Merging riadkov ktore su uz v predanych (vsetky su aj v ponuke a vsetky duplicity obsahuju rovnake hodnoty vo svetkych premennych okrem SELL_DAY a td)
d<-dp
#premen nuly vo val7 na NA (v tomto scripte to budem cistit)
vyhod<-length(d$IDZAK);
vyhod<-duplicated(d$IDZAK);
vyhod[which(!is.na(d$val7) )]=TRUE; # optimalne tie co maju hodnotu 0 nevyhadzujem, nevedel som to ale nakodit
d<-d[which(vyhod==FALSE),];
names(d)[names(d)=="ID_REGION"]<-"REGION"

#obohatenie 
##

t<-read.csv("C://Users//Lenovo//Disk Google//Data Mining//source//final data//obohatene data//jan2016realiz_vylepsene.csv", 
            header=TRUE, sep=";", colClasses=c(rep("character",11),"numeric", rep("character", 2), 
                                               rep("integer",2), "character", "integer", "character","character") );
t<-t[,!names(t) %in% c("X")]
names(t)<- c("ID_REALTY_DATA", "IDZAK","ID_TYPE_TRAN", "ID_REALTY_VARIETY", "ID_UNIT_PRICE","ID_REGION","DISTRICT","MUNICIPALITY","CADASTRE","ID_STREET","STREET",
             "AREA","INSERT_DAY","SELL_DAY","SELL_PRICE","STRONGVALIDATION","CHAR_VARIETY","CHAR_VALUE","CHAR_ID","SAMEAS","SESTATE") ;

n<-length(t$ID_REALTY_DATA);
#nacitaj iba pridane vlastnosti
unikatne_char_var<-unique(c(unique(zt$CHAR_VARIETY), unique(zt$ID_REALTY_CHAR) )  ) ;
#vylucime tie co nie su obohatenim zo sestate, duplicitne riadky a marginalne premenne 
#(tie co v povodnych datach vobec nemali vyskyt)
t<-t[which( (t$CHAR_VARIETY %in%  unikatne_char_var) & as.logical(t$SESTATE) & !duplicated(t) ),]; #

#uprav IDZAK
t$IDZAK<-as.character(t$IDZAK);
t$IDZAK<-gsub("-","",t$IDZAK);

#MASTER VALUE namiesto VAL a ID VALUES
t$CHAR_VALUE<-replace(as.character(as.numeric(t$CHAR_VALUE)), is.na(as.character(as.numeric(t$CHAR_VALUE))),"");
t$CHAR_ID<-replace(as.character(as.numeric(t$CHAR_ID)), is.na(as.character(as.numeric(t$CHAR_ID))),"");
MASTERVAL<-as.numeric(paste(t$CHAR_VALUE,t$CHAR_ID))
t<-data.frame(t[,c(1,2,4:16)],t[,20:21],t[,17],MASTERVAL); 
names(t)[18]="CHAR_VARIETY";
t<-t[,c("IDZAK","CHAR_VARIETY","MASTERVAL","SAMEAS")]
t$SAMEAS<-as.logical(t$SAMEAS);
t$CHAR_VARIETY<-as.integer(as.character(t$CHAR_VARIETY));
#preznacit zle nahodene id213 
ktore<-which(t$CHAR_VARIETY==213);
t$MASTERVAL[which(t$CHAR_VARIETY==213 & t$MASTERVAL>=2013)]=8;
t$MASTERVAL[which(t$CHAR_VARIETY==213 & t$MASTERVAL>=2010& t$MASTERVAL<2013)  ]=7;
t$MASTERVAL[which(t$CHAR_VARIETY==213 & t$MASTERVAL>=2007& t$MASTERVAL<2010)  ]=6;
t$MASTERVAL[which(t$CHAR_VARIETY==213 & t$MASTERVAL>=2003& t$MASTERVAL<2007)  ]=1;
t$MASTERVAL[which(t$CHAR_VARIETY==213 & t$MASTERVAL>=1995& t$MASTERVAL<2003)  ]=2;
t$MASTERVAL[which(t$CHAR_VARIETY==213 & t$MASTERVAL>=1970& t$MASTERVAL<1995)  ]=3;
t$MASTERVAL[which(t$CHAR_VARIETY==213 & t$MASTERVAL>=1946& t$MASTERVAL<1970)  ]=4;
t$MASTERVAL[which(t$CHAR_VARIETY==213 & t$MASTERVAL>=1000& t$MASTERVAL<1946)  ]=5;

#NACITAJ PREMENNE
d$IDZAK<-as.character(d$IDZAK);
for(i in match("id26", names(d)):length(names(d)))
  d[,names(d)[i]]<-as.integer(as.character(d[,names(d)[i]]) )
ajdi=t$IDZAK[1];
riadok=match(ajdi,d$IDZAK);
name=as.name("jozko");
poc=0; ineid=0;
POC_DOPLN<-rep(0, length(d[,1]) );


#SAMEAS<-as.logical(rep(NA, length(d[,1]) ) );
#t$SAMEAS<-as.logical(t$SAMEAS);
start<-proc.time();
for(i in 1:length(t$IDZAK))#62k vlastnosti na 70k riadkov je 9 minut
{ 
  if(ajdi!=t$IDZAK[i])
  {
    POC_DOPLN[riadok]=poc;
    poc=0;
    ajdi=t$IDZAK[i];
    riadok=match(ajdi,d$IDZAK);
  }
  if(is.na(riadok))
  {ineid=ineid+1; next; }
  #VALs
  if( popisky$char_types[match(t$CHAR_VARIETY[i],popisky$char_numberz)] =="val")
  {
    name<-as.character(paste("val",t$CHAR_VARIETY[i],sep=""))    
  }
  #IDs
  if( popisky$char_types[match(t$CHAR_VARIETY[i],popisky$char_numberz)] =="id")  
  { 
    name<-as.character(paste("id",t$CHAR_VARIETY[i],sep=""))
  }    
  if(is.na(d[riadok,name]))
  {
    d[riadok,name]<-t$MASTERVAL[i];
    poc=poc+1;
  }
}
print("obohacovanie trvalo tolkoto");print(proc.time()-start);
names_current<-as.character(names(d) );
d<-data.frame(POC_DOPLN,d[,names_current]  ) ;                  
names_current<-union(c("POC_DOPLN"),as.character(names(d) ));
print("obohatenie prinieslo tolkoto novych udajov");
print(sum(POC_DOPLN));
print("najviac jednemu bytu tolkoto");
print(summary(POC_DOPLN))
print("medzi doplnujucimi datami bolo tolkoto s inymi IDZAK (tie som vyhodil)");
print(ineid);

ztuvalid<-t;
names(d)[names(d)=="ID_REGION"]<-"REGION"
dvPredCistenim<-d;

#Cistenie
dv<-dvPredCistenim;
dv<-cisti(dv,zpopisky,povinne_premenne=c("id212","id223"),
                    ChybajucePovinnePremTolerancia=1); #aj doplnanie NA. starou metodou bez mice.
dv<-dv[,names(dPoCisti)];

#NA konzervativne
dv<-NAckuj(dv);

#NA odvazne
premenne<-unique(c("SELL_PRICE","PSQ","wp","wa","wv","wu","POC_DOPLN","Xlok","IDZAK","REGION","DISTRICT","MUNICIPALITY",
                   "CADASTRE","STREET","ID_STREET",
                   "AREA","INSERT_DAY","SELL_DAY","DAYS","AGE_SELL","AGE_INSERT","Garsonka","PocetIzieb","RozlohaIzby","val12","val13",
                   "vlast","park_garaz","park_park","vytah","rok","podlazie","naklady","stav","balkon","lodzia","terasa","konstr",
                   "balkon_rozl","lodzia_rozl","terasa_rozl","zatepl","vybav"));
faktoryy<-c("id26","id116","id125","id212","id213","id214","id215","id216","id223","id249","id261","Garsonka");
#"id121","id122","id123",
dvPoNAodvazne<-NAodvazne(d=dv,faktory=faktoryy,names_current=premenne,je_validacny=T);

#IFP vzorka
if(ifp=T)
{
#IFP testovacia vzorka
vzorka_names<-c("IDZAK","REGION","MUNICIPALITY","ID_STREET","STREET","AREA","val230",
                "val245","val248","id212","id223","id125", "SELL_PRICE");
IFPtestvzorka<-dv[,vzorka_names];
for(i in 1:length(IFPtestvzorka[1,]))
  IFPtestvzorka[,i]<-as.factor(IFPtestvzorka[,i]);
levels(IFPtestvzorka$REGION)<-c("BA","TT","TN","NR","ZA","BB","PO","KE");
levels(IFPtestvzorka$MUNICIPALITY)<-REGPJ$MUNICIPALITY_NAME[match(levels(IFPtestvzorka$MUNICIPALITY),REGPJ$MUNICIPALITY_ID)];
IFPtestvzorka$STREET<-as.character(IFPtestvzorka$STREET);
IFPtestvzorka$STREET[which(as.character(IFPtestvzorka$REGION)!="BA")]=NA;
IFPtestvzorka$STREET<-as.factor(IFPtestvzorka$STREET);
names(IFPtestvzorka)=c("IDZAK","REGION","mestska cast","ID_STREET","STREET","plocha",
                       "plocha_balkon","plocha_lodzia","plocha_terasa","stav","konstrukcia","poschodie")
levels(IFPtestvzorka$konstrukcia)<-c("panel","tehla","zmies")
levels(IFPtestvzorka$stav)<-c("pov_stav","ciast_rek","kompl_rek","novo","nadstavba");
levels(IFPtestvzorka$poschodie)<-c("prizem","ine","podstr");
IFPtestvzorka<-data.frame(IFPtestvzorka, EST_PRICE=rep("",length(IFPtestvzorka$stav)));
IFPtestvzorka<-IFPtestvzorka[sample.int(length(IFPtestvzorka$REGION),size=300),];

write.table(IFPtestvzorka, file = "C://Users//Lenovo//Disk Google//Data Mining//diplomka files//source//final data//vzorka.csv",
            sep = ",", col.names = NA,qmethod = "double")
print("testovaci dataset v ludskej reci je nacitany v IFPtestvzorka a zapisany do suboru vzorka.csv. 
      zdroj (vstup do nasho modelu)je v dv.")
}

#posledne upravy
dv<-posledne_upravy(dvPoNAodvazne)
zdv<-dv;

rm(popisky_povodne,PSQ,AGE_INSERT,ajdi,byt,char_names,char_numberz,char_types,ids_names, masterval,name,
   vals_names,counter,nother,id,t,o,val,char,n,N,dp,Np,wpr,valp,op,idp,menaa,vyhod,d);

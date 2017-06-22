#todo identifikacia ktore Z DOLEZITYCH premennych boli doplnene algoritmom (z nich sa v cisti.R robi wa)
#zatial z algoritmoveho obohatenia beriem iba premenne, ktore mali aspon 1 vyskyt v povodnom datasete

#pod SELL_PRICE je v ponukovych OFFER_PRICE
#pod INSERT_DAY je v ponukovych LAST_UPDATE_DAY

vahy<-rep(NA,5);
vahy[1]<-0.3 #wp

#REVIVE FROM BACKUP
id<-zovalid[,match("id18",names(zovalid)):length(names(zovalid))]; idp<-zovalidp[,match("id26",names(zovalidp)):length(names(zovalidp))] 
val<-zovalid[,(match("PSQ",names(zovalid))+1):(match("id18",names(zovalid))-1)]; 
valp<-zovalidp[,(match("PSQ",names(zovalidp))+1):(match("id26",names(zovalidp))-1)]; 
o<-zovalid[,1:match("PSQ",names(zovalid))]; 
op<-zovalidp[,1:match("PSQ",names(zovalidp))];
popisky<-zpopisky;

#uprav IDZAK
o$IDZAK<-as.character(o$IDZAK); op$IDZAK<-as.character(op$IDZAK); 
o$IDZAK<-gsub("-","",o$IDZAK);op$IDZAK<-gsub("-","",op$IDZAK);

#priprava OTHERS na spojenie
N<-length(o$IDZAK);
Np<-length(op$IDZAK);
menaa<-c(names(op)[1:14],          "STRONGVALIDATION",       "SAMEAS",   "SESTATE",      "DAYS", "AGE_SELL",  "AGE_INSERT", "PSQ" )
op<-data.frame(op[,c(1:14)],          rep(NA,Np),            rep(NA,Np),rep(NA,Np),       op[,c(15,16)],       rep(NA,Np),   op$PSQ)
o<-data.frame (o [,c(1:12)], rep(NA,N),o[,c(13,14)],         rep(NA,N), rep(NA,N),        rep(NA,N), rep(NA,N), o$AGE_INSERT,o$PSQ     )
names(o)<-menaa;
names(op)<-menaa;
op$STRONGVALIDATION=1;
op$AGE_INSERT=op$AGE_SELL+op$DAYS;
o$DAYS=rep(NA,N);
o$AGE_SELL=rep(NA,N);
o$SELL_DAY<-rep(NA,N);

#####################################

#predane byty
wpr<-data.frame(wp=rep(1,Np), wa=rep(NA,Np),wv=rep(NA,Np),wu=rep(NA,Np))
dp<-data.frame(wpr,op,valp,idp)
names(dp)<-c(names(wpr),names(op),names(valp),names(idp));

#ponukane byty
w<-data.frame(wp=rep(vahy[1],N), wa=rep(NA,N),wv=rep(NA,N),wu=rep(NA,N))
#wp=ponukove , wa=penale za pocet doplneny z algoritmov (!iba tie co su v konecnom modeli!)

#####!!!!!beriem iba tie PREMENNE co su aj v PREDANYCH
d<-data.frame(w,o,val,id)[,names(dp)];
names(d)<-names(dp);

#Vymazavanie 
######TU Merging riadkov ktore su uz v predanych 
#(vsetky su aj v ponuke a vsetky duplicity obsahuju rovnake hodnoty vo vsetkych premennych okrem 
#SELL_DAY a td)
d<-rbind(dp,d)
#premen nuly vo val7 na NA (v tomto scripte to budem cistit)
vyhod<-length(d$IDZAK);
vyhod<-duplicated(d$IDZAK);
vyhod[which(!is.na(d$val7) )]=TRUE; # optimalne tie co maju hodnotu 0 nevyhadzujem, nevedel som to ale nakodit
d<-d[which(vyhod==FALSE),];
names(d)[names(d)=="ID_REGION"]<-"REGION"
dPredObohatenim<-d;


#
#MERGING s UPRAVENYMI
d<-dPredObohatenim;
t<-read.csv("C://Users//Lenovo//Disk Google//Data Mining//source//final data//obohatene data//ponuka vylepsena final.csv", 
            header=TRUE, sep=";", colClasses=c(rep("character",11),"numeric", rep("character", 2), 
                                               rep("integer",2), "character", "integer", "character","character") );
names(t)<- c("ID_REALTY_DATA", "IDZAK","ID_TYPE_TRAN", "ID_REALTY_VARIETY", "ID_UNIT_PRICE","ID_REGION","DISTRICT","MUNICIPALITY","CADASTRE","ID_STREET","STREET",
             "AREA","INSERT_DAY","SELL_DAY","SELL_PRICE","STRONGVALIDATION","CHAR_VARIETY","CHAR_VALUE","CHAR_ID","SAMEAS","SESTATE") ;
ztupovodne<-t;
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


ztu<-t;
names(d)[names(d)=="ID_REGION"]<-"REGION"
dPoObohateni<-d;
#poupratuj
rm(d,N,Np,POC_DOPLN, ajdi, name,poc, riadok, unikatne_char_var,dp,id,idp,t,n,o,op,val,valp,w,wpr,menaa,vyhod,MASTERVAL,ineid,ktore);

##########


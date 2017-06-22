########################
########po nacitani validacneho

#moze obsahovat chyby

#odobratie prieniku s VALIDACNYM
d<-dPoObohateni;
if(is.null(zovalidv))
  return(print("ERROR: najprv nacitaj validacny dataset"))
vyhod<-rep(FALSE,length(d$PSQ));
vyhod[which(d$IDZAK %in% zovalidv$IDZAK)]=TRUE;
dPoPrienikuSValidacnym<-d[!vyhod,];

#SAME AS (script) + aplikacia scriptu
sameas<-function(dd,sameass,popiskyy)
{
 
names(sameass)<-c("all","master");

vyhodIDZAK<-as.character(sameass$all[sameass$master!=""]);
vyhodIDZAK<-gsub("-","",vyhodIDZAK);
print("same as vyhodi tolkoto");print(length(vyhodIDZAK));
#vyhadzovanie
return(dd[setdiff(c(1:length(dd$PSQ)), match(vyhodIDZAK, dd$IDZAK)),]);
rm(vyhod,vyhodIDZAK, popiskyy,dd,sameass);
}

#spusti script
d<-dPoPrienikuSValidacnym;
sameasy<-read.csv("C://Users//Lenovo//Disk Google//Data Mining//source//final data//obohatene data//sameas.csv"
                 , header=FALSE, sep=";")
popisky<-zpopisky
dPoSameAs<-sameas(d,sameasy,popisky); 

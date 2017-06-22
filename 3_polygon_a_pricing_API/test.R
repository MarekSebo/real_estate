
#format:
#pricingAPI
#pricingAPI(realestateTypeId,offerTypeId, streetId, municipalityId, streetNumber, area, 
#     objectStatusId, buildingApprovalEndYear, realised, hasBalcony, hasTerrace, hasLoggia, hasElevator)
#polygonAPI:
#polygonAPI(polygon_lat, polygon_long, ktory_model, realestateTypeId,offerTypeId, streetId, municipalityId, streetNumber, 
#  area, objectStatusId, buildingApprovalEndYear, realised, hasBalcony, hasTerrace, hasLoggia, hasElevator)

#spustat obe funkcie sa da bud specifikaciou parametrov:
pricingAPI(realestateTypeId="122",offerTypeId=3, streetId=529460104, municipalityId=500011, streetNumber=3, area=50, 
     objectStatusId=1, buildingApprovalEndYear=2, realised=T, hasBalcony=T, hasTerrace=F, hasLoggia=F, hasElevator=T)
#alebo iba vypisanim vstupov
pricingAPI("15",3, 529460104, 529494, 4, 100, 1, "3", F, T, T,F, T) 

##############
#obe API si poradia s integer a char premennymi, poslednych 5 premennych musi byt logical(=boolean) alebo ineger {0,1}
pricingAPI("15",3, 529460104, "529494", 4, 100, 1, "3", F, TRUE, T,0, T) 
#ak pouzivatel zle zada/ nezada ulicu, system prepne na municipality model 
pricingAPI(122,3, "jozko", 500011, 4, 70, 1, 3, T, T, F,F, T)

#neznamy vstup je najlepsie zadat ako NA. bude pouzity default
pricingAPI(12,3, "jozko", 500011, 4, 70, 1, 3, T, NA, NA,NA, T)

cas<-proc.time()
pricingAPI(122,3, "jozko", 500011, 4, 70, 1, 3, T, T, F,FALSE, TRUE)
print("runtime: ")
print(cas-proc.time() )

##############
#2) polygonAPI
#vektorove vstupy v int alebo inom numeric formate (float, real, ...)
#!!!POLYGON je zadany v poradi ABDC
#testovaci vstup: polygon cez pol slovenska
lat=c(47.00,47,50,50)
long=c(15,19,15,19)
ktory_model<-"muniSR"

#testovaci vstup: polygon v BA 
lat=c(48.00,48.00,48.08,48.08)
long=c(17.10,17.20,17.10,17.20)
ktory_model<-"uliceBA"

cas<-proc.time()
polygonAPI(lat,long,ktory_model,realestateTypeId="13",offerTypeId=1, streetId=529460104, municipalityId=500011, streetNumber=3, area=50, 
     objectStatusId=1, buildingApprovalEndYear=2, realised=T, hasBalcony=T, hasTerrace=F, hasLoggia=F, hasElevator=T)
#nakresli body
plot(polygonAPI(lat,long,ktory_model,122,1, "jozko", 500011, 4, 70, 1, 3, T, T, F,F, T)[,c("long","lat")] )
print("runtime: ")
print(proc.time()-cas )



#VYSETROVANIE
##polygony
sur_nitra<-muniSR[which(muniSR$MUNI_ID==500011 ),c("lat","long")]
lat<-c(48.29,48.29,48.33,48.33)
long<-c(18.00,18.10,18.00,18.10)
ktory_model<-"muniSR"
polygonAPI(lat,long,ktory_model,12,1, "jozko", 500011, 4, 70, 1, 3, T, T, F,F, T)
plot(polygonAPI(lat,long,ktory_model,12,1, "jozko", 500011, 4, 70, 1, 3, T, T, F,F, T)[,c("long","lat")] )

#VYSETROVANIE
#CHYBAJUCE UZEMNE JEDNOTKY V DATASETOCH (prieniky, zjednotenia,...)
#co chyba z validsetu
chybajuce<-unique(validset$Xlok[which(!(validset$Xlok %in% validset_s_odhadmi$Xlok))])
REGPJ$MUNICIPALITY_NAME[match(as.character(chybajuce), REGPJ$MUNICIPALITY_ID)] #505820 Trencin

#co chyba z dm?
dmmuni<-dm[which(nchar(as.character(dm$Xlok))=="6"),]
dmstreets<-dm[which(nchar(as.character(dm$Xlok))=="9"),]
chybajuce2<-paste("Xlok",unique(dmmuni$Xlok),sep="")
which(!(chybajuce2 %in% names(Rmodel$coefficients) ))
chybajuce2<-paste("Xlok",unique(dmstreets$Xlok),sep="")
which(!(chybajuce2 %in% names(Rmodel$coefficients) ))
#nic.

#fucking trencin
Rmodel$coefficients["Xlok505820"] #je tam tiez.

#v muniSR je?
chybajuce<-dmmuni$Xlok[which(!(unique(as.integer(as.character(dmmuni$Xlok))) %in% muniSR$MUNI_ID))] 
REGPJ$MUNICIPALITY_NAME[match(chybajuce, REGPJ$MUNICIPALITY_ID)] 
#bratislavske muni   "Bratislava-Podunajské Biskupice" "Stupava"         "Pezinok"       "Senec"
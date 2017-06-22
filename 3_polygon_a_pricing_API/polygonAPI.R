#sluzba2 =polygonAPI.R
#polygonAPI<-function(polygon, zadany_model, realestateTypeId,offerTypeId, streetId, municipalityId, streetNumber, area, 
#        objectStatusId, buildingApprovalEndYear, realised, hasBalcony, hasTerrace, hasLoggia, hasElevator)
#output = pomenovany data.frame (zatial: "lok","lok_ID","lat","long","cena")
#transf() transformuje tabulku (inak treba poriesit ked nezada lokalitu. treba ho aj tak pustit)
#vyberie ktore indexy su v polygone
#pricing() oceni s pomocou vystupu z transf()

polygonAPI<-function(polygon_lat, polygon_long, ktory_model, realestateTypeId,offerTypeId, streetId, municipalityId, streetNumber, area, 
         objectStatusId, buildingApprovalEndYear, realised, hasBalcony, hasTerrace, hasLoggia, hasElevator)
{
#format vstupov: 
#  ktory_model ={"uliceBA","muniSR"}
#  polygon_lat, polygon_long = ciselne vektory

#testy spravnosti vstupov
polygon<-as.matrix(data.frame(polygon_lat, polygon_long))
ktory_model<-as.character(ktory_model)
if(ncol(polygon)!=2)
  return(paste("zly format polygonu: ",polygon, sep="") )

#DODANY KOD PRE SUCASNU VERZIU CMN (ked sa odstrani, bude to fungovat pre lubovolny k-uholnik zadany v poradi ako idu vrcholy)
if(nrow(polygon)!=4)
  return(paste("DODANY KOD: zly pocet riadkov polygonu! v tejto verzii treba otvorený 4-uholník (4riadky). 
    Zadaný má: ",nrow(polygon), sep="") )
druhy_riadok=polygon[2,]
polygon[2,]=polygon[3,]
polygon[3,]=druhy_riadok
#KONIEC DODANEHO KODU

if(!(ktory_model %in% c("muniSR","uliceBA")) ){
  new_model<-vyber_model(municipalityId,streetId)
  print(paste("zle zadany typ modelu do polygonAPI: ",ktory_model,"  procedurou vyber_model() bol vybrany: ",new_model,sep=""))
  ktory_model<-new_model
}
print(paste("PolygonAPI> aktivoval sa:", ktory_model,"model",sep=" "))
#transformacia vstupu z formulara
x<-transf(realestateTypeId,offerTypeId, streetId, municipalityId, streetNumber, area, 
  objectStatusId, buildingApprovalEndYear, realised, hasBalcony, hasTerrace, hasLoggia, hasElevator)

#z ktoreho df vyberame?
if (ktory_model=="muniSR")
  data=muniSR else 
  data=uliceBA
n<-nrow(data)
index<-rep(NA,n)

#ktore suradnice su v polygone?
indexy<-which(in.out(polygon,as.matrix(data[,c("lat","long")]) ) )
if(length(indexy)==0)
  return(print(paste("v zadanom polygone sa nenachadza ani jedna jednotka z: ",ktory_model,sep="")))
print(paste("tolkoto indexov:",length(indexy),sep=" ")) 

#ocenovanie
cena<-exp(data[indexy,"koef_final"] +pricing(ktory_model,indexy, x) )
#print(paste("data[indexy,koef_final]",data[indexy,"koef_final"],sep=" : "))
#print( paste("pricing(ktory_model,indexy, x)",pricing(ktory_model,indexy, x),sep=" : ") )

#priprava vystupu (df)
if(ktory_model=="muniSR")
  output<-data.frame(data[indexy,c("MUNI_ID","MUNI","lat","long")], cena)
if(ktory_model=="uliceBA")
  output<-data.frame(data[indexy,c("STREET_ID","STREET","lat","long")], cena)  
return(output)
}


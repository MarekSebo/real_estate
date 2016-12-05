dm=pd.read_csv('dm.csv')

 #SEED
np.random.seed(232)


#vyber premenne ktore treba
premenne=['Xlok', 'Garsonka','PocetIzieb', 'vlast', 'park_garaz', 'vytah', 'rok', 'podlazie', 'stav',
       'konstr', 'vybav', 'stary_rek', 'wp', 'RozlohaIzby', 'balkon_rozl', 'lodzia_rozl', 'terasa_rozl']
y_name=['dPSQ']
spojite_premenne=['RozlohaIzby', 'balkon_rozl', 'lodzia_rozl', 'terasa_rozl']
kategoricke_premenne=[x for x in premenne if x not in spojite_premenne]

dm=dm[premenne+y_name]



#definicie
def kriteria(predictedY, realY):
    print("function 'kriteria': validation criteria on the sets you specified....")
    if(predictedY.shape!= realY.shape):
        return "Error in Kriteria function: šejpy nešejpujú!: predictedY_shape: {}, realY_shape: {} ".format(predictedY.shape, realY.shape)
    n_vzorka=np.shape(realY)[0]
    print("validation on set size of {} began".format(n_vzorka))
    residuals=(realY - predictedY) #vektor rezidui
    RSS = np.sum(residuals ** 2)
    TSS = np.sum((realY - np.mean(realY)) ** 2)
    print("sqrtMSE = ", np.sqrt(RSS / n_vzorka) )
    print("R**2= ", 1 - RSS / TSS)  # prikaz model_moj.score(testX, testY)
    perc_residuals=residuals/realY
    print("mRE = ", np.median(abs(perc_residuals)))
    print("sqrtMSE v percentach = ", np.sqrt(np.sum(perc_residuals**2)/n_vzorka ))
    print("-------------------------------------------------------------------END")




#shuffle
n_all=len(dm[y_name]) # 55 523
shuf=np.arange(n_all)
np.random.shuffle(shuf)

dm=dm.ix[shuf,premenne+y_name ]
dm.index=range(len(dm.ix[:,1]))

#typy premennych urci
dm[ kategoricke_premenne ]=\
    dm[kategoricke_premenne ].apply(lambda x: x.astype('category'))
dm[ spojite_premenne+y_name]=\
    dm[spojite_premenne+y_name].apply(lambda x: x.astype('float32'))
print("tieto premenne povazujeme za spojite: ",dm[spojite_premenne+y_name].dtypes )
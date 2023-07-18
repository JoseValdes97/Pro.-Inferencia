#inferencia estadistica proyecto 2

alpha=0.05   # el alpha para nuetro intervalo de confianza, solo está aqui para generalizar un poco el codigo para otros valores de alpha


for (p in seq(0.05,0.95, by=0.05)){
  for (n in list(5,10,50,100,200,500,1000)){
    data<-rbinom(n,1,p)#Recordemos que la 
    #distribución de Bernoulli es la distribucioón binomial con un solo intento 1
    
    #definimos las siguientes listas vacias (iremos añadiendole valores por cada simulación)
    
    conteo_pos1=c() ;conteolong_pos1=c() #Conteo de intervalos que contuvieron a p en la posibilidad 1 y la longitud 
    conteo_pos2=c(); conteolong_pos2=c() #Conteo de intervalos que contuvieron a p en la posibilidad 2 y la longitud 
    conteo_pos3=c() ; conteolong_pos3=c() #Conteo de intervalos que contuvieron a p en la posibilidad 3 y la longitud 
    conteo_pos4=c(); conteolong_pos4=c() #Conteo de intervalos que contuvieron a p en la posibilidad 4 y la longitud 
    conteo_pos5=c(); conteolong_pos5=c() #Conteo de intervalos que contuvieron a p en la posibilidad 5 y la longitud 
    for (i in 1:1000){
      x<-sample(data,n,replace=TRUE) 
      #Hallamos los intervalos de confianza de cada posibilidad
      #Icpos1<-c(-qnorm(1-alpha),qnorm(1-alpha)) #Corregir limites del intervalo una vez se haya hecho el punto 1
      Icpos2<-c(mean(x)-(qnorm(1-alpha/2)*sqrt(mean(x)*(1-mean(x)))),mean(x)+(qnorm(1-alpha/2)*sqrt(mean(x)*(1-mean(x)))))
      Icpos3<-c(sin(asin(sqrt(mean(x)))-(qnorm(1-alpha/2)/(2*sqrt(n))))^2,sin(asin(sqrt(mean(x)))+(qnorm(1-alpha/2)/(2*sqrt(n))))^2)
      #Icpos4<- Pendiente
      #Icpos5<-Pendiente
      
      #Conteo de intervalos que contienen a p en la posibilidad 1
      if (Icpos1[1]<=p && p<= Icpos1[2]){
        pos1=1
        longitudpos1=Icpos1[2]-Icpos1[1]
        conteo_pos1=append(conteo_pos1,pos1)
        conteolong_pos1<-append(conteolong_pos1,longitudpos1)
      }
      else {pos1=0
      conteo_pos1=append(conteo_pos1,pos1)}
        
      #Conteo de intervalos que contienen a p en la posibilidad 2
      if (Icpos2[1]<=p && p<= Icpos2[2]){
        pos2=1
        longitudpos2=Icpos2[2]-Icpos2[1]
        conteo_pos2=append(conteo_pos2,pos2)
        conteolong_pos2<-append(conteolong_pos2,longitudpos2)
      }
      else {pos2=0
      conteo_pos2=append(conteo_pos2,pos2)}
      
      
      #Conteo de intervalos que contienen a p en la posibilidad 3
      if (Icpos3[1]<=p && p<= Icpos3[2]){
        pos3=1
        longitudpos3=Icpos3[2]-Icpos3[1]
        conteo_pos3=append(conteo_pos3,pos3)
        conteolong_pos3<-append(conteolong_pos3,longitudpos3)
      }
      else {pos3=0
      conteo_pos3=append(conteo_pos3,pos3)}
      
      #Conteo de intervalos que contienen a p en la posibilidad 4, el codigo quizas puede cambiar por el bootstraping
     # if (Icpos4[1]<=p && p<= Icpos4[2]){
      #  pos4=1
       # longitudpos4=Icpos4[2]-Icpos4[1]
      #  conteo_pos4=append(conteo_pos4,pos4) #añadimos un uno a la cuenta de intervalos que contienen a p
      #  conteolong_pos4<-append(conteolong_pos4,longitudpos4) #añadimos la longitud del intervalo
       #else {pos4=0
      #conteo_pos4=append(conteo_pos4,pos4)}
      
      #Conteo de intervalos que contienen a p en la posibilidad 5
      #if (Icpos5[1]<=p && p<= Icpos5[2]){
       # pos5=1
        #longitudpos5=Icpos5[2]-Icpos5[1]
        #conteo_pos5=append(conteo_pos5,pos5)
        #conteolong_pos5<-append(conteolong_pos5,longitudpos5)}
      #else {pos5=0
      #conteo_pos5=append(conteo_pos5,pos5)}
       }
      
    
    cobertura_promediopos1=sum(conteo_pos1)/1000
    longitud_promediopos1=sum(conteolong_pos1)/1000
    
    
    cobertura_promediopos2=sum(conteo_pos2)/1000
    longitud_promediopos2=sum(conteolong_pos2)/length(conteolong_pos2)
    
    
    cobertura_promediopos3=sum(conteo_pos3)/1000
    longitud_promediopos3=sum(conteolong_pos3)/length(conteolong_pos3)
    
    #cobertura_promediopos4=sum(conteo_pos4)/1000
    #longitud_promediopos4=sum(conteolong_pos4)/length(conteolong_pos4)
    
    #cobertura_promediopos5=sum(conteo_pos5)/1000
    #longitud_promediopos5=sum(conteolong_pos5)/length(conteolong_pos5)
    }
}

#inferencia estadistica proyecto 2

#Definimos la funcion de bootstrapping para nuestro intervalo de confianza en la posibilidad 4
bootstrappos4<- function(d){
  d[1]=n ; d[2]=p
  B=1000
  bootstrap <- replicate(B,sample(data,n,replace=TRUE))  #Repetimos el muestreo 1000 veces
  medias <- apply(bootstrap, MARGIN=2, FUN=mean) #Tomamos la media de cada muestra obtenida
  ICpos4<-quantile(medias,c(alpha/2,1-alpha/2)) #Definimos los intervalos por medio de cuantiles
  return(ICpos4)
}
#Definimos la funcion de boostrapping para nuestro intervalo de confianza en la posibilidad 5
bootstrappos5<- function(d){
  d[1]=n ; d[2]=p
  B=1000
  diferencias<-c()
  for (l in 1:B){
    data2<-sample(data,n,replace=TRUE)
    mediad= mean(data2)-mean(data)
    diferencias<-append(diferencias,mediad)
  }
  cuantiles<-quantile(diferencias,c(alpha/2,1-alpha/2)) #Definimos los intervalos por medio de cuantiles
  ICpos5<-c(mean(data)-cuantiles[2],mean(data)-cuantiles[1])
  return(ICpos5)
}


#codigo principal


alpha=0.05   # el alpha para nuetro intervalo de confianza, solo está aqui para generalizar un poco el codigo para otros valores de alpha

#Estas matrices nos sirven como tablas para representar los datos, sujeto a modificaciones

A1<-matrix(0,length(c(5,10,50,100,200,500,1000)),length(seq(0.05,0.95, by=0.05)));
A2<-matrix(0,length(c(5,10,50,100,200,500,1000)),length(seq(0.05,0.95, by=0.05)));
A3<-matrix(0,length(c(5,10,50,100,200,500,1000)),length(seq(0.05,0.95, by=0.05)));
A4<-matrix(0,length(c(5,10,50,100,200,500,1000)),length(seq(0.05,0.95, by=0.05)));
A5<-matrix(0,length(c(5,10,50,100,200,500,1000)),length(seq(0.05,0.95, by=0.05)));
j=1
for (p in seq(0.05,0.95, by=0.05)){
  b=1
  for (n in c(5,10,50,100,200,500,1000)){
    #definimos las siguientes listas vacias (iremos añadiendole valores por cada simulación)
    
    conteo_pos1=c() ;conteolong_pos1=c() #Conteo de intervalos que contuvieron a p en la posibilidad 1 y la longitud 
    conteo_pos2=c(); conteolong_pos2=c() #Conteo de intervalos que contuvieron a p en la posibilidad 2 y la longitud 
    conteo_pos3=c() ; conteolong_pos3=c() #Conteo de intervalos que contuvieron a p en la posibilidad 3 y la longitud 
    conteo_pos4=c(); conteolong_pos4=c() #Conteo de intervalos que contuvieron a p en la posibilidad 4 y la longitud 
    conteo_pos5=c(); conteolong_pos5=c() #Conteo de intervalos que contuvieron a p en la posibilidad 5 y la longitud 
    for (i in 1:1000){
      data<-rbinom(n,1,p) #Recordemos que la 
      #distribución de Bernoulli es la distribucioón binomial con un solo intento 1
      
      
      #Hallamos los intervalos de confianza de cada posibilidad
      Icpos1<-c(2*mean(data)/(1+(qnorm(1-alpha/2)^2)/n), 2*(mean(data)+(qnorm(1-alpha/2)^2)/n)/(1+(qnorm(1-alpha/2)^2)/n))
      Icpos2<-c(mean(data)-(qnorm(1-alpha/2)*sqrt(mean(data)*(1-mean(data))/n)),mean(data)+(qnorm(1-alpha/2)*sqrt(mean(data)*(1-mean(data))/n)))
      Icpos3<-c(sin(asin(sqrt(mean(data)))-(qnorm(1-alpha/2)/(2*sqrt(n))))^2,sin(asin(sqrt(mean(data)))+(qnorm(1-alpha/2)/(2*sqrt(n))))^2)
      Icpos4<- bootstrappos4(c(n,p))
      Icpos5<-bootstrappos5(c(n,p))
      
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
      
      #Conteo de intervalos que contienen a p en la posibilidad 4
      
     if (Icpos4[1]<=p && p<= Icpos4[2]){
       pos4=1
       longitudpos4=Icpos4[2]-Icpos4[1]
      conteo_pos4=append(conteo_pos4,pos4) #añadimos un uno a la cuenta de intervalos que contienen a p
      conteolong_pos4<-append(conteolong_pos4,longitudpos4) #añadimos la longitud del intervalo
      }
      else {pos4=0
      conteo_pos4=append(conteo_pos4,pos4)}
      
      #Conteo de intervalos que contienen a p en la posibilidad 5
      if (Icpos5[1]<=p && p<= Icpos5[2]){
        pos5=1
        longitudpos5=Icpos5[2]-Icpos5[1]
        conteo_pos5=append(conteo_pos5,pos5)
        conteolong_pos5<-append(conteolong_pos5,longitudpos5)}
      else {pos5=0
      conteo_pos5=append(conteo_pos5,pos5)}
    }
      
    
    cobertura_promediopos1=sum(conteo_pos1)/1000
    longitud_promediopos1=sum(conteolong_pos1)/1000
    A1[b,j]<-cobertura_promediopos1
    
    
    cobertura_promediopos2=sum(conteo_pos2)/1000
    longitud_promediopos2=sum(conteolong_pos2)/length(conteolong_pos2)
    A2[b,j]<-cobertura_promediopos2
    
    cobertura_promediopos3=sum(conteo_pos3)/1000
    longitud_promediopos3=sum(conteolong_pos3)/length(conteolong_pos3)
    A3[b,j]<-cobertura_promediopos3
    
    cobertura_promediopos4=sum(conteo_pos4)/1000
    longitud_promediopos4=sum(conteolong_pos4)/length(conteolong_pos4)
    A4[b,j]<-cobertura_promediopos4
    
    cobertura_promediopos5=sum(conteo_pos5)/1000
    longitud_promediopos5=sum(conteolong_pos5)/length(conteolong_pos5)
    A5[b,j]<-cobertura_promediopos5
    
     b=b+1
  }
  j=j+1
}
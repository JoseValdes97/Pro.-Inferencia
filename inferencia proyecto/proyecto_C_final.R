######       PUNTO C     #############

###  parte a  ##########

#####Simulación y gráfica ######

##Vamos a simular 1000 veces 10 observaciones de una distribución doble 
##exponencial con theta = 6 y betha = 1

### primero crearemos una matriz donde se guardaran las simulaciones
datos <- matrix(data=NA,nrow=10,ncol=1000)
### crearemos un loop para las simulaciones
for (i in 1:1000) {datos[,i] <- rdexp(n=10,location = 6,scale = 1)}
### ahora para manipulación volver data frame
datos <- as.data.frame(datos)

#########################

##Vamos a hacer un histograma y  superponer sobre él la densidad de una 
# doble exponencial

###prob=TRUE muestra en escala de densidad
#xlim=c(min(data1),max(data1))
      hist(datos$V1,prob=TRUE,xlab="datos",
           ylab="densidad",ylim = c(0,0.5),
           main ="Histograma de datos simulados de una doble exponencial");
###sobre poner la densidad de la doble exponencial
xpts<-seq(min(datos$V1),max(datos$V1),by=0.001)
lines(xpts,ddexp(xpts, location = 6, scale = 1),col="red",lwd=2,lty = 5)

#######################

##### Maximizar una verosimilitud usando optim ######

####Vamos a olvidarnos ahora de que conocemos los parámetros y las 
####distribuciones correspondientes y vamos a encontrar las estimaciones 
####ML usando software (métodos numéricos)

### Doble exponencial

### creamos la matriz donde almacenaremos las estimaciones ML
MLE_MOM <- matrix(data=NA,nrow=2,ncol=1000)
rownames(MLE_MOM) <- c("MLE","MOM")

### crearemos un loop para encontar las estimaciones para cada una de 
### nuestras muestras
for (i in 1:1000) {
      fDobExp <- function(theta){
            fdexp=(1/2)*exp(-(abs(datos[,i] - theta)))
            L=-sum(log(fdexp)) #el negativo de la log-verosimilitud
            #return(L)
      }
      #print(fDobExp2(5))
      sol=optim(4,fDobExp) #optim busca el valor que hace mínima una función
      a=sol$par[1]
      MLE_MOM[1,i] <- a
}

#######################

### para obtener el estimador de momentos
MOM <- colMeans(datos)

## ahora lo ponemos en la tabla
MLE_MOM[2,] <- MOM
## para manipulación lo convertimos en un data frame
MLE_MOM <- as.data.frame(MLE_MOM)

############################

## extraemos las estimaciones del MLE
MLE <- as.numeric(MLE_MOM[1,])
### histograma para las estimaciones MLE
hist(MLE,prob=TRUE,xlim=c(min(MLE),max(MLE)),xlab="datos",ylab="densidad",
     main="Histograma del MLE")
## linea del parametro real
abline(v = 6,col = 'red', lwd = 4)
## linea del promedio de las estimaciones
# mean(MLE) = 6.010569
abline(v = mean(MLE),col = 'green', lwd = 2)
## cudaro para identificar las lineas
legend(locator(1), c("Theta","mean MLE"), col=c("red","green"),
       lty=c(1,1),lwd=c(2,2))

## extraemos las estimaciones del MOM
MOM <- as.numeric(MLE_MOM[2,])
### histograma para las estimaciones del MOM
hist(MOM,prob=TRUE,xlim=c(min(MOM),max(MOM)),xlab="datos",ylab="densidad",
     main="Histograma del MOM ") 
## linea del parametro real
abline(v = 6,col = 'red', lwd = 4)
## linea del promedio de las estimaciones
# mean(MOM) = 6.005607
abline(v = mean(MOM),col = 'blue', lwd = 2)
## cudaro para identificar las lineas
legend(locator(1), c("Theta","mean MOM"), col=c("red","blue"),
       lty=c(1,1),lwd=c(2,2))

#############################

### tabla con la media y la varianza de MLE y MOM
tab <- matrix(data=NA,nrow=2,ncol=2)
colnames(tab) <- c("MLE","MOM")
rownames(tab) <- c("Mean","Var")
## MLE
tab[,1] <- c(mean(MLE),var(MLE))
## MOM
tab[,2] <- c(mean(MOM),var(MOM))
###### genrar tabla en codigo latex
xtable(tab,digits = 3,3)

############################################################################
#########################################################
##################################################################

### parte b   ###################

##Vamos a simular 1000 veces diferentes tamaños de muestras de una distribución 
##doble exponencial con theta = 6 y betha = 1

### creamos una lista vaica
Dif_Muest <- list()
## primero crearemos una matriz donde se guardaran las simulaciones de diferentes
## tamaños
## vector de diferentes tamaños
n_Mues <- c(50,100,200,500,1000)
## bucle que crear los data frames y guardar en la lista
for (l in 1:5) {
      Dif_Muest[[l]] <- as.data.frame(matrix(data=NA,nrow=n_Mues[l],ncol=1000))
}
### crearemos un loop para las simulaciones
for (j in 1:5) {
   for (i in 1:1000) {
      Dif_Muest[[j]][,i] <- rdexp(n=n_Mues[j],location = 6,scale = 1)
   }
}


#############################

##Vamos a hacer un histograma y  superponer sobre él la densidad de una 
# doble exponencial para la primer columna de cada uno de los tamaños muestrales

### creamos un interfaz para tener todos los graficos
par(mfrow = c(3,2))
###prob=TRUE muestra en escala de densidad
#xlim=c(min(data1),max(data1))
for (t in 1:5) {
   hist(Dif_Muest[[t]]$V1,prob=TRUE,xlab="datos",
        ylab="densidad",ylim = c(0,0.5),col = t+1,
        main ="Hist. de datos simulados");
   ####sobre poner la densidad de la doble exponencial
   xpts<-seq(min(Dif_Muest[[t]]$V1),max(Dif_Muest[[t]]$V1),by=0.001);
   lines(xpts,ddexp(xpts, location = 6, scale = 1),col="darkgoldenrod2",lwd=2,
         lty = 1)
}
## para el cuadro de leyendas
hist(0,ylab = NULL,main="Guia de colores",col = "white",axes = FALSE)
legend(locator(1), c("n = 50","n = 100","n =200","n = 500","n = 1000"),
       col= c(2,3,4,5,6),lty=c(1,1,1,1,1),lwd=c(2,2,2,2,2))


###################################

##### Maximizar una verosimilitud usando optim ######

####Vamos a olvidarnos ahora de que conocemos los parámetros y las 
####distribuciones correspondientes y vamos a encontrar las estimaciones 
####ML usando software (métodos numéricos)

### Doble exponencial

### creamos una lista vaica
MLE_Dif_Muest <- list()
### creamos la matriz donde almacenaremos las estimaciones ML
for (l in 1:5) {
   MLE_Dif_Muest[[l]] <- as.data.frame(matrix(data=NA,nrow=1,ncol=1000))
}

### crearemos un loop para encontar las estimaciones para cada una de 
### muestras 
for (l in 1:5) {
   for (i in 1:1000) {
      fDobExp <- function(theta){
         fdexp=(1/2)*exp(-(abs(Dif_Muest[[l]][,i] - theta)))
         L=-sum(log(fdexp)) #el negativo de la log-verosimilitud
         #return(L)
      }
      #print(fDobExp2(5))
      sol=optim(4,fDobExp) #optim busca el valor que hace mínima una función
      a=sol$par[1]
      MLE_Dif_Muest[[l]][1,i] <- a
   }
}
#######################

### creamos una lista vaica
MOM_Dif_Muest <- list()
### para obtener el estimador de momentos
for (l in 1:5) {
   MOM_Dif_Muest[[l]] <- as.data.frame(colMeans(Dif_Muest[[l]])) 
}

############################

### histogramas para las estimaciones MLE
## crearemos una lista vacia
MLE_dm <- list()
## extraemos las estimaciones del MLE
for (l in 1:5) {
   MLE_dm[[l]] <- as.numeric(MLE_Dif_Muest[[l]])
}
### creamos un interfaz para tener todos los graficos
par(mfrow = c(3,2))
## histogramas
for (t in 1:5) {
   hist(MLE_dm[[t]],prob=TRUE,xlim=c(min(MLE_dm[[t]]),max(MLE_dm[[t]])),
        xlab="datos", ylab="densidad",main="Histograma del MLE",col = t+1)
   ## linea del parametro real
   abline(v = 6,col = "darkgoldenrod2", lwd = 3)
   ## linea del promedio de las estimaciones
   abline(v = mean(MLE_dm[[t]]),col = 'honeydew4', lwd = 2)
}
## para el cuadro de leyendas
hist(0,ylab = NULL,main="Guia de colores",col = "white",axes = FALSE)
legend(locator(1), c("n = 50","n = 100","n =200","n = 500","n = 1000"),
       col= c(2,3,4,5,6),lty=c(1,1,1,1,1),lwd=c(2,2,2,2,2))

### histogramas para las estimaciones MOM
### creamos un interfaz para tener todos los graficos
par(mfrow = c(3,2))
## histogramas
for (t in 1:5) {
   hist(MOM_Dif_Muest[[t]]$`colMeans(Dif_Muest[[l]])`,prob=TRUE,
        xlim=c(min(MOM_Dif_Muest[[t]]$`colMeans(Dif_Muest[[l]])`),
               max(MOM_Dif_Muest[[t]]$`colMeans(Dif_Muest[[l]])`)),
        xlab="datos", ylab="densidad",main="Histograma del MOM",col = t+1)
   ## linea del parametro real
   abline(v = 6,col = "darkgoldenrod2", lwd = 3)
   ## linea del promedio de las estimaciones
   abline(v = mean(MOM_Dif_Muest[[t]]$`colMeans(Dif_Muest[[l]])`),
          col = 'honeydew4', lwd = 2)
}
## para el cuadro de leyendas
hist(0,xlab = NULL, ylab = NULL,main="Guia de colores",col = "white",axes = FALSE)
legend(locator(1), c("n = 50","n = 100","n =200","n = 500","n = 1000"),
       col= c(2,3,4,5,6),lty=c(1,1,1,1,1),lwd=c(2,2,2,2,2))


#############################

## creamos una lista vacia
tablas <- list()
### tabla con la media y la varianza de MLE y MOM
for (l in 1:5) {
   tablas[[l]] <- as.data.frame(matrix(data=NA,nrow=2,ncol=2)) 
   colnames(tablas[[l]]) <- c("MLE","MOM")
   rownames(tablas[[l]]) <- c("Mean","Var")
}
## MLE
for (l in 1:5) {
   ## MLE
   tablas[[l]][,1] <- c(mean(MLE_dm[[l]]),var(MLE_dm[[l]]))
   ##MOM
   tablas[[l]][,2] <- c(mean(MOM_Dif_Muest[[l]]$`colMeans(Dif_Muest[[l]])`),
                        var(MOM_Dif_Muest[[l]]$`colMeans(Dif_Muest[[l]])`))
}
###### genrar tabla en codigo latex
xtable(cbind(tablas[[1]],tablas[[2]],tablas[[3]],tablas[[4]],
             tablas[[5]]),digits = 3,3)

###############################################
#################################
###########################################################

######### parte d ############################

##Vamos a simular 1000 veces 1000 observaciones de una distribución doble 
##exponencial con theta = 6 y betha = 1

### primero crearemos una matriz donde se guardaran las simulaciones
datos_2 <- matrix(data=NA,nrow=1000,ncol=1000)
### crearemos un loop para las simulaciones
for (i in 1:1000) {datos_2[,i] <- rdexp(n=1000,location = 6,scale = 1)}
### ahora para manipulación volver data frame
datos_2 <- as.data.frame(datos_2)

#############################

###### creamos tabla para almacenar le media muestral y mediana
Mean_Median_ <- matrix(data=NA,nrow=2,ncol=1000)
### para obtener la media muestral
Med_mues <- colMeans(datos_2)
Mean_Median_[1,] <- Med_mues
##### para obtener la mediana
for (i in 1:1000) {Mean_Median_[2,i] <- median(datos_2[,i])}

#######################################

###### creamos tabla para almacenar Zn y Mn
Zn_Mn <- matrix(data=NA,nrow=2,ncol=1000)
rownames(Zn_Mn)<- c("Zn","Mn")

######   suceison Zn ##################
for (i in 1:1000) {Zn_Mn[1,i] <- sqrt(1000)*(Mean_Median_[1,i] - 6)}

######   suceison Mn ##################
for (i in 1:1000) {Zn_Mn[2,i] <- sqrt(1000)*(Mean_Median_[2,i] - 6)}

##################################

##### extraer Zn
Zn <- as.numeric(Zn_Mn[1,])
### histograma para la media muestral
hist(Zn,prob=TRUE,xlim=c(min(Zn),max(Zn)),xlab="datos",
     ylab="densidad",main="Histograma del Media Muestral")
### sobreponer la normal con varianza 2
xpts_2<-seq(min(Zn),max(Zn),by=0.001)
lines(xpts_2,dnorm(xpts_2,mean = 0, sd = sqrt(2)),col="red",lwd=2)

####################################

##### extraer Mn
Mn <- as.numeric(Zn_Mn[2,])
### histograma para la media muestral
hist(Mn,prob=TRUE,xlim=c(min(Mn),max(Mn)),xlab="datos",
     ylab="densidad",main="Histograma del Mediana")
### sobreponer la normal con varianza 2
xpts_3<-seq(min(Mn),max(Mn),by=0.001)
lines(xpts_3,dnorm(xpts_3,mean = 0, sd = sqrt(1)),col="red",lwd=2)

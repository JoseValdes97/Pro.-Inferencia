#####Simulaci�n y gr�fica ######

##Vamos a simular 1000 veces 10 observaciones de una distribuci�n doble 
##exponencial con theta = 6 y betha = 1

data1 <-datos$V1 

##Vamos a hacer un histograma y  superponer sobre �l la densidad de una 
# doble exponencial

###prob=TRUE muestra en escala de densidad
#xlim=c(min(data1),max(data1))
hist(data1,prob=TRUE,xlab="datos",
     ylab="densidad",ylim = c(0,0.5),
     main ="Histograma de datos simulados de una doble exponencial");
###sobre poner la densidad de la doble exponencial
xpts<-seq(min(data1),max(data1),by=0.001)
lines(xpts,ddexp(xpts, location = 6, scale = 1),col="red",lwd=2,lty = 5)

##### 2. Maximizar una verosimilitud usando optim ######

### Usaremos data1 
summary(data1)

####Vamos a olvidarnos ahora de que conocemos los par�metros y las 
####distribuciones correspondientes y vamos a encontrar las estimaciones 
####ML usando software (m�todos num�ricos)

### Doble exponencial

fDobExp <- function(theta){
      fdexp=(1/2)*exp(-(abs(datos[,4] - theta)))
      L=-sum(log(fdexp)) #el negativo de la log-verosimilitud
      return(L)
}
fDobExp(5)

sol=optim(4,fDobExp) #optim busca el valor que hace m�nima una funci�n
a=sol$par[1]
a
?mean(datos$V1)

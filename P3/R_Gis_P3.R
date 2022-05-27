library(sdcMicro)
dades <- read.csv(file="~/Documents/data/salaris.csv", header=TRUE, sep=",", colClasses=c("CP"="character"))
#colnames(dades) #nom de les columnes
#summary(dades) #resum atributs 
#head(dades) #primeres 5 files

#dades[1:10,'Edat'] #Visualitzo els 10 primers elements ordenats
#print(paste("Atribut 'Edat': mean value =", mean(dades[, 'Edat']), "and SD =", sd(dades[, 'Edat']), sep=" ")) #Trec la mitja de la columna edat i la sd
#plot(sort(dades[, 'Edat']), type="p", col="red", xlab="Registres", ylab="Valor", main="Edat") #Grafica dels valors Edat ordenat

#Ex_1
dades_sense_identificador<-dades[3:5]
#Ex_2
x<-dades_sense_identificador[1]
y<-dades_sense_identificador[2]
c<- dades_sense_identificador[3]
#which(x==25560) #Resultat posició 18, identificat nomes amb CP.
#which(x==17800 & y==32) #Resultat posició 1, identificat amb CP i edat.
#which(x==43870 & c==78) #Resultat posició 94, identificat amb CP i salari.
#Ex_3
dades.an=addNoise(dades_sense_identificador,variables='Edat',noise=20,method="additive") #Apliquem sorroll additiu a la columna 'Edat' i la guardem a la variable dades.an
plot(cbind(dades$Edat, dades.an$Edat),ylim=c(min(dades$Edat),max(dades$Edat)),xlim=c(min(dades$Edat),max(dades$Edat)),xlab="Original", ylab="Masked", main="Additive Noise - Edat (P=0.20)")
abline(a=0, b=1, col="red")
#Ex_4
dades.rs=rankSwap(dades_sense_identificador,variables='Edat',TopPercent = 10, BottomPercent = 10, K0=NULL, R0=NULL, P=NULL, missing=NA,seed=NULL)
plot(cbind(dades$Edat, dades.rs$Edat),ylim=c(min(dades$Edat),max(dades$Edat)),xlim=c(min(dades$Edat),max(dades$Edat)),xlab="Original", ylab="Masked", main="Metode Pertorbatiu")

#Ex_5
dUtility(obj=dades_sense_identificador['Edat'],xm=dades.an$xm) #mostra la perdua d'informació produida per additive
dUtility(obj=dades_sense_identificador['Edat'],xm=dades.rs$Edat) #mostra la perdua d'informació produida per additive

#Ex_6
dRisk(obj=dades_sense_identificador['Edat'],xm=dades.an$xm)
dRisk(obj=dades_sense_identificador['Edat'],xm=dades.rs['Edat'])

#Ex_7
m1_Edat <- microaggregation(dades_sense_identificador,variables = "Edat", method = "onedims",aggr=3)
m1_Salari <- microaggregation(dades_sense_identificador,variables = "Salari", method = "onedims",aggr=3)
par(mfrow=c(2,2))
hist(dades_sense_identificador$Edat)
hist(dades_sense_identificador$Salari)
hist(m1_Edat$mx$Edat)
hist(m1_Salari$mx$Salari)
m1_Edat_Salari<-microaggregation(obj=dades_sense_identificador,variables=c("Edat","Salari"),method = "mdav",aggr=3)

#Ex_8
generalitzacio_CP <-function(llista_CP){
  for(i in llista_CP){
  aux = substr(i,1,2)}
  nova_llista = paste(aux,"***",sep="")
  return(nova_llista) 
  }
c <- data.frame("CP"=c(17820,08500,17100,43870))
c1 <- data.frame("CP"=c(08240,08500,08018))
generalitzacio_CP(c)
generalitzacio_CP(c1)
generalitzacio_CP(dades_sense_identificador['CP'])

table(dades_sense_identificador['CP'])
registres_perill = dades_sense_identificador[dades_sense_identificador$CP %in% c("08401", "25600","25610"),]
generalitzacio_CP(registres_perill['CP'])

#Ex_9
names <- c("AN", "RS", "MA one", "MA mul")
colors <- c("grey", "red", "blue", "green")
lty <- 1:4
lwd <- 3

dif_an <-abs(dades_sense_identificador[, 'Edat']-dades.an$xm[, 'Edat'])
dif_rs <-abs(dades_sense_identificador[, 'Edat']-dades.rs[, 'Edat'])
dif_ma_one <- abs(dades_sense_identificador[, 'Edat']-m1_Edat$mx[, 'Edat'])
dif_ma_mul <- abs(dades_sense_identificador[, 'Edat']-m1_Edat_Salari$mx[, 'Edat'])
ymin <- min(dif_an, dif_rs, dif_ma_one, dif_ma_mul)
ymax <- max(dif_an, dif_rs, dif_ma_one, dif_ma_mul)
par(mfrow=c(1,1))
plot(sort(dif_an), type="l", col=colors[1], lty=lty[1], lwd=lwd, ylim=c(ymin,ymax), xlab="Registers", ylab="Error", main="Edat")
lines(sort(dif_rs), col=colors[2], lty=lty[2], lwd=lwd)
lines(sort(dif_ma_one), col=colors[3], lty=lty[3], lwd=lwd)
lines(sort(dif_ma_mul), col=colors[4], lty=lty[4], lwd=lwd)
legend(x="topleft", legend=names, col=colors, lty=lty, lwd=lwd)

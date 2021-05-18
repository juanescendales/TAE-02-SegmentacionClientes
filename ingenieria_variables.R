#--------------------------------------
# Porcentaje de los canales que se usa
#--------------------------------------
#-----------------------------
X_fin <- data.frame(apply(datos_fin[,-c(8,9)], 2, as.numeric))
Y_com <- data.frame(apply(datos_comp, 2, as.numeric))
#-----------------------------
dim(X_fin) # Dimension datos financieros
dim(Y_com) # Dimension datos comportamiento
canales <- datos_comp[,colnames(datos_comp)]

# canales de entrada
length(grep("en",colnames(canales)))
en_cal <- canales[,c(grep("en_",colnames(canales)))]
# canales de salida
length(grep("sal",colnames(canales)))
sal_cal <- canales[,c(grep("sal_",colnames(canales)))]

# Reemplazamos los valores distintos de 0 por 1
en_cal_t <- sapply(en_cal,function(x) ifelse(x!=0,1,0)) # canal entrada transformado
sal_cal_t <- sapply(sal_cal,function(x) ifelse(x!=0,1,0)) # canal salida transformado

new_var_1 <- apply(en_cal_t,1,mean) # porcentage de uso de canales de entrada
new_var_2 <- apply(sal_cal_t,1,mean) # porcentage de uso de canales de entrada

Y_com_new <- data.frame(cbind(Y_com,"poc_en"=new_var_1,"poc_sal"=new_var_2))

#--------------------------------------
# variables de X: dobles interacciones
#--------------------------------------

x_int_1 <- X_fin[,1]*X_fin[,2]
x_int_2 <- X_fin[,3]*X_fin[,4]
x_int_3 <- X_fin[,6]*X_fin[,7]
x_int_4 <- X_fin[,8]*X_fin[,9]
x_int_5 <- X_fin[,11]*X_fin[,12]
x_int_6 <- X_fin[,13]*X_fin[,14]

int_2 <- cbind(x_int_1,x_int_2,x_int_3,x_int_4,x_int_5,x_int_6)
X_fin_new <- data.frame(cbind(X_fin,int_2))













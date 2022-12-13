datos <- read.csv("notas.csv", sep = ";", header = TRUE)
attach(datos)
names(datos)

#crear modelo
# crea una capa oculta para buscar, resolver e interpretar
model <- nnet(
  Resultado~N1+N2+N3,
  datos,
  size = 5,
  rang = 0.1,
  decay = 5e-2,
  maxit = 5000
)

#creando red neuronal
#entrea el modelo creado para inferir una posible solucioes
rn <- neuralnet(
  Resultado~N1+N2+N3,
  data = datos,
  hidden = 10,
  act.fct = "logistic",
  linear.output = FALSE,
)

plot(rn)

rn
#Crear datos de prueba para evaluar 
Prueba_N1 = c(18,15,7)
Prueba_N2 = c(7,5,8)
Prueba_N3 = c(12,1,20)

#crear data frame
Prueba <- data.frame(
  Prueba_N1,
  Prueba_N2,
  Prueba_N3
)

# Creadno predicción

Prediccion <- compute(rn, Prueba)


# Verificar los datos obtenidos
Pred_Reco <- ifelse(Prediccion$net.result>0.5,"APROBADO", "DESAPROBADO")

Valor <- data.frame(
  Prediccion$net.result,
  Pred_Reco
)
Valor


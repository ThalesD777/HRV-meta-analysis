# Carregar pacotes
install.packages(c("metafor", "meta", "dmetar", "esc", "ggplot2", "effsize"))
library(effsize)
library(esc)
library(metafor)
library(readxl)
library(meta)
library(ggplot2)


#Read File
Metaregression <- read_excel("D:/Downloads/Metaregression.xlsx")
View(Metaregression)

# Rodar a meta-regressão e salvar o modelo
modelo <- rma(yi = HedgessG,
              vi = Variance,
              mods = ~ DeltaIMC,
              data = Metaregression)

# Criar os valores novos de DeltaIMC para predição
range_vals <- range(Metaregression$DeltaIMC, na.rm = TRUE)

newdata <- data.frame(DeltaIMC = seq(from = range_vals[1],
                                     to = range_vals[2],
                                     length.out = 100))

# Fazer predições a partir do modelo
preds <- predict(modelo, newmods = newdata$DeltaIMC)

# Plotar os dados reais
plot(Metaregression$DeltaIMC,
     Metaregression$HedgessG,
     xlab = "Diferença de IMC entre grupos",
     ylab = "Hedge's g (RMSSD)",
     pch = 19,
     cex = 1.3,
     col = "darkblue",
     main = "Meta-regressão: Efeito do DeltaIMC sobre RMSSD")

# Adicionar linha de tendência
lines(newdata$DeltaIMC, preds$pred, col = "red", lwd = 2)

# Adicionar intervalo de confiança (linhas tracejadas)
lines(newdata$DeltaIMC, preds$ci.lb, lty = "dashed", col = "red")
lines(newdata$DeltaIMC, preds$ci.ub, lty = "dashed", col = "red")

summary(modelo)
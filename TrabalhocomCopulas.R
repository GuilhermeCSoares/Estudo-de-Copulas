## -------------------------------------------------------------------------------------------------------------------
library(readxl)
library(knitr)

data <- read_excel("C:/Users/guilh/Downloads/datacop2.xlsx")
rownames(data) <- c()
data$dolar <- c(NA, diff(data$dolar))
data$juroreal <- c(NA, diff(data$juroreal))
data <- na.omit(data)
kable(head(data))


## -------------------------------------------------------------------------------------------------------------------
typeof(data$ipca)


## -------------------------------------------------------------------------------------------------------------------
library(ggplot2)

ggplot(data, aes(x = ipca)) +
  geom_histogram(binwidth = 0.1, fill = "blue", color = "black") +
  labs(title = "Distribuição de Ipca", x = "Ipca", y = "Frequência")

ggplot(data, aes(x = igpm)) +
  geom_histogram(binwidth = 0.20, fill = "green", color = "black") +
  labs(title = "Distribuição de igpm", x = "igpm", y = "Frequência")

ggplot(data, aes(x = dolar)) +
  geom_histogram(binwidth = 0.05, fill = "red", color = "black") +
  labs(title = "Distribuição de dolar(diff)", x = "dolar", y = "Frequência")

ggplot(data, aes(x = juroreal)) +
  geom_histogram(binwidth = 0.1, fill = "purple", color = "black") +
  labs(title = "Distribuição de juroreal(diff)", x = "juroreal", y = "Frequência")



## -------------------------------------------------------------------------------------------------------------------
ggplot(data, aes(x = ipca, y = igpm)) +
  geom_point() +
  labs(title = "Gráfico de Dispersão", x = "Igpm", y = "Ipca")


## -------------------------------------------------------------------------------------------------------------------
pearson_corr <- cor(data$ipca, data$igpm, method = "pearson")
kendall_corr <- cor(data$ipca, data$igpm, method = "kendall")
spearman_corr <- cor(data$ipca, data$igpm, method = "spearman")

print(paste("Coeficiente de Correlação de Pearson:", pearson_corr))
print(paste("Coeficiente de Correlação de Kendall:", kendall_corr))
print(paste("Coeficiente de Correlação de Spearman:", spearman_corr))



## -------------------------------------------------------------------------------------------------------------------
data_normalized <- as.data.frame(scale(data))
head(data_normalized)


## -------------------------------------------------------------------------------------------------------------------
cdf_ipca <- ecdf(data$ipca)

cdf_igpm <- ecdf(data$igpm)

ipca_values <- seq(min(data_normalized$ipca), max(data_normalized$ipca), length.out = 100)
igpm_values <- seq(min(data_normalized$igpm), max(data_normalized$igpm), length.out = 100)


cdf_ipca_values <- cdf_ipca(ipca_values)
cdf_igpm_values <- cdf_igpm(igpm_values)

plot(ipca_values, cdf_ipca_values, type = "l", col = "blue",
     xlab = "ipca", ylab = "Função de Densidade Acumulada",
     main = "Função de Densidade Acumulada para ipca")


lines(igpm_values, cdf_igpm_values, type = "l", col = "red")

legend("bottomright", legend = c("ipca", "igpm"), col = c("blue", "red"), lty = 1)




## -------------------------------------------------------------------------------------------------------------------
ipca_uniform <- pnorm(data_normalized$ipca)
igpm_uniform <- pnorm(data_normalized$igpm)


uniform_df <- data.frame(ipca_uniform , igpm_uniform)


hist(uniform_df$ipca_uniform, main = "Distribuição Uniforme de ipca ", xlab = "Valor Uniformizado")
hist(uniform_df$igpm_uniform, main = "Distribuição Uniforme de igpm", xlab = "Valor Uniformizado")



## -------------------------------------------------------------------------------------------------------------------
ggplot(uniform_df, aes(x = ipca_uniform, y = igpm_uniform)) +
  geom_point() +
  labs(title = "Gráfico de Dispersão", x = "IPCA", y = "IGPM")



## -------------------------------------------------------------------------------------------------------------------
library(copula)
gaussian_copula <- normalCopula(dim = 2)
gaussian_copula_fit <- fitCopula(gaussian_copula, uniform_df)
print(gaussian_copula_fit)


## -------------------------------------------------------------------------------------------------------------------
rhog <- coef(gaussian_copula_fit)[1]
persp(normalCopula(dim=2,rhog),dCopula)


## -------------------------------------------------------------------------------------------------------------------
ug <- rCopula(nrow(uniform_df),normalCopula(dim=2,rhog))
scatter_plot <- ggplot(uniform_df, aes(x = ipca_uniform, y = igpm_uniform)) +
  geom_point() +
  labs(title = "Gráfico de Dispersão", x = "ipca", y = "igpm")
scatter_plot <- scatter_plot +
  geom_point(data = as.data.frame(ug), aes(x = ug[,1], y = ug[,2]), color = 'blue', size = 1, alpha = 0.5)

print(scatter_plot)



## -------------------------------------------------------------------------------------------------------------------
tstudent_copula <- tCopula(dim = 2)
tstudent_copula_fit <- fitCopula(tstudent_copula, uniform_df)
print(tstudent_copula_fit)



## -------------------------------------------------------------------------------------------------------------------
rho <- coef(tstudent_copula_fit)[1]
df <- coef(tstudent_copula_fit)[2]
persp(tCopula(dim=2,rho,df=df),dCopula)


## -------------------------------------------------------------------------------------------------------------------
u <- rCopula(nrow(uniform_df),tCopula(dim=2,rho,df=df))
# Plotar o gráfico de dispersão dos seus dados
scatter_plot <- ggplot(uniform_df, aes(x = ipca_uniform, y = igpm_uniform)) +
  geom_point(size=1) +
  labs(title = "Gráfico de Dispersão", x = "Ipca", y = "Igpm")

# Adicionar os pontos gerados a partir da cópula t-Student ao gráfico de dispersão
scatter_plot <- scatter_plot +
  geom_point(data = as.data.frame(u), aes(x = u[,1], y = u[,2]), color = 'blue', size = 1, alpha = 0.5)

# Exibir o gráfico combinado
print(scatter_plot)



## -------------------------------------------------------------------------------------------------------------------
clayton_copula <- claytonCopula(dim = 2)
clayton_copula_fit <- fitCopula(clayton_copula, uniform_df)
print(clayton_copula_fit)


## -------------------------------------------------------------------------------------------------------------------
alpha <- coef(clayton_copula_fit)[1]
persp(claytonCopula(dim=2,alpha),dCopula)


## -------------------------------------------------------------------------------------------------------------------
uc <- rCopula(nrow(uniform_df),claytonCopula(dim=2,alpha))
scatter_plot <- ggplot(uniform_df, aes(x = ipca_uniform, y = igpm_uniform)) +
  geom_point() +
  labs(title = "Gráfico de Dispersão", x = "Ipca", y = "Igpm")
scatter_plot <- scatter_plot +
  geom_point(data = as.data.frame(uc), aes(x = uc[,1], y = uc[,2]), color = 'blue', size = 1, alpha = 0.5)
print(scatter_plot)


## -------------------------------------------------------------------------------------------------------------------
frank_copula <- frankCopula(dim = 2)
frank_copula_fit <- fitCopula(frank_copula, uniform_df)
print(frank_copula_fit)



## -------------------------------------------------------------------------------------------------------------------
alphaf <- coef(frank_copula_fit)[1]
persp(frankCopula(dim=2,alphaf),dCopula)


## -------------------------------------------------------------------------------------------------------------------
uf <- rCopula(nrow(uniform_df),frankCopula(dim=2,alphaf))

scatter_plot <- ggplot(uniform_df, aes(x = ipca_uniform, y = igpm_uniform)) +
  geom_point() +
  labs(title = "Gráfico de Dispersão", x = "Ipca", y = "Igpm")

scatter_plot <- scatter_plot +
  geom_point(data = as.data.frame(uf), aes(x = uf[,1], y = uf[,2]), color = 'blue', size = 1, alpha = 0.5)

print(scatter_plot)


## -------------------------------------------------------------------------------------------------------------------
gumbel_copula <- gumbelCopula(dim = 2)
gumbel_copula_fit <- fitCopula(gumbel_copula, uniform_df)
print(gumbel_copula_fit)


## -------------------------------------------------------------------------------------------------------------------
alphag <- coef(gumbel_copula_fit)[1]
persp(gumbelCopula(dim=2,alphag),dCopula)


## -------------------------------------------------------------------------------------------------------------------
ug <- rCopula(nrow(uniform_df),gumbelCopula(dim=2,alphaf))
scatter_plot <- ggplot(uniform_df, aes(x = ipca_uniform, y = igpm_uniform)) +
  geom_point() +
  labs(title = "Gráfico de Dispersão", x = "Ipca", y = "Igpm")
scatter_plot <- scatter_plot +
  geom_point(data = as.data.frame(ug), aes(x = ug[,1], y = ug[,2]), color = 'blue', size = 1, alpha = 0.5)
print(scatter_plot)


## -------------------------------------------------------------------------------------------------------------------
likelihoods <- c(
  gaussian = logLik(gaussian_copula_fit),
  student = logLik(tstudent_copula_fit),
  clayton = logLik(clayton_copula_fit),
  frank = logLik(frank_copula_fit),
  gumbel = logLik(gumbel_copula_fit)
)

likelihood_table <- data.frame(
  Copula = names(likelihoods),
  Log_Likelihood = likelihoods
)

print(likelihood_table)



## -------------------------------------------------------------------------------------------------------------------
data <- data[, c("ipca","igpm" ,"juroreal","dolar")]


## -------------------------------------------------------------------------------------------------------------------
library(copulareg)
set.seed(1)
tr <- sample(c(TRUE, FALSE), nrow(data), TRUE, c(0.8, 0.20))
y_tr <- data$ipca[tr]
y_te <- data$ipca[!tr]
x_tr <- apply(data[tr, -1], 2, as.numeric)
x_te <- apply(data[!tr, -1], 2, as.numeric)
var_type_x <- apply(x_tr, 2,
                   function(x) if(length(unique(x)) > 10) "c" else "c")

md <- copulareg::copulareg(y_tr, x_tr, "c", var_type_x)
pred_copula <- predict(md, new_x = x_te)

modelo_regressao <- lm(ipca ~ +juroreal + dolar + igpm, data = data[tr,])
pred_regressao <- predict(modelo_regressao, newdata = data[!tr, -1])



## -------------------------------------------------------------------------------------------------------------------
# Criar o plot
plot(y_te, col = "black", type = "b", pch = 16, ylim = range(c(y_te, pred_copula, pred_regressao)), 
     main = "Comparação das Previsões dos Modelos com Valores Reais", 
     xlab = "Índice", ylab = "Valores")
lines(pred_copula, col = "red", type = "b", pch = 16)
lines(pred_regressao, col = "blue", type = "b", pch = 16)
legend("topright", legend = c("Valores Reais", "Modelo de Copula", "Modelo de Regressão Linear"), 
       col = c("black", "red", "blue"), pch = 16, lty = 1, cex = 0.8)


## -------------------------------------------------------------------------------------------------------------------
mse_copula <- mean((pred_copula - y_te)^2)
cat("Erro Quadrático Médio do Modelo de Copula:", mse_copula, "\n")
mse_regressao <- mean((pred_regressao - y_te)^2)
cat("Erro Quadrático Médio do Modelo de Regressão Linear:", mse_regressao, "\n")


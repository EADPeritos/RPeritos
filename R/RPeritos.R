#' Funcao para testar micronumerosidade da amostra e indicar quantidade de elementos amostrais
#'
#' Função para calcular e classificar o grau de fundamentação quanto a micronumerosidade dos dados amostrais e indicar a quantidade ideal de dados
#' @param data Dataframe do conjunto de dados
#' @return Indica a quantidade ideal de elementos amostrais e classifica com o grau de fundamentacao correspondente
#' @examples
#' micronum(data);
#' @export
micronum <- function(data) {
   cat('\n--------------------------------------------------------\n')
   n <- nrow(data)
   v <- ncol(data) - 1

   cat(' Elementos amostrais: ', n)
   cat('\n Variaveis explicativas: ', v)

   g1 <- 3 * (v + 1)
   g2 <- 4 * (v + 1)
   g3 <- 6 * (v + 1)

   cat('\n\n Classificação atual:')
   if (n <= g1) {
      cat(' Grau I')
   } else if (n <= g2) {
      cat(' Grau II')
   } else {
      cat(' Grau III')
   }

   cat('\n\n Quantidade ideal de elementos amostrais')
   cat('\n Grau I  : ', g1)
   cat('\n Grau II : ', g2)
   cat('\n Grau III: ', g3)
   cat('\n\n')

}


#' Funcao para predizer valores de bens imoveis
#'
#' Função para predicao de valores de bens imoveis via regressao linear
#' @param model O modelo calculado por lm()
#' @return Apresenta a funcao para precificacao de bens imoveis
#' @examples
#' fpred(modelo);
#' @export
fpred <- function(model) {
   cat('\n--------------------------------------------------------\n')
   cat("Valor = \n")
   for (i in 2:length(model$coefficients)) {
      if (model$coefficients[i] > 0) {
         cat("      + ",model$coefficients[i] , "*", names(model$coefficients)[i],'\n')
      } else {
         cat("       ",model$coefficients[i] , "*", names(model$coefficients)[i],'\n')

      }
   }
}

#' Intervalos e Campo arbitrio
#'
#' Calcula os intervalos de confianca e o campo de arbitrio
#' @param modelo O modelo calculado por lm()
#' @param avaliando Dataframe com os dados do bem avaliando
#' @return Valores dos intervalos de confianca (superior e inferior) e o campo de arbitrio para o valor estimado do bem imovel
#' @examples
#' iConf_CA(modelo, avaliando);
#' @export
iConf_CA <- function(modelo, avaliando) {
   pred <- predict(modelo, avaliando, interval = "confidence")
   cat('\n--------------------------------------------------------\n')
   cat("Valor estimado: ", pred[1],'\n')
   cat("\nIntervalos de Confiança")
   cat("\n I.C. Superior", pred[3])
   cat("\n I.C. Inferior", pred[2])
   cat("\n\n")
   cat("\nCampo de Arbítrio ")
   cat("\n Inferior: ", pred[1] * 0.85)
   cat("\n Superior: ", pred[1] * 1.15,'\n\n')
}


#' Análise de Significancia das variaveis e do modelo
#'
#' Calcula a significancia estatistica das variaveis e do modelo
#' @param model O modelo calculado por lm()
#' @param data Dataframe dos dados
#' @return Retorna a significancia das variaveis e do modelo e classifica referente ao Grau de Fundamentacao definido na NBR 14.653-2
#' @examples
#' niveisRM(model, data);
#' @export
niveisRM <- function(model, data) {
   dff <- nrow(data) - nrow(as.data.frame(summary(model)$coefficients[,3]))
   nr <- nrow(as.data.frame(summary(model)$coefficients[,3]))

   # testando 10%
   col_10p <- rep(0, nr)
   col_10p_aux <- rep(0, nr)
   for (i in 1:nr) {
      if (i == 1) {
         col_10p[i] <- " - "
      } else {
         if (abs(as.data.frame(summary(model)$coefficients[i,3])) > qt(0.95, df = dff)) {
            col_10p[i] <- " * "
            col_10p_aux[i] <- 1
         } else {
            col_10p[i] <- " - "
         }
      }
   }

   # testando 20%
   col_20p <- rep(0, nr)
   col_20p_aux <- rep(0, nr)
   for (i in 1:nr) {
      if (i == 1) {
         col_20p[i] <- " - "
      } else {
         if (abs(as.data.frame(summary(model)$coefficients[i,3])) > qt(0.90, df = dff)) {
            col_20p[i] <- " * "
            col_20p_aux[i] <- 1
         } else {
            col_20p[i] <- " - "
         }
      }
   }

   # testando 30%
   col_30p <- rep(0, nr)
   col_30p_aux <- rep(0, nr)
   for (i in 1:nr) {
      if (i == 1) {
         col_30p[i] <- " - "
      } else {
         if (abs(as.data.frame(summary(model)$coefficients[i,3])) > qt(0.85, df = dff)) {
            col_30p[i] <- " * "
            col_30p_aux[i] <- 1
         } else {
            col_30p[i] <- " - "
         }
      }
   }

   cat('\n--------------------------------------------------------\n')
   cat("Analise Significancia das Variaveis - Teste t Student \n")
   cat('\n')

   dt <- cbind(as.data.frame(summary(model)$coefficients[,3]), col_10p, col_20p, col_30p)
   names(dt) <- c(" t value ", " 10% ", " 20% ", " 30% ")
   print(dt)

   cat("\n\nGL: ", dff, '\n')

   t10 <- sum(col_10p_aux)
   t20 <- sum(col_20p_aux)
   t30 <- sum(col_30p_aux)

   if (t10 > t20 && t10 > t30) {
      cat("Grau III")
   }

   if (t20 > t10 && t20 > t30) {
      cat("Grau II")
   } else {
      cat("Grau I")
   }


   cat('\n')
   cat('\n--------------------------------------------------------\n')
   cat('Analise Significancia Modelo - Teste F Snedecor\n\n')

   ff <- summary(modelo)$fstatistic[1]
   dff1 <- summary(modelo)$fstatistic[2]
   dff2 <- summary(modelo)$fstatistic[3]

   cat("F-statistic: ", ff, ' GLn: ', dff1, ' GLd: ', dff2, '\n')

   # testando 1%
   if (ff > qf(0.99, dff1, dff2)) {
      cat("Grau III\n\n")
   }
   # testando 2%
   if (ff > qf(0.98, dff1, dff2)) {
      cat("Grau II\n\n")
   }
   # testando 5%
   if (ff > qf(0.95, dff1, dff2)) {
      cat("Grau I\n\n")
   }

}


#' Amplitude e Grau de Precisao
#'
#'  Calcula a amplitude e classifica quanto ao grau de precisao definido na NBR 14.653-2
#' @param modelo O modelo calculado por lm()
#' @param avaliando Dataframe com os dados do bem avaliando
#' @return Valores de amplitude e Grau de Precisao
#' @examples
#' ampModelo(modelo, avaliando);
#' @export
ampModelo <- function(modelo, avaliando) {
   predito <- predict(modelo, avaliando, interval="confidence", level = 0.80)
   px <- ((predito[3] - predito[2]) / predito[1]) * 100
   cat('\n--------------------------------------------------------\n')
   cat('Analise de amplitude (80%)\n')
   cat("\nAmplitude: ", round(px,2), "%\n")

   if (px <= 50) {
      gr <- 1
   }

   if (px <= 40 ) {
      gr <- 2
   }

   if (px <= 30) {
      gr = 3
   }

   switch (gr,
           cat("Grau I\n\n"),
           cat("Grau II\n\n"),
           cat("Grau III\n\n")
   )

}


#' Teste de Chauvenet
#'
#' Realiza o teste de Chauvenet para um vetor de dados.
#' @param dados Vetor de dados com a variavel que sera analisada
#' @return Retorna os valores do conjunto de dados que precisam ser removidos
#' @examples
#' teste.chauvenet(dados$Valor)
#' @export
teste.chauvenet <- function(dados) {
   cat('\n--------------------------------------------------------\n')
   cat('Teste de Chauvenet\n')

   valn <- c(2,3,4,5,6,7,8,9,10,12,15,20,25,30,35,40,50,75,100,200,500)
   valc <- c(1.15,1.38,1.54,1.65,1.73,1.80,1.86,1.91,1.96,2.04,2.13,2.24,2.33,2.40,2.45,2.50,2.58,2.71,2.81,3.02,3.29)

   nn <- length(dados)
   cat('\n Amostra: ', nn)

   remv <- rep(0, nn)
   crt <- valc[which(valn >= nn)[1]]
   cat('\n Dmax: ', crt, '\n\n')
   for(i in 1:nn) {
      if ((abs(dados[i] - mean(dados)) / sd(dados)) > crt) {
         remv[i] <- "remover"
      } else { remv[i] <- " --- "}
   }
   data.frame(cbind(dados, remv))

}

#' Versao do RPeritos
#'
#' @return Demonstra a versao atual da ferramenta RPeritos
#' @examples
#' versao()
#' @export
versao <- function() {
   vv <- "v:23.9.1"
   return(vv)
}


#' Teste do Desvio Padrao
#'
#' Realiza o teste do desvio padrao para um vetor de dados.
#' @param dados Vetor de dados com a variavel que sera analisada
#' @return Retorna os valores do conjunto de dados que precisam ser removidos e encontram-se acima ou abaixo de dois desvios padroes assim como o grafico informando os pontos discrepantes.
#' @examples
#' teste.dp(dados$Valor)
teste.dp <- function(dados) {
   cat('\n--------------------------------------------------------\n')
   cat('Teste do Desvio Padrao\n\n')
   nn <- length(dados)
   remv <- rep(0, nn)
   for(i in 1:nn) {
      if ((dados[i] > (mean(dados) + (2*sd(dados))))  || (dados[i] < (mean(dados) - (2*sd(dados))))  ) {
         remv[i] <- "remover"
      } else { remv[i] <- " --- "}
   }
   plot(dados, ylim=c(min(dados)-3*sd(dados), max(dados)+3*sd(dados)))
   abline(h=mean(dados) - 2*sd(dados), col='blue')
   abline(h=mean(dados) + 2*sd(dados), col='blue')
   data.frame(cbind(dados, remv))
}


#' Analisar
#'
#' Realiza a analise dos dados
#' @param dados Vetor de dados com as variaveis que serão analisadas
#' @return Retorna informacoes descritvas e classificacoes diversas
#' @examples
#' analisar(dados)
analisar <- function(data) {
   cat("-----------------------------------------------------------------\n")
   cat("TIPOS DE VARIÁVEIS\n\n")
   for (i in 1:length(names(data))) {

      if ((names(data)[i] == "Valor") || names(data)[i] == "valor") {
         cat("[ D/R ]", names(data)[i], "\n")
      } else {
         cat("[ I/E ]", names(data)[i], "\n")
      }
   }

   cat("\n\nLegenda:\n[ I/E ] Variáveis Independentes/Explicativas\n[ D/R ] Variável Dependente/Resposta\n\n")

   cat("-----------------------------------------------------------------\n")
   cat("MODELO ESTATÍSTICO TEÓRICO\n\n")

   var_ind <- rep(0, length(names(data)) -1)

   for (i in 1:length(names(data))) {
      if ((names(data)[i] == "Valor") || names(data)[i] == "valor") {
         var_model <- names(data)[i]
      } else {
         var_ind[i] <- names(data)[i]
      }
   }


   cat(var_model, " = " )
   for (i in 1:length(var_ind)) {
      cat(" + ", var_ind[i])
   }

   cat("\n\n Utilize a função lm() para criar o modelo\n\nmodelo <- lm(valor, variavel1 + variavel2, data = dados)\n")
   cat("-----------------------------------------------------------------\n")


}


calcular <- function(modelo, dados, avaliando) {
   cat("-----------------------------------------------------------------\n")
   cat(" RELATORIO TECNICO DO MODELO ESTATISTICO\n\n")

   cat("Valor estimado do bem avaliando: R$ ", as.numeric(predict.lm(modelo, avaliando)), "\n\n")

   print(summary(modelo))

   cat("-----------------------------------------------------------------\n")
   cat(" ESTATÍSTICAS E TOMADA DE DECISÃO\n\n")
   modelo_summary <- summary(modelo)
   cat(" R2: ", summary(modelo)$r.squared)
   cat(" \nR2 ajustado: ", summary(modelo)$adj.r.squared)
   cat(" \nEstatistica F: ", modelo_summary$fstatistic[1])
   cat(" \nEstatistica F: ", modelo_summary$fstatistic[1])
   cat(" \nGraus de liberdade: ", modelo_summary$fstatistic[2], ", ", modelo_summary$fstatistic[3])
   f <- summary(modelo)$fstatistic
   p <- pf(f[1],f[2],f[3],lower.tail=F)
   cat(" \np-valor: ", p)

   cat("\n\n-----------------------------------------------------------------\n")
   cat(" FUNÇÃO PREDITIVA\n\n")
   print(RPeritos::fpred(modelo))

   cat("\n\n-----------------------------------------------------------------\n")
   cat(" ANALISE DE MICRONUMEROSIDADE DOS DADOS\n\n")
   print(RPeritos::micronum(dados))

   cat("\n\n-----------------------------------------------------------------\n")
   cat(" ANALISE VALORES DISCREPANTES (DISTANCIA DE COOK)\n\n")
   par(mfrow=c(2,2))
   print(cooks.distance(modelo))
   cooksd <- cooks.distance(modelo)
   plot(cooksd, main="Pontos influentes (Distancia Cook)", ylim = c(0,1))
   abline(h = 1, col="red", lwd=2)

   cat("\n\n-----------------------------------------------------------------\n")
   cat(" ANALISE DOS RESIDUOS (DURBIN-WATSON\n\n")
   print(durbinWatsonTest(modelo))
   dw <- durbinWatsonTest(modelo)
   if (dw$p > 0.05) {
      cat("\nOs resíduos analisados são independentes e sem autocorrelação")
   } else {
      cat("\nOs resíduos analisados são dependentes e apresentam autocorrelação")

   }
   plot(modelo$residuals, main="Análise dos resíduos")

   cat("\n\n-----------------------------------------------------------------\n")
   cat(" ANALISE DE MICRONUMEROSIDADE DOS DADOS\n\n")
   print(RPeritos::micronum(dados))

   cat("\n\n-----------------------------------------------------------------\n")
   cat(" ANALISE DE SIGNIFICANCIA\n\n")
   print(RPeritos::niveisRM(modelo, dados))


   cat("\n\n-----------------------------------------------------------------\n")
   cat(" AVALIAÇÃO INTERVALAR\n\n")
   print(RPeritos::iConf_CA(modelo, avaliando))
   plot(predict(modelo, dados), main = "Intervalos de confiança")
   intervalos <- predict(modelo, dados, interval = "confidence")
   points(intervalos[,2], col='blue', pch=19, type='l')
   points(intervalos[,3], col='blue', pch=19, type='l')
   int_pred <- predict(modelo, dados, interval = "prediction")
   points(int_pred[,2], col='red', pch=19, type='l', lty=2)
   points(int_pred[,3], col='red', pch=19, type='l', lty=2)

   plot(predict.lm(modelo, avaliando), main = "Campo de arbítrio")
   ca1 <- as.numeric(predict.lm(modelo, avaliando)) * 0.85
   ca2 <- as.numeric(predict.lm(modelo, avaliando)) * 1.15
   abline(h = ca1, lwd = 1, col='red')
   abline(h = ca2, lwd = 1, col='red')

   par(mfrow = c(1,1))

   cat("\n\n-----------------------------------------------------------------\n")
   cat(" AMPLITUDE\n\n")
   print(RPeritos::ampModelo(modelo, avaliando))

}


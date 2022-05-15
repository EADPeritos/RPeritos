#' Funcao para predizer valores de bens imoveis
#'
#' Função para predicao de valores de bens imoveis via regressao linear
#' @param model O modelo calculado por lm()
#' @return Apresenta a funcao para precificacao de bens imoveis
#' @examples
#' fpred(modelo);
#' @export
fpred <- function(model) {
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
   cat('eadperitos.com')
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

   cat('eadperitos.com')
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
   cat('eadperitos.com')
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
   cat('eadperitos.com')
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



#' Teste do Desvio Padrao
#'
#' Realiza o teste do desvio padrao para um vetor de dados.
#' @param dados Vetor de dados com a variavel que sera analisada
#' @return Retorna os valores do conjunto de dados que precisam ser removidos e encontram-se acima ou abaixo de dois desvios padroes assim como o grafico informando os pontos discrepantes.
#' @examples
#' teste.dp(dados$Valor)
teste.dp <- function(dados) {
   cat('eadperitos.com')
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



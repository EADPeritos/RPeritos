#' Função para predizer valores de bens imóveis
#'
#' Função para predição de valores de bens imóveis via regressão linear
#' @param model O modelo calculado por lm()
#' @return Apresenta a função para precificação de bens imóveis
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

#' Intervalos e Campo arbítrio
#'
#' Calcula os intervalos de confiança e o campo de arbítrio
#' @param modelo O modelo calculado por lm()
#' @param avaliando Dataframe com os dados do bem avaliando
#' @return Valores dos intervalos de confiança (superior e inferior) e o campo de arbítrio para o valor estimado do bem imóvel
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


#' Análise de Significancia das variáveis e do modelo
#'
#' Calcula a significancia estatística das variáveis e do modelo
#' @param model O modelo calculado por lm()
#' @param data Dataframe dos dados
#' @return Retorna a significancia das variaveis e do modelo e classifica referente ao Grau de Fundamentação definido na NBR 14.653-2
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

#' Amplitude e Grau de Precisão
#'
#'  Calcula a amplitude e classifica quanto ao grau de precisão definido na NBR 14.653-2
#' @param model O modelo calculado por lm()
#' @param data Dataframe com os dados do bem avaliando
#' @return Valores de amplitude e Grau de Precisão
#' @examples
#' ampModelo(model, avaliando);
#' @export
ampModelo <- function (model, ava) {
   cat('eadperitos.com')
   cat('\n--------------------------------------------------------\n')
   cat('Análise de amplitude (80%)\n')
   pp <- predict(model, ava, interval="confidence", level = 0.80)

   px <- ((pp[3] - pp[2]) / pp[1]) * 100

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

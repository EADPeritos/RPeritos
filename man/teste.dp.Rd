% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RPeritos.R
\name{teste.dp}
\alias{teste.dp}
\title{Teste do Desvio Padrao}
\usage{
teste.dp(dados)
}
\arguments{
\item{dados}{Vetor de dados com a variavel que sera analisada}
}
\value{
Retorna os valores do conjunto de dados que precisam ser removidos e encontram-se acima ou abaixo de dois desvios padroes assim como o grafico informando os pontos discrepantes.
}
\description{
Realiza o teste do desvio padrao para um vetor de dados.
}
\examples{
teste.dp(dados$Valor)
}

# ALGORITMO METROPOLIS - HASTINGS: CÓDIGO EM R



# PACOTES UTILIZADOS ----

library(tidyverse)
library(readr)
library(stringi)
library(stringr)
library(purrr)
library(glue)


# LEITURA DO TEXTO DE REFERÊNCIA ----

livros_referencia <- read_file("livros_Carl_Sagan.txt") 



# MANIPULAÇÃO DO TEXTO ----

livros_referencia <- livros_referencia |> 
                 str_to_lower() |> # letra minúscula
                 gsub(pattern = "[^A-Za-z ]+", replacement = "", x=_) |> 
                 stri_trans_general(id = "Latin-ASCII")



# PARES DE LETRAS ----
pares_letras <- function(texto){
  indice_inicial <- 1 : (nchar(texto) - 1)
  indice_final <- indice_inicial + 1
  return(stri_sub(texto, from = indice_inicial,
                         to = indice_final))
}


texto_ref_pares_letras <- pares_letras(livros_referencia)

# Verificando alguns pares obtidos
texto_ref_pares_letras[10000:10030]


# Combinações mais prováveis
matriz_probabilidade <-
  table(texto_ref_pares_letras) / length(texto_ref_pares_letras)


# Probabilidade estimada - Top 10
matriz_probabilidade  %>% 
  sort(decreasing = TRUE)  %>% 
  head(10)




# INCLUSÃO DE COMBINAÇÕES NÃO ENCONTRADAS ----
matriz_probabilidade_final <- function(par_letras){
  probabilidades <- matriz_probabilidade[par_letras]
  
  if (is.na(probabilidades)) {
    return(1 / length(texto_ref_pares_letras))
  } else{
    return(probabilidades)
  }
}


# LOG-VEROSSIMILHANÇA - SCORE 
log_vero_texto <- function(texto){
    texto  %>% 
    pares_letras()  %>% 
    map_dbl(matriz_probabilidade_final)  %>% 
    log()  %>% 
    sum()
}

log_vero_texto("texto em lingua portuguesa")
log_vero_texto("fghrx gh wghdfr etfsasrcva")



# GERAÇÃO DE UMA CODIFICAÇÃO CIPHER ----

# Gerando um cipher permutando as letras do alfabeto
gerar_cipher <- function() sample(letters, replace = FALSE)

# Codificando um texto usando um cipher
codificar_texto <- function(texto, cipher) {
    chartr(x = texto,
    old = paste(letters, collapse = ""),
    new = paste(cipher, collapse = "")
  )
}

# Decodificando um texto a partir de um cipher fornecido
decodificar_texto <- function(texto_codificado, cipher) {
  chartr( x = texto_codificado,
    old = paste(cipher, collapse = ""),
    new = paste(letters, collapse = "")
  )
}



# FUNÇÃO PARA ATUALIZAÇÃO DA CODIFICAÇÃO ATUAL ----

atualizacao <- function(x){
  # Select two distinct indices
  indices_aleatorizacao <- sample(1:length(x), size = 2, replace=FALSE)
  elemento_1 <- x[indices_aleatorizacao[1]]
  elemento_2 <- x[indices_aleatorizacao[2]]
  
  x[indices_aleatorizacao[1]] <- elemento_2
  x[indices_aleatorizacao[2]] <- elemento_1
  
  return(x)
}


# ALGORITMO METROPOLIS-HASTINGS - Exemplo 1 ----

# Mensagem que queremos obter
mensagem <- "olhem de novo esse ponto. e aqui, e a nossa casa, somos nos. nele, todos a quem ama, todos a quem conhece, qualquer um sobre quem voce ouviu falar, cada ser humano que ja existiu, viveram as suas vidas. nao ha, talvez, melhor demonstracao da tola presuncao humana do que esta imagem distante do nosso minusculo mundo. para mim, destaca a nossa responsabilidade de sermos mais amaveis uns com os outros, e para preservarmos e protegermos o palido ponto azul, o unico lar que conhecemos ate hoje."


# Gerar um cipher aleatorio para ser o verdadeiro cipher
cipher_original <- gerar_cipher()
cipher_original

# Codificando a mensagem
texto_codificado <- codificar_texto(texto = mensagem,
                                    cipher = cipher_original)
texto_codificado

# Criacao de um cipher aleatorio para iniciar a Cadeia de Markov 
cipher_atual <- gerar_cipher()
cipher_atual

# Contagem de aceitacao
i <- 0

for (iter in 1:10000) {
  
  # Proposta de um novo cipher - atualização de duas letras no cipher atual
  cipher_proposta <- atualizacao(cipher_atual)
  
  # texto decodificado do cipher proposto
  texto_decodificado_proposta <- decodificar_texto(texto_codificado,
                                       cipher = cipher_proposta)
  
  # texto decodificado do cipher atual
  texto_decodificado_atual <- decodificar_texto(texto_codificado,
                                      cipher = cipher_atual)
  
  # Log-verossilhança - texto decodificado do cipher proposto
  log_vero_proposta <- log_vero_texto(texto_decodificado_proposta)
  
  # Log-verossilhança - texto decodificado do cipher atual
  log_vero_atual <- log_vero_texto(texto_decodificado_atual)
  
  # Probabilidade de aceitação - Metropolis Hastings
  prob_aceitacao <- min(1, exp(log_vero_proposta - log_vero_atual))
  
  # aceitacao or not with probability given by `prob_aceitacao`
  aceitacao <- sample(c(TRUE, FALSE),
                   size=1, prob = c(prob_aceitacao, 1-prob_aceitacao))
  
  # Se aceitarmos, então o cipher atual é atualizado com o cipher proposto
  if (aceitacao) {
    cipher_atual <- cipher_proposta
    
    # Print do texto codificado quando houver aceitacao
    print(glue("Iteração {i}: {texto_decodificado_proposta}"))
    
    # Incremento na contagem de aceitacao
    i <- i + 1
  }
}



# ALGORITMO METROPOLIS-HASTINGS - Exemplo 2 ----

# Mensagem que queremos obter
mensagem <- "minha terra tem palmeiras
             onde canta o sabia,
             as aves, que aqui gorjeiam,
             nao gorjeiam como la.

             nosso ceu tem mais estrelas,
             nossas varzeas tem mais flores,
             nossos bosques tem mais vida,
             nossa vida mais amores.

             em cismar, sozinho, a noite,
             mais prazer encontro eu la;
             minha terra tem palmeiras,
             onde canta o sabia.

             minha terra tem primores,
             que tais não encontro eu ca;
             em cismar – sozinho, a noite –
             mais prazer encontro eu la;
             minha terra tem palmeiras,
             onde canta o sabia.

             não permita deus que eu morra,
             sem que eu volte para la;
             sem que desfrute os primores
             que nao encontro por ca;
             sem qu’inda aviste as palmeiras,
             onde canta o sabia."


# Gerar um cipher aleatorio para ser o verdadeiro cipher
cipher_original <- gerar_cipher()

# Codificando a mensagem
texto_codificado <- codificar_texto(texto = mensagem,
                                    cipher = cipher_original)

# Criacao de um cipher aleatorio para iniciar a Cadeia de Markov 
cipher_atual <- gerar_cipher()

# Contagem de aceitacao
i <- 0

for (iter in 1:10000) {
  
  # Proposta de um novo cipher - atualização de duas letras no cipher atual
  cipher_proposta <- atualizacao(cipher_atual)
  
  # texto decodificado do cipher proposto
  texto_decodificado_proposta <- decodificar_texto(texto_codificado,
                                                   cipher = cipher_proposta)
  
  # texto decodificado do cipher atual
  texto_decodificado_atual <- decodificar_texto(texto_codificado,
                                                cipher = cipher_atual)
  
  # Log-verossilhança - texto decodificado do cipher proposto
  log_vero_proposta <- log_vero_texto(texto_decodificado_proposta)
  
  # Log-verossilhança - texto decodificado do cipher atual
  log_vero_atual <- log_vero_texto(texto_decodificado_atual)
  
  # Probabilidade de aceitação - Metropolis Hastings
  prob_aceitacao <- min(1, exp(log_vero_proposta - log_vero_atual))
  
  # aceitacao or not with probability given by `prob_aceitacao`
  aceitacao <- sample(c(TRUE, FALSE),
                      size=1, prob = c(prob_aceitacao, 1-prob_aceitacao))
  
  # Se aceitarmos, então o cipher atual é atualizado com o cipher proposto
  if (aceitacao) {
    cipher_atual <- cipher_proposta
    
    # Print do texto codificado quando houver aceitacao
    print(glue("Iteração {i}: {texto_decodificado_proposta}"))
    
    # Incremento na contagem de aceitacao
    i <- i + 1
  }
}



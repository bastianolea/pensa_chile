library(dplyr)
library(purrr)
library(furrr)
library(tidytext)
library(beepr)
library(tokenizers)

datos_prensa <- arrow::read_feather("datos/prensa_datos.feather")

prueba2 <- datos_prensa |> 
  slice(1:100)

# prueba <- datos_prensa$cuerpo_limpio[1:100]
# 
# tokens_a <- tokenize_words(prueba)
# 
# prueba2 <- tibble("texto" = prueba) 


bench::mark(iterations = 10, check = F,
            "tokenizer" = tokenizers::tokenize_words(prueba),
            "tidytext" = unnest_tokens(prueba2, input = texto, output = "palabra"),
            "strsplit" = strsplit(prueba, " "),
            "stringr" = stringr::str_split(prueba, " ")
)

datos_prensa


prueba2 |> 
  mutate(palabras = tokenizers::tokenize_words(texto)) |> 
  select(palabras) |> 
  tidyr::unnest(palabras)


bench::mark(iterations = 20, check = F,
            "tidytext" = prueba2 |> 
              mutate(palabra = tokenizers::tokenize_words(cuerpo_limpio)) |> 
              tidyr::unnest(palabra),
            "tokenizers" = prueba2 |> 
              unnest_tokens(output = palabra, input = cuerpo_limpio,
                            token = "words", drop = T)
)


prensa_limpia <- arrow::read_feather("~/Collahuasi/seguimiento-scraping/P4_prensa/scraping/resultados_scraping/prensa_limpia.feather")

prensa |> 
  pluck("soyiquique") |> pull(titulo)


prensa_limpia |> 
  pluck("soyiquique") |> 
  head() |> 
  pull(titulo)

prensa_limpia |> 
  pluck("soyiquique")

#para diagnosticar problema con titutlos pegados
prensa_limpia |> 
  bind_rows() |> 
  group_by(fuente) |> 
  summarize(n = mean(nchar(titulo))) |> 
  arrange(desc(n)) |> 
  #slice(1:8) |> pull(fuente) |> dput()
  print(n=Inf)

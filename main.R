funcs <- list.files("R/")

for(f in funcs){
  source(paste0("R/",f))
}

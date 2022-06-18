# this need to be run in order to load all the functions used.

funcs <- list.files("R/")

for(f in funcs){
  source(paste0("R/",f))
}

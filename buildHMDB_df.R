library(xml2)
library(tidyverse)

urine <- read_xml("hmdb_metabolites.xml")

xml_name(urine)
num_cpd <- xml_children(urine) %>% length()

test <- xml_child(urine, search=1)

IDs <- xml_children(test) %>% 
  xml_name()

Date <- map(xml_children(urine), function(x) xml_child(x, search=3)) %>% 
  map(., function(x) xml_contents(x)) %>% 
  map_chr(., function(x) as.character(x))
  
ID <- map(xml_children(urine), function(x) xml_child(x, search=4)) %>% 
  map(., function(x) xml_contents(x)) %>% 
  map_chr(., function(x) as.character(x))

parse_KEGG <- function (x){
  if (is_empty(x)) return(NA)
  return(as.character(x))
}

KEGG <- map(xml_children(urine), function(x) xml_child(x, search=37)) %>% 
  map(., function(x) xml_contents(x)) %>% 
  map_chr(parse_KEGG)

METLIN <- map(xml_children(urine), function(x) xml_child(x, search=43)) %>% 
  map(., function(x) xml_contents(x)) %>% 
  map_chr(parse_KEGG)

Name <- map(xml_children(urine), function(x) xml_child(x, search=6)) %>% 
  map(., function(x) xml_contents(x)) %>% 
  map_chr(., function(x) as.character(x))

CID <- map(xml_children(urine), function(x) xml_child(x, search=44)) %>% 
  map(., function(x) xml_contents(x)) %>% 
  map_chr(parse_KEGG)

Formula <- map(xml_children(urine), function(x) xml_child(x, search=9)) %>% 
  map(., function(x) xml_contents(x)) %>% 
  map_chr(parse_KEGG)

Mass <- map(xml_children(urine), function(x) xml_child(x, search=11)) %>% 
  map(., function(x) xml_contents(x)) %>% 
  map_chr(parse_KEGG) %>% 
  as.numeric()

find_logp <- function(x){
   
  pos <- xml_children(x) %>% 
    grep('logp(.)*ChemAxon',.)
  
  if (is_empty(pos)) { 
    return(NA)
  } 
  else {
    val <- xml_child(x, search=pos) %>% 
      xml_child(search=2) %>% 
      xml_double() 
      #as.numeric()
    return(val)
  }
}

logp <- map(xml_children(urine), function(x) xml_child(x, search=22)) %>% 
  map_dbl(~ find_logp(.))

HMDB <- data.frame(Name, ID, KEGG, METLIN, CID, Formula, Mass, logp, stringsAsFactors = F)

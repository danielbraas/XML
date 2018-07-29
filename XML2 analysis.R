library(xml2)
library(XML)
library(tidyverse)

setwd("C:/Users/Daniel/Dropbox/R_projects/XML2")

urine <- read_xml("urine_metabolites.xml")

xml_name(urine)
num_cpd <- xml_children(urine) %>% length()

test <- xml_child(urine, search=1)

IDs <- xml_children(test) %>% 
  xml_name()

Date <- map(xml_children(urine), function(x) xml_child(x, search=3)) %>% 
  map(., function(x) xml_contents(x)) %>% 
  map_chr(., function(x) as.character(x))
  
HMDB <- map(xml_children(urine), function(x) xml_child(x, search=4)) %>% 
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

find_logp

xml_child(urine) %>% xml_child(search=22) %>% xml_children() %>% grep('logp(.)*ChemAxon',.)
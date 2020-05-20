library(glue)
library(purrr)
library(magrittr)
library(stringr)

files = list.files(pattern = ".Rmd$", recursive = T)

make_tile <- function(path) {
  
  yaml <- rmarkdown::yaml_front_matter(path)
  
  link <- stringr::str_replace(path, "\\.Rmd", ".html")
  
  if(str_detect(path, "Steve|Caroline")) {
    link <- str_replace_all(link, " ", "-")
  }
  
  #Don't have an html file, or it's not named the same
  if(!file.exists(link)) {return(NULL)}
  
  display_page <- xml2::read_html(link)
  
  pic_options = display_page %>% 
    rvest::html_nodes("img") %>% 
    discard(map(., ~rvest::html_attr(., "class")) == "photo")
  
  is_frontpage <- pic_options %>% 
    rvest::html_attr("frontpage") %>% 
    is.na() %>% 
    not()
  
  
  
  #If no tagged frontpage image, just pick first non-photo image in html file
  if (any(is_frontpage)) {
    pic <- pic_options[is_frontpage] %>% rvest::html_attr("src")
  } else {
    pic <- pic_options[[1]] %>% rvest::html_attr("src")
  }
  
  headshot <- glue::glue("{dirname(path)}/{yaml$photo}")
  
  if (str_detect(path, "Doug")) {
    headshot <- str_remove(headshot, "resources/")
  }
  
  intro <- display_page %>% 
    rvest::html_node("#introduction") %>% 
    rvest::html_node("p") %>% 
    rvest::html_text()
  
  template = '<div class="col-lg-3 col-md-4 col-sm-6 mb-4">
      <div class="card h-100">
        <a href="{link}" target="_blank"><img class="card-img-top" src="{pic}" alt=""></a>
        <div class="card-body">
          <h4 class="card-title">
            <a href="{link}" target="_blank">{yaml$author}</a>
            <a href="{path}" target="_blank" title="See the raw code"><i class="far fa-file-code"></i></a>
            <img class="headshot" src="{headshot}" style="object-position: 50% 60%">
          </h4>
          <p class="card-text">{yaml$title}<hr/> <small>{intro}</small></p>
        </div>
      </div>
    </div>'
  
  glue::glue(template)
}

tiles <- map(files, make_tile) %>% compact() %>% glue::glue_collapse()

template <- xml2::read_html("template.html") %>% as.character()

html <- glue::glue(template)
writeLines(html, "index.html")


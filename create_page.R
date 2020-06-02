library(glue)
library(purrr)
library(magrittr)
library(stringr)

files = list.files(pattern = ".Rmd$", recursive = T)

make_tile <- function(path) {
  yaml <- rmarkdown::yaml_front_matter(path)
  yaml$title <- str_replace_all(
    yaml$title, 
    c(
      "~(\\w+)~" = "<sub>\\1</sub>",
      "_(\\w+)_" = "<span style='font-style: italic'>\\1</span>"
    )
  )
  
  link <- stringr::str_replace(path, "\\.Rmd", ".html")
  
  if (str_detect(link, "Cathrine")) {
    link <- str_replace(link, "Cathrine Ingvordsen.html", "Cathrine-Ingvordsen.html")
  }
  
  if (str_detect(link, "Gavin")) {
    link <- str_replace(link, "Data School Project_Gavin Hunter.html","Data-School-Project_Gavin-Hunter.html")
  }
  
  if (str_detect(link, "Elaheh") & str_detect(link, "Untitled")) {
    return(NULL)
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
  
    if (str_detect(link, "Derek")) {
    is_frontpage <- c(F,F,F,F,F,T)
  }
  
  #If no tagged frontpage image, just pick first non-photo image in html file
  if (any(is_frontpage)) {
    pic <- pic_options[is_frontpage][1] %>% rvest::html_attr("src")
  } else {
    pic <- pic_options[[1]] %>% rvest::html_attr("src")
  }
  

  
  #headshot <- glue::glue("{dirname(path)}/{yaml$photo}")
  
  headshot <- display_page %>%
    rvest::html_nodes("img") %>% 
    keep(map(., ~rvest::html_attr(., "class")) == "photo") %>% 
    as.character %>% 
    str_replace('.+src=\"(.+)\".+', "\\1")
  
  
  intro <- display_page %>% 
    rvest::html_node("#introduction") %>% 
    rvest::html_nodes("p") %>% 
    rvest::html_text() %>% 
    glue_collapse(sep = "\n")
  
  
  # Tristan had to make it not self-contained
  if (str_detect(link, "Tristan")) {
    pic <- glue::glue("{dirname(path)}/{pic}")
    headshot <- glue::glue("{dirname(path)}/{headshot}")
  }
  
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
  
  if (str_detect(link, "Yuwan")) {
    template = '<div class="col-lg-3 col-md-4 col-sm-6 mb-4">
      <div class="card h-100">
        <a href="{link}" target="_blank"><img class="card-img-top" src="{pic}" alt=""></a>
        <div class="card-body">
          <h4 class="card-title">
            <a href="{link}" target="_blank">{yaml$author}</a>
            <img class="headshot" src="{headshot}" style="object-position: 50% 60%">
          </h4>
          <p class="card-text">{yaml$title}<hr/> <small>{intro}</small></p>
        </div>
      </div>
    </div>'
  }
  
  glue::glue(template)
}

tiles <- map(files, make_tile) %>% compact() %>% glue::glue_collapse()

tiles <- str_c(tiles, readLines("EliseB/tile.html") %>% glue::collapse())

tiles <- str_c(tiles, readLines("Megan/tile.html") %>% glue::collapse())

template <- xml2::read_html("template.html") %>% as.character()

html <- glue::glue(template)
writeLines(html, "index.html")


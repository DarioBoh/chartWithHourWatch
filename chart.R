rm(list=ls())
db <- NULL
toc <- NULL
#function for hourwatch
tic <- function () {
  now <- proc.time()
  function () {
    proc.time() - now
  }
}
#toc <- tic()

#custommized add_tooltip function with timer
add_tooltip2 <- function (vis, html, on = c("hover", "click")) {
  on <- match.arg(on)
  show_tooltip2 <- function(data, location, session, ...) {
    if (is.null(data)) {
      hide_tooltip(session)
      return(session)
    }
    html <- html(data)
    if (is.null(html)) {
      hide_tooltip(session)
    }
    else {
      #start hourwatch
      toc <<- tic()
      show_tooltip(session, location$x + 5, location$y + 5, html)
    }
  }
  hide_tooltip2 <- function(session) {
    hide_tooltip(session)
    #print --- next step is to save in an object
    print(c(paste('Time elapsed:',toc()[3]),tab_output ))
    db <<- c(db,c(paste('Time elapsed:',toc()[3]),tab_output ))
    
    #output db should be matrix(ncol=5, byrow=T)
  }
  switch(on, click = handle_click(vis, show_tooltip2), hover = handle_hover(vis, show_tooltip2, hide_tooltip2))
}



formatting_values <- function(tab) {
  if(is.null(tab)) return(NULL)
  tabHtml <- paste0(names(tab), ": ", format(tab), collapse = "<br />")
  #return a string with the hovered values
  tab_output <<- c(paste0(names(tab), ": ", format(tab)),as.character(Sys.time()))
  
  return(tabHtml)
}



iris %>% 
  ggvis(~Sepal.Length,~Sepal.Width, fill=~Species) %>% 
  layer_points() %>% 
  add_axis("y", orient = "right") %>% 
  add_axis("y", orient = "left") %>% 
  add_tooltip2(formatting_values,'hover') %>% 
  bind_shiny('ggvis','2d') 
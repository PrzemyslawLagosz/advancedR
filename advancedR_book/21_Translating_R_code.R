library(dbplyr)
translate_sql(x ^ 2)
#> <SQL> POWER(`x`, 2.0)
translate_sql(x < 5 & !is.na(x))
#> <SQL> `x` < 5.0 AND NOT(((`x`) IS NULL))
translate_sql(!first %in% c("John", "Roger", "Robert"))
#> <SQL> NOT(`first` IN ('John', 'Roger', 'Robert'))
translate_sql(select == 7)
#> <SQL> `select` = 7.0

library(rlang)
library(purrr)


# <body>
#   <h1 id='first'>A heading</h1>
#   <p>Some text &amp; <b>some bold text.</b></p>
#   <img src='myimg.png' width='100' height='100' />
# </body>

with_html(
  body(
    h1("A heading", id = "first"),
    p("Some text &", b("some bold text")),
    img(src = "myimg.png", width = 100, height = 100)
  )
)


## 21.2.2 Escaping
# The easiest way to do this is to create an S3 class (Section 13.3) that distinguishes between regular text (that needs escaping) and HTML (that doesn’t).

html <- function(x) structure(x, class = "advr_html")

print.advr_html <- function(x, ...) {
  out <- paste0("<HTML>", x)
  cat(paste(strwrap(out), collapse = "\n"), "\n", sep = "")
}

gsub("&", "hEHE", x = "EL&O")
#> "ELhEHEO"

# We then write an escape generic.

escape <- function(x) UseMethod("escape")

escape.character <- function(x) {
  x <- gsub("&", "&amp;", x)
  x <- gsub("<", "&lt;", x)
  x <- gsub(">", "&gt;", x)
  
  html(x)
}

escape.advr_html <- function(x) x

escape("This is some text.")
#> <HTML> This is some text.
escape("x > 1 & y < 2")
#> <HTML> x &gt; 1 &amp; y &lt; 2

# Double escaping is not a problem
escape(escape("This is some text. 1 > 2"))
#> <HTML> This is some text. 1 &gt; 2

# And text we know is HTML doesn't get escaped.
escape(html("<hr />"))
#> <HTML> <hr />



## 21.2.3 Basic tag functions

dots_partition <- function(...) {
  dots <- list2(...)
  
  if (is.null(names(dots))) {
    is_named <- rep(FALSE, length(dots))
  } else {
    is_named <- names(dots) != ""
  }
  
  list(
    named = dots[is_named],
    unnamed = dots[!is_named]
  )
}

str(dots_partition(a = 1, 2, b = 3, 4))
#> List of 2
#>  $ named  :List of 2
#>   ..$ a: num 1
#>   ..$ b: num 3

source("dsl-html-attributes.r")
p <- function(...) {
  dots <- dots_partition(...)
  attribs <- html_attributes(dots$named)
  children <- map_chr(dots$unnamed, escape)
  
  html(paste0(
    "<p", attribs, ">",
    paste(children, collapse = ""),
    "</p>"
  ))
}

p("Some text")
#> <HTML> <p>Some text</p>
p("Some text", id = "myid")
#> <HTML> <p id='myid'>Some text</p>
p("Some text", class = "important", `data-value` = 10)
#> <HTML> <p class='important' data-value='10'>Some text</p>
#>  $ unnamed:List of 2
#>   ..$ : num 2
#>   ..$ : num 4
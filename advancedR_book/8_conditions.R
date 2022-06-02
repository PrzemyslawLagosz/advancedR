library(rlang)

stop("This is what an error looks like", call. = FALSE) # <- TRUE by default
#> Error in eval(expr, envir, enclos): This is what an error looks like
rlang::abort("This is what an error looks like")


warning("This is what a warning looks like")
#> Warning: This is what a warning looks like

message("This is what a message looks like")
#> This is what a message looks like

# You can control this behaviour with the warn option:
  
# To make warnings appear immediately, set 
options(warn = 1)

# To turn warnings into errors, set 
options(warn = 2)
# This is usually the easiest way to debug a warning, as once it’s an error you can use tools like traceback() to find the source.

# Restore the default behaviour with 
options(warn = 0)

### 8.3 Ignoring conditions

# The simplest way of handling conditions in R is to simply ignore them:
  
# Ignore errors with 
try()
# Ignore warnings with 
suppressWarnings()
# Ignore messages with 
suppressMessages()


### 8.4 Handling conditions

## 8.4.1 Condition objects
cnd <- catch_cnd(stop("An error"))
str(cnd)
#> List of 2
#>  $ message: chr "An error"
#>  $ call   : language force(expr)
#>  - attr(*, "class")= chr [1:3] "simpleError" "error" "condition"

conditionMessage
#> [1] "An error"
conditionCall(cnd)
#> force(expr)

## 8.4.2 Exiting handlers

tryCatch() 
#registers exiting handlers, and is typically used to handle error conditions. It allows you to override the default error behaviour.

f3 <- function(x) {
  tryCatch(
    error = function(cnd) NA,
    log(x)
  )
}

f3("x")
#> [1] NA



tryCatch(
  message = function(cnd) "There",
  {
    message("Here")
    stop("This code is never run!")
  }
)
#> [1] "There"

tryCatch(
  error = function(cnd) {
    paste0("--", conditionMessage(cnd), "--")
  },
  stop("This is an error"),
  finally = {print("I alway run of whether the initial expression succeeds or fails")}
)
#> [1] "--This is an error--"



## 8.4.3 Calling handlers

tryCatch(
  message = function(cnd) cat("Caught a message!\n"), 
  {
    message("Someone there?")
    message("Why, yes!")
  }
)
#> Caught a message!

withCallingHandlers(
  message = function(cnd) cat("Caught a message!\n"), 
  {
    message("Someone there?")
    message("Why, yes!")
  }
)
#> Caught a message!
#> Someone there?
#> Caught a message!
#> Why, yes!




# Muffles the default handler which prints the messages
withCallingHandlers(
  message = function(cnd) {
    cat("Level 2\n")
    cnd_muffle(cnd)
  },
  withCallingHandlers(
    message = function(cnd) cat("Level 1\n"),
    message("Hello")
  )
)
#> Level 1
#> Level 2

# Muffles level 2 handler and the default handler
withCallingHandlers(
  message = function(cnd) cat("Level 2\n"),
  withCallingHandlers(
    message = function(cnd) {
      cat("Level 1\n")
      cnd_muffle(cnd)
    },
    message("Hello")
  )
)
#> Level 1



## 8.4.4 Call stacks <- In the book

# EXCERCISE #

show_condition <- function(code) {
  tryCatch(
    error = function(cnd) "error",
    warning = function(cnd) "warning",
    message = function(cnd) "message",
    {
      code
      NULL
    }
  )
}


show_condition(stop("!"))
show_condition(10)
show_condition(warning("?!"))
show_condition({
  10
  message("?")
  warning("?!")
})



## 8.5 Custom conditions

log(letters)
#> Error in log(letters): non-numeric argument to mathematical function
log(1:10, base = letters)
#> Error in log(1:10, base = letters): non-numeric argument to mathematical
#> function

my_log <- function(x, base = exp(1)) {
  if(!is.numeric(x)) {
    abort(paste0(
      "`x` must be numeric vector; not ", typeof(x), "."
      ))
  }
  if(!is.numeric(base)) {
    abort(paste0(
      "`base` must be numeric vector; not ", typeof(base), "."
      ))
  }
  
  base::log(x, base = base)
}

my_log(letters)
#> Error: `x` must be a numeric vector; not character.
my_log(1:10, base = letters)
#> Error: `base` must be a numeric vector; not character.



# 8.5.2 Signalling

abort_bad_argument <- function(arg, must, not = NULL) {
  msg <- glue::glue("`{arg}` must {must}")
  if (!is.null(not)) {
    not <- typeof(not)
    msg <- glue::glue("{msg}; not {not}.")
  }
  
  abort("error_bad_argument", 
        message = msg, 
        arg = arg, 
        must = must, 
        not = not
  )
}


my_log <- function(x, base = exp(1)) {
  if (!is.numeric(x)) {
    abort_bad_argument("x", must = "be numeric", not = x)
  }
  if (!is.numeric(base)) {
    abort_bad_argument("base", must = "be numeric", not = base)
  }
  
  base::log(x, base = base)
}

my_log(letters)
#> Error: `x` must be numeric; not character.
my_log(1:10, base = letters)
#> Error: `base` must be numeric; not character.


## 8.5.3 Handling

tryCatch(
  error_bad_argument = function(cnd) "bad_argument",
  error = function(cnd) "other error",
  my_log("a")
)
#> [1] "bad_argument"






### 8.6 Applications

fail_with <- function(expr, value = NULL) {
  tryCatch(
    error = function(cnd) value,
    expr
  )
}

fail_with(log(10), value = "I will be displayed if expresion in 1st agrument will throw error")
#> [1] 2.3
fail_with(log("x"), value = "I will be displayed if expresion in 1st agrument will throw error")
#> [1] NA


try2 <- function(expr, silent = FALSE) {
  tryCatch(
    error = function(cnd) {
      msg <- conditionMessage(cnd)
      if (!silent) {
        message("Error: ", msg)
      }
      structure(msg, class = "try-error")
    },
    expr
  )
}

try2(1)
#> [1] 1
try2(stop("Hi"))
#> Error: Hi
#> [1] "Hi"
#> attr(,"class")
#> [1] "try-error"
try2(stop("Hi"), silent = TRUE)
#> [1] "Hi"
#> attr(,"class")
#> [1] "try-error"

# Funckja sprawdzająca czy dane wyrazenie wyrzuca error. Zwraca T/F
does_error <- function(expr) {
  tryCatch(
    error = function(cnd) TRUE, #error_val
    {
      expr
      FALSE #success_val
    }
  )
}

does_error(log("s"))


safety <- function(expr) {
  tryCatch(
    error = function(cnd) {
      list(result = NULL, error = cnd)
    },
    list(result = expr, error = NULL)
  )
}

str(safety(1+1))
str(safety(1+"a"))


# You could write a similar function if you were trying to find the source of an annoying message. 
warning2error <- function(expr) {
  withCallingHandlers(
    warning = function(cnd) abort(conditionMessage(cnd)),
    expr
  )
}

warning2error({
  x <- 2 ^ 4
  warn("Hello")
})
#> Error: Hello


catch_cnds <- function(expr) {
  conds <<- list()
  add_cond <- function(cnd) {
    conds <<- append(conds, list(cnd))
    cnd_muffle(cnd)
  }
  
  withCallingHandlers(
    message = add_cond,
    warning = add_cond,
    expr
  )
  
  conds
}

catch_cnds({
  inform("a")
  warn("b")
  inform("c")
})
#> [[1]]
#> <message: a
#> >
#> 
#> [[2]]
#> <warning: b>
#> 
#> [[3]]
#> <message: c
#> >


# What if you also want to capture errors?
catch_cnds <- function(expr) {
  conds <- list()
  add_cond <- function(cnd) {
    conds <<- append(conds, list(cnd))
    cnd_muffle(cnd)
  }
  
  tryCatch(
    error = function(cnd) {
      conds <<- append(conds, list(cnd))
    },
    withCallingHandlers(
      message = add_cond,
      warning = add_cond,
      expr
    )
  )
  
  conds
}

catch_cnds({
  inform("a")
  warn("b")
  abort("C")
})
#> [[1]]
#> <message: a
#> >
#> 
#> [[2]]
#> <warning: b>
#> 
#> [[3]]
#> <error/rlang_error>
#> C
#> Backtrace:
#>  1. global::catch_cnds(...)
#>  6. base::withCallingHandlers(...)
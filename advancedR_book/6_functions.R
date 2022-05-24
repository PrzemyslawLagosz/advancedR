x <- 10
f1 <- function(x) {
  function() {
    x + 10
  }
}
f1(1)()


### 6.2.1 Function components
f02 <- function(x, y) {
  # A comment
  x + y
}

formals(f02)
#> $x
#> 
#> 
#> $y

body(f02)
#> {
#>     x + y
#> }

environment(f02)
#> <environment: R_GlobalEnv>

attr(f02, "srcref")
#> function(x, y) {
#>   # A comment
#>   x + y
#> }

# anonymous function.
lapply(mtcars, function(x) length(unique(x)))
Filter(function(x) !is.numeric(x), mtcars)
Filter(function(x) x > 2, 1:7)

integrate(function(x) x ^ 3, 0, 2)


## 6.2.4 Invoking a function
args <- list(1:10, na.rm = TRUE)
do.call(mean, args)
#> [1] 5.5





### 6.4 Lexical scoping
# Lexical scoping determines where, but not when to look for values. R looks for values when the 
# function is run, not when the function is created. Together, these two properties tell us that the
# output of a function can differ depending on the objects outside the function’s environment:

g12 <- function() x + 1
x <- 15
g12()
#> [1] 16

x <- 20
g12()
#> [1] 21

# To detect this problem, use codetools::findGlobals(). This function lists all the external dependencies (unbound symbols) within a function:
codetools::findGlobals(g12)

# you can manually change the function’s environment to the emptyenv(), an environment which contains nothing:
environment(g12) <- emptyenv()
g12()
#> Error in x + 1: could not find function "+"


### 6.5 Lazy evaluation

# In R, function arguments are lazily evaluated: they’re only evaluated if accessed.
# Lazy evaluation is powered by a data structure called a promise

## 6.5y <- 10

y <- 10
h02 <- function(x) {
  y <- 100
  x + 1
}

h02(y)
#> [1] 11

h02(y <- 1000)
#> [1] 1001
y
#> [1] 1000


## 6.5.2 Default arguments
h04 <- function(x = 1, y = x * 2, z = a + b) {
  a <- 10
  b <- 100
  
  c(x, y, z)
}

h04()
#> [1]   1   2 110

# 6.5.3 Missing arguments
h06 <- function(x = 10) {
  list(missing(x), x)
}
str(h06())
#> List of 2
#>  $ : logi TRUE  # <- True bo został wziety argument domyslny, czyli zmienna `x` nie podana -> missing
#>  $ : num 10
str(h06(20))
#> List of 2
#>  $ : logi FALSE
#>  $ : num 10


# 
sample <- function(x, size = NULL, replace = FALSE, prob = NULL) {
  if (is.null(size)) {
    size <- length(x)
  }
  
  x[sample.int(length(x), size, replace = replace, prob = prob)]
}

# %||% infix function, which uses the left side if it’s not NULL and the right side otherwise

`%||%` <- function(lhs, rhs) {
  if (!is.null(lhs)) {
    lhs
  } else {
    rhs
  }
}

sample <- function(x, size = NULL, replace = FALSE, prob = NULL) {
  size <- size %||% length(x)
  x[sample.int(length(x), size, replace = replace, prob = prob)]
}

### 6.6 ... (dot-dot-dot)

i01 <- function(y, z) {
  list(y = y, z = z)
}

i02 <- function(x, ...) {
  i01(...)
}

str(i02(x = 1, y = 2, z = 3))
#> List of 2
#>  $ y: num 2
#>  $ z: num 3

# More useful is list(...), which evaluates the arguments and stores them in a list:
i04 <- function(...) {
  list(...)
}
str(i04(a = 1, b = 2))
#> List of 2
#>  $ a: num 1
#>  $ b: num 2
#>  
#>  
#>  (See also rlang::list2() to support splicing and to silently ignore trailing commas, 
#>  and rlang::enquos() to capture unevaluated arguments, the topic of quasiquotation.)




###  6.7 Exiting a function

# There are two ways that a function can return a value:
#   
#   Implicitly, where the last evaluated expression is the return value:
  
  j01 <- function(x) {
    if (x < 10) {
      0
    } else {
      10
    }
  }
j01(5)
#> [1] 0
j01(15)
#> [1] 10


#Explicitly, by calling return():
  
  j02 <- function(x) {
    if (x < 10) {
      return(0)
    } else {
      return(10)
    }
  }
  
j04 <- function() invisible(1)
j04()

print(j04())
#> [1] 1
  
(j04())
#> [1] 1

withVisible(j04())

# use on.exit() to set up an exit handler. !

j06 <- function(x) {
  cat("Hello\n")
  on.exit(cat("Goodbye!\n"), add = TRUE)
  
  if (x) {
    return(10)
  } else {
    on.exit(cat("Goodbye123!\n"), add = TRUE)
    stop("Error")
  }
}

j06(TRUE)
#> Hello
#> Goodbye!
#> [1] 10

j06(FALSE)
#> Hello
#> Error in j06(FALSE): Error
#> Goodbye!

# Always set add = TRUE when using on.exit(). If you don’t, each call to on.exit() will 
# overwrite the previous exit handler. Even when only registering a single handler, it’s 
# good practice to set add = TRUE so that you won’t get any unpleasant surprises if you later
# add more exit handlers.

with_dir <- function(dir, code) {
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  
  force(code)
}

getwd()
#> [1] "/Users/runner/work/adv-r/adv-r"
with_dir("~", getwd())
#> [1] "/Users/runner"
#>
#> The use of force() isn’t strictly necessary here as simply referring to code will force its evaluation

plot_pdf <- function(code) {
  pdf("test.pdf")
  on.exit(dev.off(), add = TRUE)
  code
}

### 6.8 Function forms

# prefix: the function name comes before its arguments, like foofy(a, b, c). These
# constitute of the majority of function calls in R.
# 
# infix: the function name comes in between its arguments, like x + y. Infix forms 
# are used for many mathematical operators, and for user-defined functions that begin and end with %.
# 
# replacement: functions that replace values by assignment, like names(df) <- c("a", "b", "c"). 
# They actually look like prefix functions.
# 
# special: functions like [[, if, and for. While they don’t have a consistent structure, 
#                           they play important roles in R’s syntax.

## INFIX

`% %` <- function(a, b) paste(a, b)
`%/\\%` <- function(a, b) paste(a, b)

"a" % % "b"
#> [1] "a b"
"a" %/\% "b"
#> [1] "a b"

# R’s default precedence rules mean that infix operators are composed left to right:
`%-%` <- function(a, b) paste0("(", a, " %-% ", b, ")")
"a" %-% "b" %-% "c"
#> [1] "((a %-% b) %-% c)"


##  Replacement functions
# Replacement functions are used by placing the function call on the left side of <-: # 1

`second<-` <- function(x, value) {
  x[2] <- value
  x
}

#1
x <- 1:10
second(x) <- 5L
x
#>  [1]  1  5  3  4  5  6  7  8  9 10

`modify<-` <- function(x, position, value) {
  x[position] <- value
  x
}
modify(x, 1) <- 10
x
#>  [1] 10  5  3  4  5  6  7  8  9 10

`[`(x,5)

`rr<-` <- function(x, value) {
  index <- sample(length(x), 1)
  x[index] <- value
  x
}

rr(x) <- 15

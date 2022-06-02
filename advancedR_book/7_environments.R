library(rlang)
library(magrittr)

### 7.2 Environment basics

# Generally, an environment is similar to a named list, with four important exceptions:
#   
# 1 Every name must be unique.
# 2 The names in an environment are not ordered.
# 3 An environment has a parent.
# 4 Environments are not copied when modified.

e1 <- env(
  a = FALSE,
  b = "a",
  c = 2.3,
  d = 1:3,
)

e1
#> <environment: 0x7fe6c2184968>

env_print(e1)
#> <environment: 0x7fe6c2184968>
#> parent: <environment: global>
#> bindings:
#>  * a: <lgl>
#>  * b: <chr>
#>  * c: <dbl>
#>  * d: <env>

env_names(e1)
#> [1] "a" "b" "c" "d"

## 7.2.3 Parents
# set the parent environment by supplying an unnamed argument to env()
e2a <- env(d = 4, e = 5) #  <-- parent
e2b <- env(e2a, a = 1, b = 2, c = 3)

env_parent(e2b)
#> <environment: 0x7fe6c7399f58>
env_parent(e2a)
#> <environment: R_GlobalEnv>

e2c <- env(empty_env(), d = 4, e = 5)
e2d <- env(e2c, a = 1, b = 2, c = 3)

env_parents(e2b)
#> [[1]]   <env: 0x7fe6c7399f58>
#> [[2]] $ <env: global>
env_parents(e2d)
#> [[1]]   <env: 0x7fe6c4d9ca20>
#> [[2]] $ <env: empty>

## 7.2.4 Super assignment, <<-

# The ancestors of an environment have an important relationship to <<-. Regular assignment, <-, 
# always creates a variable in the current environment. Super assignment, <<-, never creates a variable in the 
# current environment, but instead modifies an existing variable found in a parent environment.

x <- 0
f <- function() {
  x <<- 1
}
f()
x
#> [1] 1

## 7.2.5 Getting and setting

e3 <- env(x = 1, y = 2)
e3$x
#> [1] 1

e3$z <- 3
e3[["z"]]
#> [1] 3

e3$xyz
#> NULL

env_get(e3, "xyz")
#> Error in env_get(e3, "xyz"): argument "default" is missing, with no default

env_get(e3, "xyz", default = NA)
#> [1] NA

# Unlike lists, setting an element to NULL does not remove it, because sometimes you want a name that refers to NULL. Instead, use env_unbind():

e3$a <- NULL
env_has(e3, "a")
#>    a 
#> TRUE

env_unbind(e3, "a")
env_has(e3, "a")
#>     a 
#> FALSE
 
## 7.2.6 Advanced bindings

# env_bind_lazy() creates delayed bindings, which are evaluated the first time they are accessed.

env_bind_lazy(current_env(), b = {Sys.sleep(3); 1})

system.time(print(b))
#> [1] 1
#>    user  system elapsed 
#>    0.00    0.00    3.09
system.time(print(b))
#> [1] 1
#>    user  system elapsed 
#>       0       0       0

system.time(
for (i in 1:10) {
  Sys.sleep(1)
  print(i)
}
)

# env_bind_active() creates active bindings which are re-computed every time they’re accessed:

env_bind_active(current_env(), z1 = function(val) runif(1))

z1
#> [1] 0.0808
z1
#> [1] 0.834

## EXCERCISE
# 1.
e1 <- env()
e1$loop <- e1

lobstr::ref(e1)

# 5.
e2 <- new.env()
e3 <- new.env()

env_poke2 <- function(env, name, value) {
  if (env_has(env, name)) {
    abort(paste0("\"", name, "\" is already assigned to a value."))
  }
  
  env_poke(env, name, value)
  invisible(env)
}

# Test
env_poke2(e3, "b", 100)
e3$b
#> [1] 100
env_poke2(e3, "b", 200)
#> Error: "b" is already assigned to a value.

# 6.
rebind <- function(name, value, env = caller_env()) {
  if (identical(env, empty_env())) {
    stop("Can't find `", name, "`", call. = FALSE)
  } else if (env_has(env, name)) {
    env_poke(env, name, value)
  } else {
    rebind(name, value, env_parent(env))
  }
}
rebind("a", 10)
#> Error: Can't find `a`
a <- 5
rebind("a", 10)
a
#> [1] 10

# A: The primary difference between rebind() and <<- is that rebind() will only carry 
# out an assignment when it finds an existing binding; unlike <<- it will never create a 
# new one in the global environment. This behaviour of <<- is usually undesirable because 
# global variables introduce non-obvious dependencies between functions.

### 7.3 Recursing over environments

where <- function(name, env = caller_env()) {
  if (identical(env, empty_env())) {
    # Base case
    stop("Can't find ", name, call. = FALSE)
  } else if (env_has(env, name)) {
    # Success case
    env
  } else {
    # Recursive case
    where(name, env_parent(env))
  }
}

where("yyy")
#> Error: Can't find yyy

x <- 5
where("x")
#> <environment: R_GlobalEnv>

where("mean")
#> <environment: base>

## EXERCISe

where2 <- function(name, env = caller_env(), results = list()) {
  if (identical(env, empty_env())) {
    # Base case
    return(results)
  } else {
    if (env_has(env, name)) {
      results <- c(results, env)
    }
    where2(name, env_parent(env), results)
  }
}

where2("mean")

e1a <- env(empty_env(), a = 1, b = 2)
e1b <- env(e1a, b = 10, c = 11, x = function(x) x +2)
e1c <- env(e1b, a = 12, d = 13)

where2("a", e1c)

fget <- function(name, env = caller_env(), inherits = TRUE) {
  if (env_has(env, name) == TRUE) {
    obj <- env_get(env, name)
    
    if (is.function(obj) == TRUE) {
      return(obj)
    }
  }
  
  if (identical(env, empty_env()) || inherits == FALSE) {
    stop("Could not find a function called \"", name, "\".",
         call. = FALSE)
  }
  
  fget(name, env_parent(env))
}

fget("measn")


### 7.4 Special environments

# base::search()

# A function binds the current environment when it is created. This is called the function environment, 
# and is used for lexical scoping. Across computer languages, functions that capture (or enclose) their environments are called closures

## 7.4.3 Namespaces

e <- env()
e$g <- function() 1

environment(g)


## 7.4.4 Execution environments

g <- function() {
  if (!env_has(current_env(), "a")) {
    message("Defining a")
    a <- 1
  } else {
    a <- a + 1
  }
  a
}
g()


f1 <- function(x1) {
  f2 <- function(x2) {
    f3 <- function(x3) {
      x1 + x2 + x3
      print("f3")
      print(env_print())
    }
    f3(3)
    print("f2")
    print(env_print())
  }
  f2(2)
  print("f1")
  print(env_print())
}

f1(1)
# https://advanced-r-solutions.rbind.io/images/environments/function_environments_corrected.png

#> [1] "f3"
#> <environment: 0x7f877079f758>
#> parent: <environment: 0x7f87707a34a8>
#> bindings:
#>  * x3: <dbl>
#> <environment: 0x7f877079f758>
#> [1] "f2"
#> <environment: 0x7f87707a34a8>
#> parent: <environment: 0x7f87707a32e8>
#> bindings:
#>  * f3: <fn>
#>  * x2: <dbl>
#> <environment: 0x7f87707a34a8>
#> [1] "f1"
#> <environment: 0x7f87707a32e8>
#> parent: <environment: global>
#> bindings:
#>  * f2: <fn>
#>  * x1: <dbl>
#> <environment: 0x7f87707a32e8>

### 7.5 Call stacks
rlang::caller_env()

f <- function(x) {
  g(x = 2)
}
g <- function(x) {
  h(x = 3)
}
h <- function(x) {
  #lobstr::cst()
  stop()
}

#> The way you most commonly see a call stack in R is by looking at the traceback() after an error has occurred:

f(x = 1)
#> Error:
traceback()
#> 4: stop()
#> 3: h(x = 3) 
#> 2: g(x = 2)
#> 1: f(x = 1)

# call stack tree: cst()
h <- function(x) {
  lobstr::cst()
}

f(x = 1)
#> █
#> └─f(x = 1)
#>   └─g(x = 2)
#>     └─h(x = 3)
#>       └─lobstr::cst()

## 7.5.2 Lazy evaluation + call stack

a <- function(x) b(x)
b <- function(x) c(x)
c <- function(x) x

a(f())
#> █
#> ├─a(f())
#> │ └─b(x)
#> │   └─c(x)
#> └─f()
#>   └─g(x = 2)
#>     └─h(x = 3)
#>       └─lobstr::cst()
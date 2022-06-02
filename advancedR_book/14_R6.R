# install.packages("R6")
library(R6)

# The following example shows the two most important arguments to R6Class():
#   
# The first argument is the classname. It’s not strictly needed, but it improves error 
# messages and makes it possible to use R6 objects with S3 generics. By convention, R6 classes have UpperCamelCase names.
# 
# The second argument, public, supplies a list of methods (functions) and fields (anything else) that make up the public interface of the object. By convention,methods and fields use snake_case. Methods can access the methods and fields of the current object via self$.75

# Class definition
Accumulator <- R6Class("Accumulator", list(
  sum = 0,
  add = function(x = 1) {
    self$sum <- self$sum + x
    invisible(self)
  }
))

# Construct a new object from the class Accumulator
x <- Accumulator$new()

x$add(4)
# > [1] 4
x$add(4)
# > [1] 8

x$sum

## 14.2.1 Method chaining

# $add() is called primarily for its side-effect of updating $sum
# Side-effect R6 methods should always return self invisibly. This returns the “current” object and makes it possible to chain together multiple method calls:

# method chaining
x$
  add(10)$
  add(50)$
  sum

## 14.2.2 Important methods

# $initialize() and $print()

Person <- R6Class("Person", public = list(
  name = NULL,
  age = NA,
  initialize = function(name, age = NA) {
    stopifnot(is.character(name), length(name) == 1)
    stopifnot(is.numeric(age), length(age) == 1)
    
    self$name <- name
    self$age <- age
  },
  print = function(...) {
    cat("Person: \n")
    cat("  Name: ", self$name, "\n")
    cat("  Age: ", self$age, "\n")
    invisible(self)
  }
))

Przemo <- Person$new("Przemo", "25")
# Error in initialize(...) : zmienna is.numeric(age) nie ma wartości TRUE
Przemo <- Person$new("Przemo", 25)
Przemo$age <- 80
Przemo$age

## 14.2.3 Adding methods after creation

Accumulator <- R6Class("Accumulator")
Accumulator$set("public", "sum", 0)
Accumulator$set("public", "add", function(x = 1) {
  self$sum <- self$sum + x 
  invisible(self)
})

## 14.2.4 Inheritance

AccumulatorChatty <- R6Class("AccumulatorChatty",
                             inherit = Accumulator,
                             public = list(
                               add = function(x) {
                                 cat("Adding: ", x, "to current sum of:", self$sum, "\n")
                                 super$add(x = x)
                               }
                             ))

x2 <- AccumulatorChatty$new()
x2$add(15)


## 14.2.5 Introspection

class(Przemo)
# "Person" "R6"

names(Przemo)
# ".__enclos_env__" "age"             "name"            "clone"           "print"           "initialize"


### EXERCISE ###
# 1.
Bank <- R6Class(
  "Bank",
  public = list(
    
    balance = 0,
    initialize = function(balance) {
      stopifnot(is.numeric(balance))
      self$balance <- balance
    },
    
    deposit = function(x) {
      stopifnot(is.numeric(x))
      self$balance <- self$balance + x
      print(paste0("Accual balance:", self$balance))
      invisible(self)
    },
    
    withdraw = function(x) {
      stopifnot(is.numeric(x))
      if(self$balance < x) {
        stop("Not enought balance",
             call. = FALSE)
      } else {
        self$balance <- self$balance - x
        print(paste0("You just withdrawed:", x))
        print(paste0("Accual balance:", self$balance))
        invisible(self)
      }
    }
  ))

przemo <- Bank$new(1000)

przemo$withdraw(500)
przemo$balance <- 100
przemo$deposit(200)

BankDept <- R6Class(
  "BankDept",
  inherit = Bank,
  public = list(
    withdraw = function(x) {
      stopifnot(is.numeric(x))
      if(self$balance < x) {
        self$balance <- self$balance - 1.05*x
        print(paste0("You just withdrawed:", x))
        print(paste0("Accual balance:", self$balance))
        print(paste0("Dept transaction fee:", 0.05*x))
      } else {
        super$withdraw(x = x)
        invisible(self)
      }
    }
  ))

mary <- BankDept$new(1000)

mary$balance
mary$deposit(100)
mary$withdraw(600)


# 2.
suit <- c("_PIK", "_KIER", "_KARO", "_TREFL")
value <- c("A", 2:10, "J", "Q", "K")
cards <- paste0(rep(value, 4), suit)

ShuffledDeck <- R6Class(
  classname = "ShuffledDeck",
  public = list(
    deck = NULL,
    initialize = function(deck = cards) {
      self$deck <- sample(deck)
    },
    reshuffle = function() {
      self$deck <- sample(cards)
      invisible(self)
    },
    n = function() {
      length(self$deck)
    },
    draw = function(n = 1) {
      if (n > self$n()) {
        stop("Only ", self$n(), " cards remaining.", call. = FALSE)
      }
      
      output <- self$deck[seq_len(n)]
      self$deck <- self$deck[-seq_len(n)]
      output
    }
  )
)


### 14.3 Controlling access

# R6Class() has two other arguments that work similarly to public:
#   
# 1. private allows you to create fields and methods that are only available from within the class, not outside of it.
# 1.1. The private argument to R6Class works in the same way as the public argument: you give it a named list of methods (functions) and fields (everything else).
# 1.2. Fields and methods defined in private are available within the methods using private$ instead of self$. You cannot access private fields or methods outside of the class.



Person <- R6Class("Person", 
                  public = list(
                    initialize = function(name, age = NA) {
                      private$name <- name
                      private$age <- age
                    },
                    print = function(...) {
                      cat("Person: \n")
                      cat("  Name: ", private$name, "\n", sep = "")
                      cat("  Age:  ", private$age, "\n", sep = "")
                    }
                  ),
                  private = list(
                    age = NA,
                    name = NULL
                  )
)

hadley3 <- Person$new("Hadley")
hadley3
#> Person: 
#>   Name: Hadley
#>   Age:  NA
hadley3$name
#> NULL # <-- bo field zdefiniowany w private not in public

## 14.3.2 Active fields

# 2. active allows you to use accessor functions to define dynamic, or active, fields.
# 2.1 Active fields allow you to define components that look like fields from the outside, but are defined with functions, like methods.

Rando <- R6Class("Rando", active = list(
  random = function(value) {
    if (missing(value)) {
      runif(1)
    } else {
      stop("Can't set `$random`", call. = FALSE)
    }
  }
))

r <- Rando$new()
r$random


Person <- R6Class("Person", 
                  private = list(
                    .age = NA,
                    .name = NULL
                  ),
                  
                  active = list(
                    age = function(value) {
                      if (missing(value)) {
                        private$.age
                      } else {
                        stop("`$age` is read only", call. = FALSE)
                      }
                    },
                    name = function(value) {
                      if (missing(value)) {
                        private$.name
                      } else {
                        stopifnot(is.character(value), length(value) == 1)
                        private$.name <- value
                        self
                      }
                    }
                  ),
                  
                  public = list(
                    initialize = function(name, age = NA) {
                      stopifnot(is.character(name), length(name) == 1)
                      private$.name <- name
                      private$.age <- age
                    }
                  )
)

hadley4 <- Person$new("ABC", age = 38)
hadley4$name
#> [1] "Hadley"
hadley4$name <- 10
#> Error in (function (value) : is.character(value) is not TRUE
hadley4$age <- 20
hadley4$age
#> Error: `$age` is read only


### EXERCISE ###

BankNoSetting <- R6Class(
  "BankNoSetting",
  private = list(
    balance = 0
  ),
  public = list(
    initialize = function(InitialBalance) {
      private$balance <- InitialBalance
    },
    
    deposit = function(DepositAmount) {
      stopifnot(is.numeric(DepositAmount))
      private$balance <- private$balance + DepositAmount
      print(paste0("Accual balance:", private$balance))
      invisible(self)
    },
    
    withdraw = function(draw) {
      stopifnot(is.numeric(draw))
      if(private$balance < draw) {
        stop("Not enought balance",
             call. = FALSE)
      } else {
        private$balance <- private$balance - draw
        print(paste0("You just withdrawed:", draw))
        print(paste0("Accual balance:", private$balance))
        invisible(self)
      }
    }
  ))

banko <- BankNoSetting$new(1000)

banko$deposit(100)
banko$withdraw(1000)


Account <- R6Class("Account",
                   private = list(
                     .password = NULL
                   ),
                   active = list(
                     password = function(value) {
                       if (!missing(value)) {
                         private$.password <- value
                       } else {
                         print("Can't acces a password")
                       }
                     }
                   ),
                   public = list(
                     initialize = function(InitialPassword) {
                       private$.password <- InitialPassword
                     }
                   ))



Password <- R6Class(
  classname = "Password",
  public = list(
    initialize = function(InitialPassword) {
      private$password <- InitialPassword
    },
    print = function(...) {
      cat("<Password>: ********\n")
      invisible(self)
    },
    set = function(value) {
      private$password <- value
    },
    check = function(password) {
      identical(password, private$password)
    }
  ),
  private = list(
    password = NULL
  )
)

ja <- Password$new(12345)

ja$set(123)
ja$check(123)

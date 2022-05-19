
## Q1 https://adv-r.hadley.nz/names-values.html#quiz

df <- data.frame(runif(3), runif(3))
names(df) <- c(1, 2)

## backtikst pozwalają na wykorysztanie liczba jako nazyw kolumn
df$`3` <- df$`1` + df$`2`

## Bez tego trzebabyloby to obejsc w ten sposób
df$blal <- df[,1] + df[,2]
names(df) <- c(1, 2, 3)

# install.packages('lobstr')
library(lobstr)

x <- c(1, 2, 3)
y <- x

# The object, or value, doesn’t have a name; it’s actually the name that has a value.

# Adres w pamięci
obj_addr(x)

# Ten sam adres. R przypisyje
obj_addr(x) == obj_addr(y)

## 2.2.1 Non-syntactic names

a <- 1:10
b <- a
c <- b
d <- 1:10

list_of_names <- list(a, b, c, d)
obj_addrs(list_of_names)

# The following code accesses the mean function in multiple ways. 
# Do they all point to the same underlying function object? Verify this with lobstr::obj_addr().

mean_functions <- list(
  mean,
  base::mean,
  get("mean"),
  evalq(mean),
  match.fun("mean")
)

unique(obj_addrs(mean_functions))

## 2.3 Copy-on-modify

x <- c(1, 2, 3)
cat(tracemem(x), "\n") # Włącza sledzenie adresu obiektu przypisanego pod X

y <- x
y[[3]] <- 4

# No copy-on-modify
z <- x
z <- c(z,1)

untracemem(x) # wyłacza śledzenie adresu

## 2.3.2 Function calls

## 2.3.3 Lists

l1 <- list(1, 2, 3)
ref(l1)

l2 <- l1
ref(l2)

l2[[3]] <- 4
ref(l2)

# 2.3.4 Data frames

d1 <- data.frame(x = c(1, 5, 6), y = c(2, 4, 3))


x <- c("a", "a", "abc", "d")
ref(x, character = TRUE)

## 2.3.6 Exercises

x <- c(1L, 2L, 3L)
tracemem(x)

x[[3]] <- 4


x <- list(1:10)
x[[2]] <- x



## 2.4 Object size

# Sprawdzenie ile miejsca zajmuje dany obiekt

obj_size(letters)

x <- runif(1e6)
obj_size(x)
#> 8,000,048 B

y <- list(x, x, x)
obj_size(y)
#> 8,000,128 B


## EXERCISE

base_pkgs <- c(
  "package:stats", "package:graphics", "package:grDevices",
  "package:utils", "package:datasets", "package:methods",
  "package:base"
)

lapply(base_pkgs, as.environment)

## 2.5 Modify-in-place

x <- data.frame(matrix(runif(5 * 1e4), ncol = 5))
medians <- vapply(x, median, numeric(1))

cat(tracemem(x), "\n")
for (i in seq_along(medians)) {
  x[[i]] <- x[[i]] - medians[[i]]
}

y <- as.list(x)

## 2.5.2 Environments
e1 <- rlang::env(a = 1, b = 2, c = 3)


x <- list()
x[[1]] <- x

# Excercise
create_random_df <- function(nrow, ncol) {
  random_matrix <- matrix(runif(nrow * ncol), nrow = nrow)
  as.data.frame(random_matrix)
}

create_random_df(2, 2)
#>      V1     V2
#> 1 0.972 0.0116
#> 2 0.849 0.4339


subtract_df <- function(x, medians) {
for (i in seq_along(medians)) {
  x[[i]] <- x[[i]] - medians[[i]]
}
x
}

subtract_list <- function(x, medians) {
  x <- as.list(x)
  x <- subtract_df(x, medians)
  list2DF(x)
}


benchmark_medians <- function(ncol) {
  df <- create_random_df(nrow = 1e4, ncol = ncol)
  medians <- vapply(df, median, numeric(1), USE.NAMES = FALSE)
  
  bench::mark(
    "data frame" = subtract_df(df, medians),
    "list" = subtract_list(df, medians),
    time_unit = "ms"
  )
}

benchmark_medians(1)
#> # A tibble: 2 x 6
#>   expression    min median `itr/sec` mem_alloc `gc/sec`
#>   <bch:expr>  <dbl>  <dbl>     <dbl> <bch:byt>    <dbl>
#> 1 data frame 0.0419 0.0740    12450.     344KB     11.2
#> 2 list       0.0543 0.125      7866.     156KB     16.4

results <- bench::press(
  ncol = c(1, 10, 50, 100, 250, 300, 400, 500, 750, 1000),
  benchmark_medians(ncol)
)
#> Running with:
#>     ncol
#>  1     1
#>  2    10
#>  3    50
#>  4   100
#>  5   250
#>  6   300
#>  7   400
#>  8   500
#>  9   750
#> 10  1000

library(ggplot2)

ggplot(
  results,
  aes(ncol, median, col = attr(expression, "description"))
) +
  geom_point(size = 2) +
  geom_smooth() +
  labs(
    x = "Number of Columns",
    y = "Execution Time (ms)",
    colour = "Data Structure"
  ) +
  theme(legend.position = "top")
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'
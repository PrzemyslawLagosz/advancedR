
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

untracemem(x) # wyłacza śledzenie adresu

## 2.3.2 Function calls

## 2.3.3 Lists

l1 <- list(1, 2, 3)

ref(l1)


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

## 2.5 Modify-in-place

x <- data.frame(matrix(runif(5 * 1e4), ncol = 5))
medians <- vapply(x, median, numeric(1))

for (i in seq_along(medians)) {
  x[[i]] <- x[[i]] - medians[[i]]
}

y <- as.list(x)

## 2.5.2 Environments
e1 <- rlang::env(a = 1, b = 2, c = 3)


x <- list()
x[[1]] <- x

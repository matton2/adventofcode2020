library(tidyverse)


example <- "1 + 2 * 3 + 4 * 5 + 6"

example2 <- "1 + (2 * 3) + (4 * (5 + 6))"

example3 <- "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"

day18 <- read_lines('day18/day18.txt')


# there are only + and * in the file.  since + and - both operate left to right, we can switch all the * to - but actually have the - operate as the *
`-` <- `*`
input <- gsub("\\*", "\\-", day18)
part1 <- sum(eval(as.call(c(c, parse(text = input)))))

# we will use the same input string that we modified above and now switch the + for * and make the * operate as +, kinda crazy, right?
`*` <- `+`
input2 <- gsub("\\+", "\\*", input)
part2 <- sum(eval(as.call(c(c, parse(text = input2)))))
format(part2, scientific = FALSE)

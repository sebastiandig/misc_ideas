
# make line with text
header1 <- cli::rule(
  left = crayon::bold("Something important"),
  right = crayon::blurred("something else"))
header2 <- cli::rule(center = crayon::yellow("Hello World"))
print(header1)
print(header2)
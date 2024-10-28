
"%&%" <- function(x, y) {
  assert_is_int(x)
  assert_is_int(y)
  bitwAnd(x, y)
}

"%|%" <- function(x, y) {
  assert_is_int(x)
  assert_is_int(y)
  bitwOr(x, y)
}

"%<<%" <- function(x, y) {
  assert_is_int(x)
  assert_is_int(y)
  bitwShiftL(x, y)
}

"%>>%" <- function(x, y) {
  assert_is_int(x)
  assert_is_int(y)
  bitwShiftR(x, y)
}


# Define multi_join function
.multi_join = function(join_function, list_to_be_joined, ...) {
  Reduce(
    function(x, y, ...) join_function(x, y, all = TRUE, ...),
    list_to_be_joined
  )
}


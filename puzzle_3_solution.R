library(magrittr)
count_trees = function(side_slope = 3, down_slope = 1){
  trees = scan("puzzle_3_input.txt", what = character(), sep = "\n")
  #side_slope = 3
  #down_slope = 1
  # we need to know how many times we need to replicate the pattern
  n_rep = ceiling(side_slope * length(trees) / nchar(trees[1]))
  
  full_trees = purrr::map_chr(trees, ~ paste0(rep(.x, n_rep), collapse = "")) %>%
    strsplit(., "")
  
  full_matrix = do.call(rbind, full_trees)
  
  use_row = 1
  use_col = 1
  n_tree = 0
  while (use_row < nrow(full_matrix)) {
    use_row = use_row + down_slope
    use_col = use_col + side_slope
    if (full_matrix[use_row, use_col] == "#") {
      n_tree = n_tree + 1
    }
  }
  n_tree
  
}

n_1 = count_trees(1, 1)
n_2 = count_trees(3, 1)
n_3 = count_trees(5, 1)
n_4 = count_trees(7, 1)
n_5 = count_trees(1, 2)

prod(n_1, n_2, n_3, n_4, n_5)

policies = scan("puzzle_2_input.txt", what = character(), sep = "\n")

# part 1
check_policy_password = function(query_string){
  parts = strsplit(query_string, " ", fixed = TRUE)[[1]]
  ranges = as.numeric(strsplit(parts[1], "-", fixed = TRUE)[[1]])
  letter_use = gsub(":", "", parts[2])
  all_char = strsplit(parts[3], "", fixed = TRUE)[[1]]
  n_match = sum(all_char %in% letter_use)
  if ((n_match >= ranges[1]) & (n_match <= ranges[2])) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

is_policy = purrr::map_lgl(policies, check_policy_password)

sum(is_policy)

# part 2
check_policy_password2 = function(query_string){
  parts = strsplit(query_string, " ", fixed = TRUE)[[1]]
  positions = as.numeric(strsplit(parts[1], "-", fixed = TRUE)[[1]])
  letter_use = gsub(":", "", parts[2])
  all_char = strsplit(parts[3], "", fixed = TRUE)[[1]]
  match_loc = which(all_char %in% letter_use)
  if (sum(match_loc %in% positions) == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
is_policy2 = purrr::map_lgl(policies, check_policy_password2)
sum(is_policy2)

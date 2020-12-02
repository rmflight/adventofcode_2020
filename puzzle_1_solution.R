# goal is to find two numbers that sum to 2020 based on the input.

input_nums = read.table("puzzle_1_input.txt", sep = "", header = FALSE)

all_combs = combn(input_nums$V1, 2)
comb_sums = colSums(all_combs)

match_2020 = which(comb_sums == 2020)
all_combs[1, match_2020] * all_combs[2, match_2020]

# goal is to find three numbers that sum to 2020
combs_3 = combn(input_nums$V1, 3)
match_3_2020 = which(colSums(combs_3) == 2020)
prod(combs_3[, match_3_2020])

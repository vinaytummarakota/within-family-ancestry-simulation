mother_chromosome_lengths <- c(
  3.65, 3.31, 2.70, 2.64, 2.45, 2.54,
  2.31, 2.24, 1.93, 2.11, 1.80, 2.14,
  1.30, 1.55, 1.36, 1.69, 1.49, 1.56,
  1.14, 1.21, 0.65, 0.74
)

father_chromosome_lengths <- c(
  2.16, 2.09, 1.90, 1.60, 1.51, 1.31,
  1.34, 1.13, 1.43, 1.38, 1.15, 1.28,
  0.95, 1.17, 1.10, 1.01, 1.05, 0.97,
  0.96, 0.81, 0.51, 0.49
)

generate_ancestry_proportion <- function(chromosome_lengths) {
  # intialize length of chromosomal segments that belong to the focal ancestry group to be zero
  focal_ancestry_length = 0

  for(L in chromosome_lengths) {
    # assume crossover events follow a Poisson process, consistent with Haldane's mapping
    num_crossover_events = rpois(n = 1, lambda = L)
    # assume crossover events take place uniformly and independently of one another along the chromosome, consistent with Haldane's mapping
    crossover_and_terminal_loci = c(0, sort(runif(n = num_crossover_events, min = 0, max = L)), L)
    # randomly choose which chromosome is selected during independent assortment
    is_focal_ancestry_group = sample(c(TRUE, FALSE), size = 1)
    for(l in seq_len(length(crossover_and_terminal_loci)-1)) {
      # add the length of the segment if it belongs to the focal ancestry group
      if(is_focal_ancestry_group) {
        focal_ancestry_length = focal_ancestry_length + (crossover_and_terminal_loci[l+1] - crossover_and_terminal_loci[l])
      }
      # flip the ancestry of the next segment due to crossover
      is_focal_ancestry_group = !is_focal_ancestry_group
    }
  }
  return(focal_ancestry_length / sum(chromosome_lengths))
}

generate_sibling_ancestry_proportion <- function() {
  maternally_inherited_ancestry_proportion <- generate_ancestry_proportion(mother_chromosome_lengths)
  paternally_inherited_ancestry_proportion <- generate_ancestry_proportion(father_chromosome_lengths)

  return((maternally_inherited_ancestry_proportion + paternally_inherited_ancestry_proportion)/2)
}




N = 10000 

sibship_deviations <- c()
sib1_ancestries <- c()
sib2_ancestries <- c()

for(n in 1:N) {
  sib1_ancestry = generate_sibling_ancestry_proportion()
  sib2_ancestry = generate_sibling_ancestry_proportion()

  sibship_avg_ancestry = (sib1_ancestry + sib2_ancestry) / 2

  sib1_ancestries <- c(sib1_ancestries, sib1_ancestry)
  sib2_ancestries <- c(sib2_ancestries, sib2_ancestry)
  sibship_deviations <- c(sibship_deviations, sib1_ancestry - sibship_avg_ancestry, sib2_ancestry - sibship_avg_ancestry)
}

variance_sibship_deviation <- var(sibship_deviations)
sprintf("Within-Family Variance in Ancestry: %.6f", variance_sibship_deviation)

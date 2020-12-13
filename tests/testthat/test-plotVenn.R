plotVenn(list(
  group_1 = list(
    "/1TB/Cloud/Lab/Projects/SleepSignature/workflow/results/20201209/edgeR/y_KCs/y_KCs_assay-counts_comparison-ZT20 sleep + ZT20 midline cross vs. ZT20 sleep deprivation + ZT8 wake + ZT8 wake stimulation + ZT2 14h SD + ZT8 20h SD_grouping-Condition_diff_table.csv"
  ), group_2 = list(
    "/1TB/Cloud/Lab/Projects/SleepSignature/workflow/results/20201209/edgeR/a_b_prime_KCs/a_b_prime_KCs_assay-counts_comparison-ZT20 sleep + ZT20 midline cross vs. ZT20 sleep deprivation + ZT8 wake + ZT8 wake stimulation + ZT2 14h SD + ZT8 20h SD_grouping-Condition_diff_table.csv"
  ), group_3 = list(
    "/1TB/Cloud/Lab/Projects/SleepSignature/workflow/results/20201209/edgeR/a_b_KCs/a_b_KCs_assay-counts_comparison-ZT20 sleep + ZT20 midline cross vs. ZT20 sleep deprivation + ZT8 wake + ZT8 wake stimulation + ZT2 14h SD + ZT8 20h SD_grouping-Condition_diff_table.csv"
  )
))


test_list <- list(
  group_1 = list(
    "/1TB/Cloud/Lab/Projects/SleepSignature/workflow/results/20201209/edgeR/y_KCs/y_KCs_assay-counts_comparison-ZT20 sleep + ZT20 midline cross vs. ZT20 sleep deprivation + ZT8 wake + ZT8 wake stimulation + ZT2 14h SD + ZT8 20h SD_grouping-Condition_diff_table.csv"
  ), group_2 = list(
    "/1TB/Cloud/Lab/Projects/SleepSignature/workflow/results/20201209/DESeq/y_KCs/y_KCs_assay-counts_comparison-ZT20 sleep + ZT20 midline cross vs. ZT20 sleep deprivation + ZT8 wake + ZT8 wake stimulation + ZT2 14h SD + ZT8 20h SD_grouping-Condition_diff_table.csv"
  ), group_3 = list(
    # "/1TB/Cloud/Lab/Projects/SleepSignature/workflow/results/20201209/scvi/y_KCs/y_KCs_assay-counts_comparison-ZT20 sleep + ZT20 midline cross vs. ZT20 sleep deprivation + ZT8 wake + ZT8 wake stimulation + ZT2 14h SD + ZT8 20h SD_grouping-Condition_change_diff_table.csv"
    "/1TB/Cloud/Lab/Projects/SleepSignature/workflow/results/20201209/MAST/y_KCs/y_KCs_assay-counts_comparison-ZT20 sleep + ZT20 midline cross vs. ZT20 sleep deprivation + ZT8 wake + ZT8 wake stimulation + ZT2 14h SD + ZT8 20h SD_grouping-Condition_diff_table.csv"
  )
)
plotVenn(test_list)

computeGeneEnsembles(computePartitionCounts(test_list), names(test_list))

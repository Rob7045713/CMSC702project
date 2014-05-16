# Query description

| Query file | Query string | Description | Time | Result |
| :---: | :---: | :---: | :---: | :---: |
| mean.xml | mean(exon_expr.raw_counts) where (exon_expr.barcode = TCGA-A6-5659-01A-01R-1653-07) | Average raw counts of a specific experiment | 24.663s | 504.0123264889981
| mean_2_constraints.xml | mean(exon_expr.raw_counts) where (exon_expr.barcode = TCGA-A6-5659-01A-01R-1653-07) and (exon_expr.raw_counts > 500) | Average raw counts that are above 500 of a specific experiment | 24.178s | 2943.928116187253
| mean_age_white.xml | mean(Patient.age_at_initial_pathologic_diagnosis) where (Patient.race = WHITE) | Average age at initial diagnosis of white patients | 23.482s | 2081/33
| mean_across_users.xml | mean(exon_expr.raw_counts) where (exon_expr.exon = chr10:100003848-100004653:+) | Average raw counts of a specific exon | 24.386s | 490.6

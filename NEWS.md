# IFFuzzyStats 0.1.0

## Initial CRAN release

* `ifv()` S3 class for intuitionistic fuzzy values (membership `mu`,
  non-membership `nu`, hesitation `pi = 1 - mu - nu`), with `as_ifv()`,
  `is_ifv()`, `if_pi()`, `if_check()` and `if_comp()` helpers.
* Xu (2007) arithmetic: `if_add()`, `if_mul()`, `if_scalar_mul()`,
  `if_pow()`, plus Einstein variants `if_einstein_add()`,
  `if_einstein_mul()`, `if_einstein_scalar_mul()`, `if_einstein_pow()`.
* Aggregation operators: `ifwa()`, `ifwg()`, `ifowa()`, `ifowg()`,
  `ifha()`, `ifhg()`, `if_einstein_wa()`, `if_einstein_wg()` and the
  `if_aggregate_matrix()` decision-matrix helper.
* Score / accuracy / ranking: `if_score()`, `if_accuracy()`,
  `if_hesitation()`, `if_rank()`, `if_order()`, `if_best()`.
* Entropy measures: `if_entropy()` with `"burillo"`, `"szmidt"`,
  `"vlachos"` and `"zeng_li"` methods.
* Distances: `if_dist()` with `"hamming"`, `"euclidean"`, `"hausdorff"`,
  `"ye"`, `"ye_cosine"` and `"park"` methods (optional element weights).
* Similarities: `if_sim()` with `"chen"`, `"hong_kim"`, `"ye_cosine"`,
  `"ye_weighted"`, `"deng"`/`"liang_shi"` (Jaccard) and `"dice"` methods.
* Approximate reasoning: max-min / max-product `if_compose()`,
  `if_infer()` (compositional rule of inference) and `if_rule_block()`.
* Clustering: intuitionistic fuzzy c-means `ifcmeans()` (with hesitation
  penalty `lambda` and `nstart` random starts) and hard `ifkmeans()`.
* Decision making: IF-TOPSIS `if_topsis()` (Boran et al. 2009) and the
  `if_rank_df()` ranking table.
* Converters: `if_fuzzify()` (numeric to IFV) and `if_defuzzify()`
  (`"score"`, `"expected"`, `"accuracy"`, `"mu"`).
* Vignette `IFFuzzyStats-intro`, full `testthat` suite, base-R only (no hard
  dependencies).

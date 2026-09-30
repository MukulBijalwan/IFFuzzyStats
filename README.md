# IFFuzzyStats: Intuitionistic Fuzzy Statistics Toolkit

Intuitionistic fuzzy sets (Atanassov, 1986) carry membership $\mu$,
non-membership $\nu$ ($\mu+\nu\le 1$) and hesitation $\pi=1-\mu-\nu$.
**IFFuzzyStats** is a zero-dependency (base-R only) toolkit covering:

- `ifv()` constructor + `if_add()`, `if_mul()`, `if_scalar_mul()`,
  `if_pow()` (Xu arithmetic) and Einstein variants
- aggregation: `ifwa()`, `ifwg()`, `ifowa()`, `ifowg()`, `ifha()`,
  `ifhg()`, `if_einstein_wa()`, `if_einstein_wg()`
- entropy: `if_entropy()` (Burillo, Szmidt-Kacprzyk, Vlachos-Sergiadis,
  Zeng-Li)
- distance: `if_dist()` (Hamming, Euclidean, Hausdorff, Ye, Ye-cosine, Park)
- similarity: `if_sim()` (Chen, Hong-Kim, Ye cosine/weighted, Jaccard/Deng,
  Dice)
- inference: `if_compose()`, `if_infer()`, `if_rule_block()`
- clustering: `ifcmeans()`, `ifkmeans()`
- decisions: `if_topsis()`, `if_rank()`, `if_rank_df()`
- converters: `if_fuzzify()`, `if_defuzzify()`, `if_score()`

## Install

```r
# from local source
install.packages("/path/to/IFFuzzyStats_0.1.0.tar.gz", repos = NULL, type = "source")
```

## Quick start

```r
library(IFFuzzyStats)
x <- ifv(c(0.6, 0.5, 0.7), c(0.3, 0.2, 0.1))
ifwa(x, c(0.5, 0.3, 0.2))
if_entropy(x, "szmidt")
if_sim(x, ifv(c(0.5, 0.4, 0.6), c(0.3, 0.3, 0.2)), "ye_cosine")
```

## References

Atanassov (1986) Fuzzy Sets and Systems 20(1); Xu (2007) IEEE TFS 15(6);
Szmidt & Kacprzyk (2000) FSS 114(3); Vlachos & Sergiadis (2007) PRL 28(2);
Ye (2011) MCM 53(1-2); Boran et al. (2009) ESWA 36(8).

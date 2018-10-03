## Clustering Ethnographic Atlas

This is supplementary materials for chapter 3 of my thesis "Computational methods for the study of post-marital residence"

This repository contains source data and code to reproduce work. It also contain number of additional figures and tables (albeit in a lower quality) that were not included in the chapter itself.

### Analysis

Analysis consisted of 4(5) steps:

* filtering
* (MCA)
* clustering
* evaluation of clustering (comparison)
* exploring the differences between clusters (variable comparison)

The file organization should more or less agree to these steps.

### Content
**Files:**
* **selection.txt** -- selection of EA variables for analysis
* **filtering.r** -- exploration of selected variables and their filtering
* **mca.r** -- performing MCA on EA data
* **cluster.r** -- clustering of EA and calculation of purity functions
* **cluster_mca.r** -- same as above but with the transformed data from MCA
* **comparison.r** -- evaluation of clustering outcomes
* **comparison_mca.r** -- same as above but for outcomes from MCA data
* **variable_comparison.r** -- exploring the differences in variables between clusters from the best clustering outcome
* **factominer.r** -- experimenting with FactoMineR package
* **Investigate.html** -- automatic analysis and interpretation with FactoInvestigate package

**Folders:**
* **data** -- contains the R version of Ethnographic Atlas and specification of parameters
* **figures** -- number of supplementary figures such as: purities and their penalizations, figure of the whole clustering outcome and for each explored number of clusters and tanglegrams between clustering outcomes
* **processed** -- raw processed data, tables and saved R objects of MCA and clustering. 
* **source** -- source code divided in individual "modules"
* **standalone** -- stand-alone tables, number of them were not presented in the main text

### Author's notes
I am not happy with the final file organization. I have tried to approach it from the point of view of "One file, one task", make the tasks files as clean as possible and have everything in number of modules.

However, as I have progressed and started running out of time, this got a bit messy. The individual "modules" got a bit more entangled and started to be less "one task files". Additionally, I got a bit tired of hiding everything, such as "call this single function on all these methods" in the source files. This could be easily solved, but would require quite a bit more time. And I doubt someone else will use this code anyway. Still, experience gained there will help in new projects.

The idea of modules was to have kind of lightweight package-like system. The system mostly utilizing `sys.source` which, unlike the more common `source`, can read source to particular environment and also run from path of loaded file (while the `source` is interpreted in the current active path).

I failed again to make it work exactly as I wanted. There were a few problems that I wasn't able to solve, mostly due to lack of time (I needed to analyze data, not build some system), but the basic idea worked quite well. From previous experience, the modules work well when you have several well-separated problems that do not share any code. Then you can easily make code like:

```
    task_1$main()
    task_2$main()
    ...
    task_n$main()
```

or something of this sort.

The idea to have something more lightweight stems from the fact that making packages is still a little bit more involved. And not exactly comfortable when the code itself is changing rapidly as analyzing data is rather exploratory in most cases.

Another unsuccessful attempt was to make tanglegrams of clustering outcomes. Good idea, but when a larger trees were used, a problem was quickly discovered. Tanglegrams are usually used with n-to-n mapping. But n-to-m should still not pose problem. The problem is that my case is more like n_i-to-m_j, since each cluster can map to multiple other clusters. So the lines will get messy quickly. This can be solved by rotating trees so that the biggest similarity is on or near diagonal elements of some cluster similarity matrix. First problem is that the diagonal elements is not just main diagonal of matrix, but some mapping to a higher dimension of n and m. Additionally, I must be able to actually rotate tree. But while the `ape` is awesome, it is far from perfect. It requires some particular structure of tree data. For some reason, this structure is not present in my trees. And when I want to rotate taxa, the result is significantly malformed. But rotation works well on randomly generated trees. However, plotting and everything else seems to work on my trees.

The code has been tested to work with [Racket](http://download.racket-lang.org/) 6.0.1 and 6.0.

Usage
==============

Translate
-------------
To translate custom XML descriptions to Intermine's XML descriptions:

> racket run-translate.rkt [desc1.xml] [desc2.xml] ...

Translation will create `desc.xml.out` for each `desc.xml`


Query
------------

To run query `query.xml` on database with descriptions under directory `descriptions-dir` and data under `data-dir`, run:

> racket run-query.rkt <query.xml> <descriptions-dir> <data-dir>

Additionally, flag `-v` dumps statistics.

> racket run-query.rkt -v ../queries/mean.xml ../descriptions ../data

Result:

> Done loading. Objects created:
> -- 239322 Exon(s)
> -- 5 Experiment(s)
> -- 1196610 exon_expr(s)
> Done loading. Objects created:
> -- 42 Experiment(s)
> -- 42 Patient(s)
> Query: mean(exon_expr.raw_counts) where (exon_expr.barcode = TCGA-A6-5659-01A-01R-1653-07)
> Query result: 504.0123264889981
>
> real  0m24.616s
> user  0m24.217s
> sys   0m0.433s

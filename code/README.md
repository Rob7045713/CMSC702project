The code has been tested to work with [Racket](http://download.racket-lang.org/) 6.0.1.

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

> racket run-query <query.xml> <descriptions-dir> <data-dir>

Additionally, flag `-v` dumps statistics.

> racket run-query -v ../queries/mean.xml ../descriptions ../data

Result:

> Done loading. Objects created:
>
> -- 239321 Exon(s)
>
> -- 5 Experiment(s)
>
> -- 1196605 exon_expr(s)
> 
> Done loading. Objects created:
>
> -- 41 Experiment(s)
>
> -- 41 Patient(s)
>
> Query result: 504.0117708015594
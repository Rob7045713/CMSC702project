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

To load database with description `desc.xml` from data files `data1.txt`, `data2.txt`, etc. and run query `query.xml`. Use flag `-v` for verbose mode.

> racket run-query <desc.xml> <query.xml> [data1.txt] [data2.txt] ...

For example:

> racket run-query -v ../file_formats/exon_expr.xml ../file_formats/query_mean.xml ../data/*__expression_exon.txt

Result:
> Done loading. Objects created:

> -- 239321 Exon(s)

> -- 5 Experiment(s)

> -- 1196605 exon_expr(s)

> Query result: 504.0117708015594

# Quast Evaluator

**Depends on Quast version 3.2**

# Install

* [pandoc](http://pandoc.org/) must be installed 

1.Clone this repository with `git clone https://github.com/pbelmann/quast-evaluator.git`
2.Change working directory: `cd quast-evaluator ` 
3.Run `R -e 'install.packages("packrat" , repos="http://cran.us.r-project.org"); packrat::restore()'` 

## User Guide

Run 

`Rscript quast_evaluator_cli.r  /path/to/assemblers.tsv   /path/to/info.tsv  -o /path/to/output/directory`

where assemblers.tsv and info.tsv must have the following columns:

* assemblers.tsv:

~~~BASH
assembler       path
jolly_euclid_1  /home/belmann/projects/quast-evaluator/tests/testthat/data/assembler1
angry_newton_2  /home/belmann/projects/quast-evaluator/tests/testthat/data/assembler2
~~~

where `/home/belmann/projects/quast-evaluator/tests/testthat/data/assembler1` must contain the directories:

  * `runs_per_reference`
  * `combined_reference`

* info.tsv:

~~~BASH
id    gc    order   path  label	group
1030752.gt1kb   66.49   15.414285       1030752.gt1kb   refA	strain
1030755.gt1kb   62.12   12.66829        1030755.gt1kb   refB	strain
1030836.gt1kb   41.31   3.1131791       1030836.gt1kb   refC	evolved
1030878.gt1kb   68.33   1.5677500       1030878.gt1kb   refD	evolved
~~~

where 
  * `path` is a directory inside the `runs_per_reference` directory.
  * `id` is a uniq identifier
  * `order` is column which defines an reference order (i.e.: coverage, abundance, length, etc ...)
  * `label` is a custom label for the reference 
  * `group` is an identfier which groups different references

### Example

  See [tests](tests/testthat/test_quast_evaluator.r) for example.

## Developer Guide

You can run tests with

~~~BASH
R -e 'devtools::test()'
~~~

Please use this if you want to propose a Pull Request.

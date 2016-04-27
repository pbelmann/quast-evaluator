# Quast Evaluator

**Depends on Quast version 3.2**

# Install

* [pandoc](http://pandoc.org/) must be installed 

1.Clone this repository with `git clone https://github.com/pbelmann/quast-evaluator.git`

2.Change working directory: `cd quast-evaluator ` 

3.Run `R -e 'install.packages("packrat" , repos="http://cran.us.r-project.org"); packrat::restore()'` 

## User Guide

Quast Evaluator has two modes:

### 'new' Mode

This mode lets you collect quast results from different quast output directories.


Run 

`Rscript quast_evaluator_cli.r  new  /path/to/assemblers.tsv   /path/to/info.tsv  -o /path/to/output/directory`

where assemblers.tsv and info.tsv must have the following columns:

* assemblers.tsv:

~~~BASH
assembler       path	group
jolly_euclid_1  /home/belmann/projects/quast-evaluator/tests/testthat/data/assembler1	1
angry_newton_2  /home/belmann/projects/quast-evaluator/tests/testthat/data/assembler2	2
~~~

where `/home/belmann/projects/quast-evaluator/tests/testthat/data/assembler1` must contain the directories:

  * `runs_per_reference`
  * `combined_reference`

* info.tsv:

~~~BASH
id    gc    abundance   path  label	group	length	mapping	
1030752.gt1kb   66.49   15.414285       1030752.gt1kb   refA	strain	1200	43000
1030755.gt1kb   62.12   12.66829        1030755.gt1kb   refB	strain	2300	89999
1030836.gt1kb   41.31   3.1131791       1030836.gt1kb   refC	evolved	4500	76666
1030878.gt1kb   68.33   1.5677500       1030878.gt1kb   refD	evolved	8999	50000
~~~

where 
  * `path` is a directory inside the `runs_per_reference` directory.
  * `id` is a uniq identifier
  * `abundance` is column for reference abundance
  * `label` is a custom label for the reference 
  * `group` is an identfier which groups different references
  * `length` is the length of the reference
  * `mapping` how many reads are mapped to the specific reference

### 'reuse' Mode

This mode lets you reuse tsv files produced by the 'new' mode.

Run 

`Rscript quast_evaluator_cli.r  reuse  /path/to/combined_ref_data.tsv   /path/to/ref_data.tsv  -o /path/to/output/directory`

where combined_ref_data.tsv and ref_data.tsv have the columns stated in these example files: [tests/testthat/data/ref_data.tsv](tests/testthat/data/ref_data.tsv) and [tests/testthat/data/combined_ref_data.tsv](tests/testthat/data/combined_ref_data.tsv) 

### Further Parameters:

  * '-c  /path/to/plot.conf'  This file allows you to define min and max for each plot individually. The [here](tests/testthat/data/plots_conf.tsv) for an example.  

### Example

  See [tests](tests/testthat/test_quast_evaluator.r)

## Developer Guide

You can run tests with

~~~BASH
R -e 'devtools::test()'
~~~

Please use this if you want to propose a Pull Request.

# AnaDiff : A tool for differential analysis of microarray and rnaseq

This set of scripts allows differential analysis of microarrays and rnaseq.

- microarrays: 
  - suitable for Agilent chips scanned by the Inopsys scanner.
  - Using Limma and httr packages.

- rnaseq: 
  - use on .sf files output from Salmon or on .counts files output from samtools.
  - Using edgeR and DEseq2 packages.

## Create Rscript with the makefile script

### For rnaseq

Go to the rnaseq folder.

```{}
cd ./scripts/rnaseq
```

Ask the Makefile programme.

```{}
make
```

A new Rscript is create with as name : make_anaDiff_RNAseq_<date>.R

### for agilent

Go to the rnaseq folder.

```{}
cd ./scripts/agilent
```

Ask the Makefile programme.

```{}
make
```

A new Rscript is create with as name : make_anaDiff_agilent_<date>.R

## Using script on data

First you have to create a summary file that describe yours comparisons and theirs samples.

### rnaseq summary

Create a tabular file with one line for each sample of each comparison :

- Comparison : the name of the comparison. Must be the same for all line of it.
- File : The path of the file containing reads counts
- Name : the name of the sample source (the same of each biological repet)
- Group : indication if this sample is a control (denominator) or a traitement (numerator) for the ratio calculation

In the example below, we have two comparisons (CompA_S1-S2 and CompB_S1-S3).
The comparison CompA_S1-S2 is made up of 4 samples, two control samples (sample_2a and sample_2b) whose source sample name is S2 and two traited samples (sample_1a and sample_1b) whose source sample name is S1.

```{}
Comparison   File                    Name   Group
CompA_S1-S2  ../quant/sample_1a.sf   S1     Ttmt
CompA_S1-S2  ../quant/sample_1b.sf   S1     Ttmt
CompA_S1-S2  ../quant/sample_2a.sf   S2     Control
CompA_S1-S2  ../quant/sample_2b.sf   S2     Control
CompB_S1-S3  ../quant/sample_1a.sf   S1     Ttmt
CompB_S1-S3  ../quant/sample_1b.sf   S1     Ttmt
CompB_S1-S3  ../quant/sample_3a.sf   S3     Control
CompB_S1-S3  ../quant/sample_3b.sf   S3     Control
```
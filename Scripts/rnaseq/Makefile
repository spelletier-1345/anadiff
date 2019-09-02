rfiles=./src/*.R
now=$(shell date +%Y%m%d)
fileDate=make_anaDiff_RNAseq_$(now).R

$(fileDate): $(rfiles)
	for rfile in $(rfiles) ; do \
		echo >> $@; \
		sed '/^source/d' $$rfile >> $@; \
	done

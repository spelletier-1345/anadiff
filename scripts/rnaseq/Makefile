rfiles=./src/*.R
now=$(shell date +%Y%m%d)
fileDate=make_anaDiff_RNAseq_$(now).R

$(fileDate): $(rfiles)
	cat ../licence > $@ ;
	for rfile in $(rfiles) ; do \
		echo >> $@; \
		sed '1,15d' $$rfile >> $@ ; \
	done
	sed -i '/^source/d' $@ ;
	sed -i 's/DD-MM-YYYY/$(shell date +%d-%m-%Y)/g' $@


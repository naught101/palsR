
build: 
	R CMD build .

install: 
	R CMD INSTALL .

clean:
	rm -rf pals_*.tar.gz


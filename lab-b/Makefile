all: ising.py

ising.py: ising.fut
	futhark-pyopencl --library $<

run: ising.py
	python ising-gui.py

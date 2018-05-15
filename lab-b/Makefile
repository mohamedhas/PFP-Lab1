all: ising.py

ising.py: ising.fut
	/home/moh/.local/bin/futhark-pyopencl --library $<

run: ising.py
	python ising-gui.py

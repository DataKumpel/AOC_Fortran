COMPILER = gfortran
FLAGS = -g -std=f2008 -Wall
CH_08 = ./build/ch_08.exe
CH_09 = ./build/ch_09.exe
CH_10 = ./build/ch_10.exe

all:
	$(COMPILER) ./src/challenge_08.f90 -o $(CH_08) $(FLAGS)
	$(COMPILER) ./src/challenge_09.f90 -o $(CH_09) $(FLAGS)
	$(COMPILER) ./src/challenge_10.f90 -o $(CH_10) $(FLAGS)

day8:
	$(COMPILER) ./src/challenge_08.f90 -o $(CH_08) $(FLAGS)
	$(CH_08)

day9:
	$(COMPILER) ./src/challenge_09.f90 -o $(CH_09) $(FLAGS)
	$(CH_09)

day10:
	$(COMPILER) ./src/challenge_10.f90 -o $(CH_10) $(FLAGS)
	$(CH_10)
#!/usr/local/bin/python3
import sys
import graph
import read


def main():
	if len(sys.argv) != 2:
		print("Usage: ./main.py [filename]", file=sys.stderr)
		sys.exit()
	try:
		with open(sys.argv[1]) as f:
			line_list = f.readlines()
	except FileNotFoundError:
		print("Invalid filename", file=sys.stderr)
		sys.exit()

	G = graph.Graph()

	for line in line_list:
		read.read_instruction(G, line)

if (__name__ == "__main__"):
	main()

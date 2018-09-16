import graph
import string


def __read_double_implication(G: graph.Graph, instruction: str):
	pass


def __read_single_implication(G: graph.Graph, instruction: str):
	both_sides = instruction.split("=>")
	if len(both_sides) != 2:
		print("Wrong rule format: " + instruction)
		quit()
	left_side = both_sides[0]
	right_side = both_sides[1]
	G.add_edge(left_side, right_side)


def __read_rule(G: graph.Graph, instruction: str):
	if (instruction.find("=>") >= 0):
		__read_single_implication(G, instruction)
	elif (instruction.find("=>") >= 0):
		__read_double_implication(G, instruction)
	else:
		print("Wrong rule format: " + instruction)
		quit()


def __read_truth(G: graph.Graph, instruction: str):
	truths = [l for l in instruction[1:] if l in string.ascii_uppercase]
	for t in truths:
		G.add_truth(t)
	

def __read_query(G: graph.Graph, instruction: str):
	queries = [l for l in instruction[1:] if l in string.ascii_uppercase]
	for q in queries:
		marked = graph.Search(G, q).marked()
		truths = G.truths()
		common = [t for t in truths if t in marked]
		print(f"{q}: {not not common}")


def __clean_line(string: str)->str:
	index = string.find("#")
	if index >= 0:
		string = string[:index]
	return string.strip().replace(" ", "")


def read_instruction(G: graph.Graph, instruction: str):
	instruction = __clean_line(instruction)
	if instruction == "":
		return
	elif instruction[0] == "=":
		__read_truth(G, instruction)
	elif instruction[0] == "?":
		__read_query(G, instruction)
	else:
		__read_rule(G, instruction)

import string


class Graph:
	def __init__(self):
		self._adjacency = {i: [] for i in string.ascii_uppercase}
		self._truth = []


	def vertices(self)->list:
		return [key for key, value in self._adjacency.items()]
		

	def adj(self, vertex: str)->list:
		return self._adjacency[vertex]
		

	def add_edge(self, src: str, dst: str):
		# création de vertex à la volée
		# création automatique de règles
		# probablement besion de remplacer par des sets
		self._adjacency[dst].append(src)
		

	def truths(self)->list:
		return self._truth


	def add_truth(self, vertex: str):
		self._truth.append(vertex)

	# def __str__(self):
	# 	strings = []
	# 	for v_name, adj_list in self._adjacency.items():
	# 		if adj_list is None:
	# 			continue
	# 		string = f"Vertex {v_name}:\n"
	# 		for edge in adj_list:
	# 			string.append(f"{edge}")
	# 		strings.append(string)
	# 	return ("\n".join(strings))
		


class Search:
	def __init__(self, G: Graph, s: str):
		self._marked = {vertex: False for vertex in G.vertices()}
		self._dfs(G, s)


	def marked(self):
		return [v for v, m in self._marked.items() if m is True]


	def _dfs(self, G:Graph, s: str):
		if s in self.marked():
			return
		self._marked[s] = True
		for edge in G.adj(s):
			self._dfs(G, edge)
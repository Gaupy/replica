type load = float (*The load of the server*)
type node = { n : int ; mutable w : load }
type input = float
type index_spe = int (*The index of the speed on the speed_table*)

type server_tree = 
  |Noeud of ((server_tree list) * node * input)

type full_tree =
  |Server of ((full_tree list) * node * input)
  |Node of ((full_tree list) * input)

type id = int
type full_tree_indexed =
  |NodeIndexed of ((full_tree list) * input * id)

type serv_prop = {mutable s : index_spe ; mutable load : load} (*load \leq tab_of_spe.(s)*)

type server_tree_int = (*tarbre list represents the list of sons of the ServerNode,  node reprents the identity (node.n) of the node, and its load (node.w), serv_prop is the property of the server.*)
  | ServerInt of ((server_tree_int list) * node * serv_prop)

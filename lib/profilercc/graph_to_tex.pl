:- module(graph_to_tex, [
            graph_to_tex/3,
% types:
            graph/1, node/1, edge/1, label/1],
        [hiord, assertions, regtypes]).

:- use_module(library(hiordlib), [minimum/3, maplist/3]).
:- use_module(engine(messages_basic), [lformat/1]).

:- doc(author, "Teresa Trigo").
:- doc(author, "Edison Mera").

:- doc(module, "
    We represent our call-graph in the following way
      G = (N,E) -- A graph is a set of nodes and edges
       Each node is a pair (Name, Time)
        Name: the name of the node
        Time: the cumulated time in the cost center
       Each edge is a tuple (Origin, Destiny, Time)
        Origin  : the name of the node origin
        Destiny : the name of the node origin
        Time    : the cumulated time in the cost center Destiny 
        when it is called from the cost center Origin").

%% Convert a prolog graph into its latex code

:- regtype graph/1 # "Represents a graph.".
graph(g(Nodes, Edges)) :-
    list(node, Nodes),
    list(edge, Edges).

:- regtype node/1 # "A node of a graph.".
node(n(A, T)) :-
    label(A),
    num(T).

:- regtype edge/1 # "An edge of a graph.".
edge(e(A, B, T)) :-
    label(A),
    label(B),
    num(T).

:- regtype label/1 # "A label of a node.".
label(A) :- atm(A).

:- pred graph_to_tex(NodeField, EdgeFields, Graph) :: atm * list(atm) *
    graph #
"Converts a graph defined in prolog to latex (in dot format) and writes
    a table in which the size of each node is explicit. The method is 
    parametric with respect to the metric of the size of the nodes".

graph_to_tex(_, _, g([], _)) :- !.
graph_to_tex(_, _, g(_, [])) :- !.
graph_to_tex(NodeField, EdgeFields, g(Nodes, Edges)) :-
    lformat('\\begin{dot2tex}[dot, straightedges, options=-tmath]\n'),
    lformat('digraph G {\n'),
    lformat('ranksep="0.0";\n'),
    lformat('mindist="0";\n'),
    lformat('node [shape=circle];\n'),
    transform(Nodes, NodesTrans),
    rename_nodes(NodesTrans, NodesRen, Equiv, 1),
    rename_edges(Equiv, Edges, EdgesRen),
    nodes_to_tex(NodesRen),
    edges_to_tex(EdgesRen, EdgeFields),
    lformat('}\n'),
    lformat('\\end{dot2tex}\n'),
    lformat('\\\\\n'),
    lformat('\\\\\n'),
    print_equivalences(NodeField, Equiv, Nodes).

:- pred print_equivalences(Field, Equiv, Nodes) #
"Prints a list of (cost_center,node_in_the_graph,size_of_node) in a 
    latex table.".
print_equivalences(Field, Equiv, Nodes) :-
    lformat('\\begin{tabular}{|c|c|c|}\n'),
    lformat('\\hline\n'),
    print_header(Field),
    lformat('\\hline\n'),
    print_equivalences_(Equiv, Nodes),
    lformat('\\end{tabular}\n').

print_equivalences_([],              []).
print_equivalences_([(CC, N)|Equiv], [n(CC, Size)|Nodes]) :-
    lformat(['cc', N, '&', ''(CC), '&', Size, '\\\\\n']),
    lformat('\\hline\n'),
    print_equivalences_(Equiv, Nodes).

print_header(counts) :-
    lformat('Node & Cost Center Name & Cost Center Size (Counts)\\\\\n').
print_header((counts, per)) :-
    lformat('Node & Cost Center Name & Cost Center Size (Counts \\%)\\\\\n').
print_header(ticks) :-
    lformat('Node & Cost Center Name & Cost Center Size (Ticks)\\\\\n').
print_header((ticks,per)) :-
    lformat('Node & Cost Center Name & Cost Center Size (Ticks \\%)\\\\\n').
print_header(time(_)) :-
    lformat(
        'Node & Cost Center Name & Cost Center Size (Execution Time)\\\\\n').
print_header((time(_),per)) :-
    lformat(
        'Node & Cost Center Name & Cost Center Size (Execution Time \\%)\\\\\n').
print_header(Values) :-
    lformat(
        ['Node & Cost Center Name & Cost Center Size (', Values, ')\\\\\n']).
print_header((Values,per)) :-
    lformat(
        ['Node & Cost Center Name & Cost Center Size (', Values, '\\%)\\\\\n']).


nodes_to_tex([]).
nodes_to_tex([n(N, T)|Nodes]) :-
    T0 is T/10,
    color(T, Color),
    lformat(['cc', N, '[margin="', T0, '", style="fill= ', Color, '!20"];\n']),
    nodes_to_tex(Nodes).

%% Color the nodes of the graph
%  Ranges can be modified if necessary
color(T, Color) :-
    T < 1.25,
    !,
    Color = green.
color(T, Color) :-
    T < 1.5,
    !,
    Color = yellow.
color(T, Color) :-
    T < 1.75,
    !,
    Color = orange.
color(_, red).

edges_to_tex([], _).
% Special cases of formatting:
edges_to_tex([e(O, D, Values)|Edges], EdgeFields) :-
    (
        EdgeFields =
        [call_exits_t, call_fails_t, redo_exits_t, redo_fails_t] ->
        Values = [C_E, C_F, R_E, R_F],
        (
            O = D ->
            lformat(['cc', O, '->', 'cc', O, ' [label="', C_E, ' ', C_F,
                    ',', R_E, R_F, '", topath="loop right"];\n'])
        ;
            lformat(['cc', O, '->', 'cc', D, ' [label="', C_E, ' ', C_F,
                    ',', R_E, ' ', R_F, '"];\n'])
        )
    ;
        (
            O = D ->
            lformat(['cc', O, '->', 'cc', O, ' [label="', Values,
                    '", topath="loop right"];\n'])
        ;
            lformat(['cc', O, '->', 'cc', D, ' [label="', Values, '"];\n'])
        )
    ),
    edges_to_tex(Edges, EdgeFields).

%% Transform a list of nodes (each time between 1 and 2)
transform(Nodes, NodesNorm) :-
    minimum(Nodes, min_node, n(_, Min)),
    minimum(Nodes, max_node, n(_, Max)),
    maplist(transform_node(Min, Max), Nodes, NodesNorm).

min_node(n(_, X), n(_, Y)) :- X < Y.
max_node(n(_, X), n(_, Y)) :- X > Y.

transform_node(Min, Max, n(N, T), n(N, T1)) :-
    T1 is 1 + (T - Min)/(Max - Min).

% We have seen that if the names of the nodes are not uniform (in the sense of 
% having the same length) we can not guarantee that the size of the nodes 
% reflects the cumulated time in the cost center because if the node label 
% is bigger than the node size value dot ignores our specification and adapts 
% automatically the size of the node. The following predicates rename nodes 
% (both in nodes and edges) to 1,2,3,...

%% Renames a list of nodes to 1, 2, 3, ...

rename_nodes([],              [],                  _,               _).
rename_nodes([n(N, T)|Nodes], [n(N1, T)|NodesRen], [(N, N1)|Equiv], N1) :-
    N2 is N1 + 1,
    rename_nodes(Nodes, NodesRen, Equiv, N2).

%%And the nodes in the edges

rename_edges([],              Edges, Edges).
rename_edges([(N, N1)|Equiv], Edges, EdgesRen) :-
    maplist(renamenode(N, N1), Edges, EdgesRen1),
    rename_edges(Equiv, EdgesRen1, EdgesRen).

renamenode(N, N1, e(N, N, T), e(N1, N1, T)) :- !.
renamenode(N, N1, e(N, D, T), e(N1, D,  T)) :- !.
renamenode(N, N1, e(O, N, T), e(O,  N1, T)) :- !.
renamenode(_, _,  e(O, D, T), e(O,  D,  T)).


%% Tests
%graph_to_tex(g([n(p1,1),n(p2,2),n(q,5)],[e(p1,q,2),e(p2,q,5)])).
%rename([n(p1,1),n(p2,2),n(q,5)],X,[e(p1,q,_),e(p2,q,_)],Y,Z).

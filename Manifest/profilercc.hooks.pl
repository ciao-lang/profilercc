:- module(_, [], [ciaobld(bundlehooks)]).

:- doc(title,  "Bundle Hooks for profilercc").

:- use_module(ciaobld(ciaoc_aux), [clean_tree/1]).
:- use_module(library(pathnames), [path_dirname/2]).
:- use_module(library(stream_utils), [string_to_file/2]).
:- use_module(library(bundle/bundle_paths), [bundle_path/3]).
:- use_module(library(llists), [flatten/2]).
:- use_module(engine(stream_basic), [absolute_file_name/2]).

'$builder_hook'(lib('lib/profilercc')).
'$builder_hook'(prepare_build_bin) :-
    gen_decl_auto.
'$builder_hook'(clean_bin) :- !,
    % TODO: Remove hook if lib/2 is treated by clean_bin
    clean_tree(~bundle_path(profilercc, 'lib/profilercc')).

h_path(Spec, Path) :-
    absolute_file_name(Spec, File),
    path_dirname(File, Path).

% TODO: generalize, share with other foreign code
% TODO: add wrappers for compiler opts instead (ala pp package)
gen_decl_auto :-
    h_path(library(hrtime/hrtime), Path1),
    h_path(library(hashtable/hashtable), Path2),
    flatten(["%Do not edit generated automatically\n\n",
             ":- extra_compiler_opts('-I", ~atom_codes(Path1), "').\n",
             ":- extra_compiler_opts('-I", ~atom_codes(Path2), "').\n"], T),
    string_to_file(T, ~bundle_path(profilercc, 'lib/profilercc/profilercc_decl_auto.pl')).

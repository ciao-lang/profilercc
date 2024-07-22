:- module(_, [], [doccfg]).

%! \title Config for profilercc reference manual

:- include(core_docsrc(docpaths)).

filepath := at_bundle(profilercc, 'lib').
filepath := ~ciaofilepath_common.

output_name := 'profilercc'.

doc_structure :=
    % Profiler with cost centers
    'profilercc/profiler_doc'-[
        'profilercc/profiler_utils',
        'profilercc/profiler_auto_conf'].

bibfile := ~ciao_bibfile.

allow_markdown   := yes.
syntax_highlight := yes.

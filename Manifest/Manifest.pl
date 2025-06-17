:- bundle(profilercc).
version('1.25.0'). % (same as 'core')
depends([core]).
alias_paths([
    library = 'lib'
]).
lib('lib').
manual('profilercc', [main='doc/SETTINGS.pl']).

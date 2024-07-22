% TODO: May not work in 64-bits! see hashtab.h; ub4 is defined as 'long'
:- module(hashtable, _, [assertions, foreign_interface]).

:- use_foreign_source(library(hashtable/recycle)).
:- use_foreign_source(library(hashtable/lookupa)).
:- use_foreign_source(library(hashtable/hashtab)).
:- use_foreign_source(library(hashtable/hashtable)).

:- trust pred ht_create(in(LogSize), go(HTab)) :: c_long * address + (foreign, returns(HTab)).

:- trust pred ht_destroy(in(HTab)) :: address + (foreign).

:- trust pred ht_count(in(HTab), go(Count)) :: address * c_long + (foreign(ht_count_),
   returns(Count)).

% NOTE: Keys are stored as arrays of chars, but they are not \0-ended
% (so we cannot translate easily to atoms)

:- trust pred ht_key(in(HTab), go(Key)) :: address * address +
   (foreign(ht_key_), returns(Key)).

:- trust pred ht_keyl(in(HTab), go(Keyl)) :: address * c_long +
   (foreign(ht_keyl_), returns(Keyl)).

:- trust pred ht_stuff(in(HTab), go(Stuff)) :: address * address +
   (foreign(ht_stuff_), returns(Stuff)).

% TODO: Missing predicates for (address,length)<->atom conversion

:- trust pred ht_find(in(HTab), in(Key), in(Keyl), go(Find)) :: 
   address * address * c_long * c_long + (foreign, returns(Find)).

:- trust pred ht_add(in(HTab), in(Key), in(Keyl), in(Stuff), go(Add)) ::
   address * address * c_long * address * c_long + (foreign, returns(Add)).

:- trust pred ht_add2(in(HTab), in(Key), in(Stuff), go(Add)) ::
   address * atm * address * c_long + (foreign, returns(Add)).

:- trust pred ht_del(in(HTab), go(Del)) :: address * c_long + (foreign,
   returns(Del)).

:- trust pred ht_first(in(HTab), go(First)) :: address * c_long + (foreign,
   returns(First)).

:- trust pred ht_next(in(HTab), go(Next)) :: address * c_long + (foreign(ht_next_),
   returns(Next)).

:- trust pred ht_nbucket(in(HTab), go(NBucket)) :: address * c_long +
   (foreign, returns(NBucket)).

:- trust pred ht_stat(in(Htab)) :: address + (foreign).

% TODO: wrong type in ht_add2 (Stuff is not an address)
% add_rec(_,[]).
% add_rec(HashTable,[D|Ds]) :-
%       ht_add2(HashTable, D, '', _X),
%       add_rec(HashTable,Ds).

% main :-
%       Dictionary=[one,two,tree,four,five,six,two,four],
%       ht_create(8, HashTable),
%       add_rec(HashTable,Dictionary),
%       ht_destroy(HashTable).

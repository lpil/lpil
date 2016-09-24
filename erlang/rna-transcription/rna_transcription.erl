-module(rna_transcription).
-export([to_rna/1]).

to_rna(Strand) -> lists:map(fun to_rna_base/1, Strand).

to_rna_base($G) -> $C;
to_rna_base($C) -> $G;
to_rna_base($T) -> $A;
to_rna_base($A) -> $U.

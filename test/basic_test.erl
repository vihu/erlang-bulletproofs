-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    {ok, PCGens} = erlang_bulletproofs:pedersen_gens_default(),
    {ok, BPGens} = erlang_bulletproofs:bulletproof_gens_new(64, 1),
    Secret = 1037578891,
    {ok, Blinding} = erlang_bulletproofs:scalar_random(),
    {ok, ProverTranscript} = erlang_bulletproofs:transcript_new(<<"doctest example">>),
    {ok, RP} = erlang_bulletproofs:range_proof_prove_single(BPGens, PCGens, ProverTranscript, Secret, Blinding, 32),
    {ok, VerifierTranscript} = erlang_bulletproofs:transcript_new(<<"doctest example">>),
    Res = erlang_bulletproofs:range_proof_verify_single(RP, BPGens, PCGens, VerifierTranscript, 32),
    ?assertEqual(true, Res).

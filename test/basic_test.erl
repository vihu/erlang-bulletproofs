-module(basic_test).

-include_lib("eunit/include/eunit.hrl").

example_test() ->
    %% generators for pedersen commitments, independent of bulletproofs generators
    {ok, PCGens} = erlang_bulletproofs:pedersen_gens_default(),

    %% generators for bulletproofs
    {ok, BPGens} = erlang_bulletproofs:bulletproof_gens_new(64, 1),

    %% A secret we will prove lies within [0, 2^32)
    Secret = 1037578891,

    %% A random blinding factor
    {ok, Blinding} = erlang_bulletproofs:scalar_random(),

    %% Initial transcript with doctest domain separator
    {ok, ProverTranscript} = erlang_bulletproofs:transcript_new(<<"doctest example">>),

    %% Create a 32-bit rangeproof
    {ok, RP} = erlang_bulletproofs:range_proof_prove_single(BPGens, PCGens, ProverTranscript, Secret, Blinding, 32),

    %% Verification requires a transcript with identical initial state
    {ok, VerifierTranscript} = erlang_bulletproofs:transcript_new(<<"doctest example">>),
    Correct = erlang_bulletproofs:range_proof_verify_single(RP, BPGens, PCGens, VerifierTranscript, 32),
    ?assertEqual(true, Correct),

    %% This one should fail
    {ok, FakeVerifierTranscript} = erlang_bulletproofs:transcript_new(<<"why are you lying">>),
    Incorrect = erlang_bulletproofs:range_proof_verify_single(RP, BPGens, PCGens, FakeVerifierTranscript, 32),
    ?assertEqual(false, Incorrect),

    %% This one should also fail, since secret does not lie within [0, 2^8)
    AnotherIncorrect = erlang_bulletproofs:range_proof_verify_single(RP, BPGens, PCGens, VerifierTranscript, 8),
    ?assertEqual(false, AnotherIncorrect),

    %% And this one should fail too, because fake verifier transcript
    AndAnotherIncorrect = erlang_bulletproofs:range_proof_verify_single(RP, BPGens, PCGens, FakeVerifierTranscript, 8),
    ?assertEqual(false, AndAnotherIncorrect).

-module(erlang_bulletproofs).

-export([load/0]).
-on_load(load/0).

-export([pedersen_gens_default/0,
         bulletproof_gens_new/2,
         range_proof_from_bytes/1,
         transcript_new/1,
         scalar_random/0,
         range_proof_prove_single/6,
         range_proof_verify_single/5
        ]).

load() ->
    erlang:load_nif(filename:join(priv(), "libnative"), none).

-spec pedersen_gens_default()-> {ok, reference()} | {error, any()}.
pedersen_gens_default() ->
    not_loaded(?LINE).

-spec bulletproof_gens_new(GensCapacity :: pos_integer(),
                           PartyCapacity :: pos_integer())-> {ok, reference()} | {error, any()}.
bulletproof_gens_new(_, _) ->
    not_loaded(?LINE).

-spec range_proof_from_bytes(Bytes :: binary())-> {ok, reference()} | {error, any()}.
range_proof_from_bytes(_) ->
    not_loaded(?LINE).

-spec transcript_new(Bytes :: binary())-> {ok, reference()} | {error, any()}.
transcript_new(_) ->
    not_loaded(?LINE).

-spec scalar_random()-> {ok, reference()} | {error, any()}.
scalar_random() ->
    not_loaded(?LINE).

-spec range_proof_prove_single(
        reference(),
        reference(),
        reference(),
        pos_integer(),
        reference(),
        pos_integer()
       )-> {ok, reference()} | {error, any()}.
range_proof_prove_single(_, _, _, _, _, _) ->
    not_loaded(?LINE).

-spec range_proof_verify_single(
        reference(),
        reference(),
        reference(),
        reference(),
        pos_integer()
       )-> {ok, reference()} | {error, any()}.
range_proof_verify_single(_, _, _, _, _) ->
    not_loaded(?LINE).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv()->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end.


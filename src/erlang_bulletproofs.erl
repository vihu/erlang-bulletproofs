-module(erlang_bulletproofs).

-export([load/0]).
-on_load(load/0).

-export([pedersen_gens_default/0,
         bulletproof_gens_new/2,
         range_proof_from_bytes/1
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


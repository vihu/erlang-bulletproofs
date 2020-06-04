extern crate bulletproofs;
extern crate rustler;
extern crate hex;

use rustler::{Env, Term};

mod atoms;
mod pedersen_gens;
mod bulletproof_gens;
mod range_proof;

fn load(env: Env, _: Term) -> bool {
    pedersen_gens::load(env);
    bulletproof_gens::load(env);
    range_proof::load(env);

    true
}

rustler::init!(
    "erlang_bulletproofs",
    [
        // PedersenGens API
        pedersen_gens::default,
        // BulletproofGens API
        bulletproof_gens::new,
        // RangeProof API
        range_proof::from_bytes,
    ],
    load = load
);

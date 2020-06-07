extern crate bulletproofs;
extern crate rustler;
extern crate hex;
extern crate curve25519_dalek;
extern crate rand;
extern crate merlin;

use rustler::{Env, Term};

mod atoms;
mod pedersen_gens;
mod bulletproof_gens;
mod range_proof;
mod transcript;
mod scalar;
mod compressed_ristretto;

fn load(env: Env, _: Term) -> bool {
    pedersen_gens::load(env);
    bulletproof_gens::load(env);
    range_proof::load(env);
    transcript::load(env);
    scalar::load(env);
    compressed_ristretto::load(env);

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
        range_proof::prove_single,
        range_proof::verify_single,
        range_proof::verify_multiple,
        // Transcript API
        transcript::new,
        // Scalar API
        scalar::random,
        // CompressedRistretto API
        compressed_ristretto::from_slice,
    ],
    load = load
);

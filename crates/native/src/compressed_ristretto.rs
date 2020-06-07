use crate::atoms::ok;
use rustler::{Atom, Env, ResourceArc, Binary};
use curve25519_dalek::ristretto::CompressedRistretto;

/// Struct to hold PedersenGens
pub struct CompressedRistrettoResource {
    pub compressed_ristretto: CompressedRistretto
}

pub fn load(env: Env) -> bool {
    rustler::resource!(CompressedRistrettoResource, env);
    true
}

#[rustler::nif(name = "compressed_ristretto_from_slice")]
fn from_slice(bytes: Binary) -> (Atom, ResourceArc<CompressedRistrettoResource>) {
    let compressed_ristretto = CompressedRistretto::from_slice(&hex::decode(bytes.as_slice()).unwrap());
    let res = ResourceArc::new(CompressedRistrettoResource {
        compressed_ristretto
    });

    (ok(), res)
}

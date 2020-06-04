use crate::atoms::ok;
use rustler::{Atom, Env, ResourceArc};
use bulletproofs::PedersenGens;

/// Struct to hold PedersenGens
struct PedersenGensResource {
    pc_gens: PedersenGens
}

pub fn load(env: Env) -> bool {
    rustler::resource!(PedersenGensResource, env);
    true
}

#[rustler::nif(name = "pedersen_gens_default")]
fn default() -> (Atom, ResourceArc<PedersenGensResource>) {
    let res = ResourceArc::new(PedersenGensResource {
        pc_gens: PedersenGens::default()
    });

    (ok(), res)
}

use crate::atoms::ok;
use rustler::{Atom, Env, ResourceArc};
use bulletproofs::BulletproofGens;

/// Struct to hold BulletproofGens
pub struct BulletproofGensResource {
    pub bp_gens: BulletproofGens
}

pub fn load(env: Env) -> bool {
    rustler::resource!(BulletproofGensResource, env);
    true
}

#[rustler::nif(name = "bulletproof_gens_new")]
fn new(gens_capacity: usize, party_capacity: usize) -> (Atom, ResourceArc<BulletproofGensResource>) {
    let res = ResourceArc::new(BulletproofGensResource {
        bp_gens: BulletproofGens::new(gens_capacity, party_capacity)
    });

    (ok(), res)
}

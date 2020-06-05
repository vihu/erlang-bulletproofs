use crate::atoms::ok;
use rustler::{Atom, Env, ResourceArc};
use curve25519_dalek::scalar::Scalar;
use rand::thread_rng;

/// Struct to hold RangeProof
pub struct ScalarResource {
    pub blinding: Scalar
}

pub fn load(env: Env) -> bool {
    rustler::resource!(ScalarResource, env);
    true
}

#[rustler::nif(name = "scalar_random")]
fn random() -> (Atom, ResourceArc<ScalarResource>) {
    let res = ResourceArc::new(ScalarResource {
        blinding: Scalar::random(&mut thread_rng())
    });

    (ok(), res)
}

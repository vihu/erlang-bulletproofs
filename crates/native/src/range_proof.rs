use crate::atoms::ok;
use rustler::{Atom, Env, ResourceArc, Binary, Term};
use bulletproofs::RangeProof;
use hex;

/// Struct to hold RangeProof
struct RangeProofResource {
    rp: RangeProof
}

pub fn load(env: Env) -> bool {
    rustler::resource!(RangeProofResource, env);
    true
}

#[rustler::nif(name = "range_proof_from_bytes")]
fn from_bytes(bytes: Binary) -> (Atom, ResourceArc<RangeProofResource>) {
    let res = ResourceArc::new(RangeProofResource {
        rp: RangeProof::from_bytes(&hex::decode(bytes.as_slice()).unwrap())
            .expect("blow_up_here")
    });

    (ok(), res)
}

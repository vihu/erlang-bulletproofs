use crate::atoms::ok;
use rustler::{Atom, Env, ResourceArc, Binary};
use merlin::Transcript;

/// Struct to hold RangeProof
pub struct TranscriptResource {
    pub transcript: Transcript
}

pub fn load(env: Env) -> bool {
    rustler::resource!(TranscriptResource, env);
    true
}

#[rustler::nif(name = "transcript_new")]
fn new<'a>(bytes: Binary<'a>) -> (Atom, ResourceArc<TranscriptResource>) {
    let res = ResourceArc::new(TranscriptResource {
        transcript: Transcript::new(&bytes)
    });

    (ok(), res)
}

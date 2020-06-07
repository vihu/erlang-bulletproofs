use crate::atoms::{ok, error};
use crate::bulletproof_gens::BulletproofGensResource;
use crate::pedersen_gens::PedersenGensResource;
use crate::transcript::TranscriptResource;
use crate::scalar::ScalarResource;
use crate::compressed_ristretto::CompressedRistrettoResource;
use rustler::{Atom, Env, ResourceArc, Binary};
use bulletproofs::RangeProof;
use curve25519_dalek::ristretto::CompressedRistretto;
use hex;

/// Struct to hold RangeProof
struct RangeProofResource {
    rp: RangeProof,
    committed_value: Option<CompressedRistretto>
}

pub fn load(env: Env) -> bool {
    rustler::resource!(RangeProofResource, env);
    true
}

#[rustler::nif(name = "range_proof_from_bytes")]
fn from_bytes(bytes: Binary) -> (Atom, ResourceArc<RangeProofResource>) {
    let proof = RangeProof::from_bytes(&hex::decode(bytes.as_slice()).unwrap())
        .expect("blow_up_here");
    let res = ResourceArc::new(RangeProofResource {
        rp: proof,
        committed_value: None
    });

    (ok(), res)
}

#[rustler::nif(name = "range_proof_prove_single")]
fn prove_single(
    bp_gens_res: ResourceArc<BulletproofGensResource>,
    pc_gens_res: ResourceArc<PedersenGensResource>,
    transcript_res: ResourceArc<TranscriptResource>,
    v: u64,
    v_blinding_res: ResourceArc<ScalarResource>,
    n: usize) -> (Atom, ResourceArc<RangeProofResource>) {

    let bp_gens = bp_gens_res.bp_gens.clone();
    let pc_gens = pc_gens_res.pc_gens;
    let mut transcript = transcript_res.transcript.clone();
    let blinding = v_blinding_res.blinding;

    let (proof, committed_value) = RangeProof::prove_single(&bp_gens, &pc_gens, &mut transcript, v, &blinding, n).expect("blow_up_here");

    let res = ResourceArc::new(RangeProofResource {
        rp: proof,
        committed_value: Some(committed_value)
    });

    (ok(), res)
}

#[rustler::nif(name = "range_proof_verify_single")]
fn verify_single(
    proof_res: ResourceArc<RangeProofResource>,
    bp_gens_res: ResourceArc<BulletproofGensResource>,
    pc_gens_res: ResourceArc<PedersenGensResource>,
    transcript_res: ResourceArc<TranscriptResource>,
    n: usize) -> bool {

    let bp_gens = bp_gens_res.bp_gens.clone();
    let pc_gens = pc_gens_res.pc_gens;
    let mut verifier_transcript = transcript_res.transcript.clone();

    let proof = proof_res.rp.clone();
    let committed_value = proof_res.committed_value.clone().expect("blow_up_here");

    proof
        .verify_single(&bp_gens, &pc_gens, &mut verifier_transcript, &committed_value, n)
        .is_ok()

}

#[rustler::nif(name = "range_proof_verify_multiple")]
fn verify_multiple(
    proof_res: ResourceArc<RangeProofResource>,
    bp_gens_res: ResourceArc<BulletproofGensResource>,
    pc_gens_res: ResourceArc<PedersenGensResource>,
    transcript_res: ResourceArc<TranscriptResource>,
    vec_value_commitments_res: Vec<ResourceArc<CompressedRistrettoResource>>,
    n: usize) -> Atom {

    let bp_gens = bp_gens_res.bp_gens.clone();
    let pc_gens = pc_gens_res.pc_gens;
    let mut verifier_transcript = transcript_res.transcript.clone();

    let proof = proof_res.rp.clone();

    let mut value_commitments: Vec<CompressedRistretto> = vec![];
    for res in vec_value_commitments_res {
        value_commitments.push(res.compressed_ristretto)
    };

    match proof
        .verify_multiple(
            &bp_gens,
            &pc_gens,
            &mut verifier_transcript,
            &value_commitments,
            n) {
            Ok(()) => ok(),
            _ => error()
        }
}

import { public_, circuitMain, Circuit, Keypair, Proof, VerificationKey } from "../../snarkydist/node/lib/circuit.js";
import { Provable } from "../../snarkydist/node/lib/provable.js";
// func needs to be an UNCURRIED 2 argument function where
// the first arg is the private input(s) and the second is the public
// input(s)
export function mkCircuit_ (func,publicProver,privateProver) {
  const aCircuit  = {
    _main: {main: func, publicInputType: publicProver, privateInputType: privateProver},
    __proto__: Circuit
  }

  const keyPair = aCircuit.generateKeypair();

  console.log(keyPair);

  return aCircuit;
};

export function debug(x) {
  return function() {
    console.log(x);
  }
};

export async function prove_(circ,priv,pub) {
  const k1 = circ.generateKeypair;
  const keyPair = await circ.generateKeypair();
  console.log('keypair');
  console.log(keyPair);
  const p1 = circ.prove;
  const proof = p1.bind(circ);
  // const keyPair = circ.generateKeypair();
  // const proof = circ.prove([priv],[pub],keyPair);
  // console.log(proof)
  return proof(priv,pub,keyPair)
};

export function bindToConstant(field) {
  const f1 = field.toConstant;
  const f2 = f1.bind(field);
  field.toConstant = f2;
  return field;
};

export function proveArray (prover) {
  return Provable.Array(prover,1);
};

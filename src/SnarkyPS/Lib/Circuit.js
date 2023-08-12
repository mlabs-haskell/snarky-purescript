import { public_, circuitMain, Circuit, Keypair, Proof, VerificationKey } from "../../snarkydist/node/lib/circuit.js";
import { Provable } from "../../snarkydist/node/lib/provable.js";
import { Field } from "../../snarkydist/node/lib/field.js";
// func needs to be an UNCURRIED 2 argument function where
// the first arg is the private input(s) and the second is the public
// input(s)
export function mkCircuit_ (func,publicProver,privateProver) {
  const aCircuit  = {
    _main: {main: func, publicInputType: publicProver, privateInputType: privateProver},
    __proto__: Circuit,
  }

  const keyPair = aCircuit.generateKeypair();

  return aCircuit;
};

export function debug(x) {
  return function() {
    console.log(x);
  }
};

export async function prove_(circ,pub,priv) {
  const k1 = circ.generateKeypair;
  const keyPair = await circ.generateKeypair();
  const p1 = circ.prove;
  const proof = p1.bind(circ);
  return proof(priv,pub,keyPair);
};

export function bindToConstant(field) {
  const f1 = field.toConstant;
  const f2 = f1.bind(field);
  field.toConstant = f2;
  return field;
};

export function mkProvable (size) {
  return Provable.Array(Field,size)
};

export function provableSizeInFields(provable) {
  return provable.sizeInFields();
}

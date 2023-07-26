import { public_, circuitMain, Circuit, Keypair, Proof, VerificationKey } from "../../snarkydist/node/lib/circuit.js";

// func needs to be an UNCURRIED 2 argument function where
// the first arg is the private input(s) and the second is the public
// input(s)
export function mkCircuit (func) {
  const MkCircuit = new class extends Circuit {
    static _main = {TODO}
  };
  const result = MkCircuit();
  console.log(result);
  return result;
};

export function debug(x) {
  return function() {
    console.log(x);
  }
};

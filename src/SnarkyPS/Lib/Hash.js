import { Poseidon } from "../../snarkydist/node/lib/hash.js";

export function hashFields(fields){
  return Poseidon.hash(fields)
};

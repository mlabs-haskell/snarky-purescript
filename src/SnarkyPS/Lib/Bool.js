import { Bool } from "../../snarkydist/node/lib/bool.js";

export function newBool(b) {
  return new Bool(b)
};

export function boolToString(b) {
  return b.toString();
};

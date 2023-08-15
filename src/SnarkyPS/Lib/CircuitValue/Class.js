import { Struct } from "../../snarkydist/node/lib/circuit_value.js";
import { HashInput } from "../../snarkydist/node/bindings/lib/provable-snarky.js"
import { Provable } from "../../snarkydist/node/lib/provable.js";
import { Field } from "../../snarkydist/node/lib/field.js";


export function error(x) {
  console.log('ERROR:');
  console.log(x);
};

export function checkEmptyRec(rec) {
  return;
};

export function checkEmptyVar(v) {
  return;
};

export function appendHash(x) {
  return function(y) {
    return HashInput.append(x,y);
  };
};

export function emptyHash() {
  return HashInput.empty
};

export function unsafeHead(arr) {
  return arr[0];
};

export function unsafeIf (size) {
  return function(bool) {
    return function(tBranch) {
      return function(fBranch) {
        return Provable.if(bool,Provable.Array(Field,size),tBranch,fBranch);
      };
    };
  };
};

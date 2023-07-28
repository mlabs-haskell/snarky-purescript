import { Struct } from "../../snarkydist/node/lib/circuit_value.js";
import { HashInput } from "../../snarkydist/node/bindings/lib/provable-snarky.js"

export function error(x) {
  console.log('ERROR:');
  console.log(x);
};

export function checkEmptyRec(rec) {
  return;
};

// Misc functions for working with opaques
export function sizeToInt(size) {
  return size
};

export function intToSize(num) {
  return num
};

export function arrToFields(arr){
  return arr
};

export function fieldsToArr(fields){
  return fields
};

export function appendHash(x) {
  return function(y) {
    return HashInput.append(x,y);
  };
};

export function emptyHash() {
  return HashInput.empty
}

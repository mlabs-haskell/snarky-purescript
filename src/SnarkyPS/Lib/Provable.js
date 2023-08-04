import { Provable } from "../../snarkydist/node/lib/provable.js";


export function zkIf_ (bool) {
  return function(tyProver) {
    return function(tBranch) {
      return function(fBranch) {
        return Provable.if(bool,tyProver,tBranch,fBranch);
      };
    };
  };
};

export function witness_(tyProver) {
  return function(compute) {
    return Provable.witness(tyProver,compute);
  };
};

// switch seems kind of useless atm, but we could use it to build sum types...

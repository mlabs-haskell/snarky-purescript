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


export function zkIfI_ (bool) {
    return function(tBranch) {
      return function(fBranch) {
        return Provable.if(bool,tBranch,fBranch);
    };
  };
};

export function witness_(tyProver) {
  return function(compute) {
    return Provable.witness(tyProver,compute);
  };
};

export function logAndThen(x) {
  return function(res) {
    Provable.asProver( () => console.log(x) );
    return res
  }
};

export function logAndThen_(x) {
  return function(res) {
    Provable.log(x);
    return res
  }
};

export function log(x) {
  Provable.log(x);
  return {};
}

export function log_(x) {
  Provable.asProver( () => console.log(x) );
  return {};
}


// switch seems kind of useless atm, but we could use it to build sum types...

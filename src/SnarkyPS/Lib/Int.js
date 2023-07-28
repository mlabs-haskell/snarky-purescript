import { UInt64, UInt32, Int64 } from "../../snarkydist/node/lib/int.js";


// U64

export function fromFieldU64(i) {
  return UInt64.from(i);
};

export function fromBigIntU64(i) {
  return UInt64.from(i);
};

export function toBigIntU64(i) {
  return i.toBigInt();
};

export function toFieldU64(i) {
  return i.value;
};

export function u64toU32(i) {
  return i.ToUInt32;
};

export function u64ToU32Clamped(i) {
  return i.toUInt32Clamped()
};

export function toInputU64(i) {
  return UInt64.toInput(i);
};

export function toJSONU64(i) {
  return UInt64.toJSON(i);
};

export function fromJSONU64(js) {
  return UInt64.fromJSON(js)
};


export function divModU64(self) {
  return function(other) {
    return self.divMod(other);
  };
};

export function divU64(self) {
  return function(other) {
    return self.div(other);
  };
};

export function modU64(self) {
  return function(other) {
    return self.mod(other);
  };
};

export function addU64(self) {
  return function(other) {
    return self.add(other);
  };
};

export function subU64(self) {
  return function(other) {
    return self.sub(other);
  };
};

export function mulU64(self) {
  return function(other) {
    return self.mul(other);
  };
};

export function ltU64(self) {
  return function (other) {
    return self.lessThan(other);
  };
};

export function lteU64(self) {
  return function (other) {
    return self.lessThanOrEqual(other);
  };
};

export function gtU64(self) {
  return function (other) {
    return self.greaterThan(other);
  };
};

export function gteU64(self) {
  return function (other) {
    return self.greaterThanOrEqual(other);
  };
};

export function assertLtU64(msg) {
  return function (self) {
    return function (other) {
      return self.assertLessThan(other,msg);
    };
  };
};

export function assertLteU64(msg) {
  return function (self) {
    return function (other) {
      return self.assertLessThanOrEqual(other,msg);
    };
  };
};

export function assertGtU64(msg) {
  return function (self) {
    return function (other) {
      return self.assertGreaterThan(other,msg);
    };
  };
};

export function assertGteU64(msg) {
  return function (self) {
    return function (other) {
      return self.assertGreaterThanOrEqual(other,msg);
    };
  };
};

export function checkU64(n) {
    return UInt64.check(n);
};

export function unsafeU64(i) {
  return UInt64.from(i);
};

export function safeU64(i) {
  return unsafeU64(i)
};

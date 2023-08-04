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
  const f1 = i.toUInt32;
  const f2 = f1.bind(i);
  return f2()
};

export function u64ToU32Clamped(i) {
  const f1 = i.toUInt32Clamped;
  const f2 = f1.bind(i);
  return f2()
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
    const f1 = self.divMod;
    const f2 = f1.bind(self,other);
    return f2();
  };
};

export function divU64(self) {
  return function(other) {
    const f1 = self.div;
    const f2 = f1.bind(self,other);
    return f2();
  };
};

export function modU64(self) {
  return function(other) {
    const f1 = self.mod;
    const f2 = f1.bind(self,other);
    return f2()
  };
};

export function addU64(self) {
  return function(other) {
    const f1 = self.add;
    const f2 = f1.bind(self,other);
    return f2();
  };
};

export function subU64(self) {
  return function(other) {
    const f1 = self.sub;
    const f2 = f1.bind(self,other)
    return f2();
  };
};

export function mulU64(self) {
  return function(other) {
    const f1 = self.mul;
    const f2 = f1.bind(self,other);
    return f2();
  };
};

export function ltU64(self) {
  return function (other) {
    const f1 = self.lessThan;
    const f2 = f1.bind(self,other);
    return f2();
  };
};

export function lteU64(self) {
  return function (other) {
    const f1 = self.lessThanOrEqual;
    const f2 = f1.bind(self,other);
    return f2()
  };
};

export function gtU64(self) {
  return function (other) {
    const f1 = self.greaterThan;
    const f2 = f1.bind(self,other);
    return f2()
  };
};

export function gteU64(self) {
  return function (other) {
    const f1 = self.greaterThanOrEqual;
    const f2 = f1.bind(self,other);
    return f2()
  };
};

export function assertLtU64(msg) {
  return function (self) {
    return function (other) {
      const f1 = self.assertLessThan;
      const f2 = f1.bind(self,other);
      return f2()
    };
  };
};

export function assertLteU64(msg) {
  return function (self) {
    return function (other) {
      const f1 = self.assertLessThanOrEqual;
      const f2 = f1.bind(self,other);
      return f2()
    };
  };
};

export function assertGtU64(msg) {
  return function (self) {
    return function (other) {
      const f1 = self.assertGreaterThan;
      const f2 = f1.bind(self,other);
      return f2();
    };
  };
};

export function assertGteU64(msg) {
  return function (self) {
    return function (other) {
      const f1 = self.assertGreaterThanOrEqual;
      const f2 = f1.bind(self,other);
      return f2()
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

import { Field } from "../../snarkydist/node/lib/field.js";
import { UInt64, UInt32, Int64 } from "../../snarkydist/node/lib/int.js";
import { Bool } from "../../snarkydist/node/lib/bool.js";

export function newField(x) {
  const f = Field.from(x);
  return f
};

export function error(msg) {
  throw new Error(msg);
};

export function assertEqField(msg) {
  return function(s) {
    return function (o) {
      return s.assertEquals(o,msg)
    }
  }
};

export function eqField(s) {
  return function(o) {
    return s.equals(o)
  }
};

export function showField(f1) {
  return f1.toString()
};

// U64

export function fromFieldU64(i) {
  return UInt64.from(i);
};

export function fromBigIntU64(i) {
  return UInt64.from(i);
};

export function unsafeU64(i) {
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
  return function () {
    UInt64.check(n);
  };
};

// Field

export function toBigIntField(i) {
  return i.toBigInt();
};

export function coerceToField (i) {
  return new Field(i);
};

// returns a Boolean (not a Bool)
export function isConstantField(i) {
  return i.isConstant();
};

export function toConstantField(i) {
  return i.toConstant();
};

export function negField(i) {
  return i.neg();
};

export function invField(i) {
  return i.inv();
};

//these return Bools
export function isEvenField(i) {
  return i.isEven();
};

export function isOddField(i) {
  return i.isOdd();
};

export function sqrtField(i) {
  return i.sqrt();
};


export function squareField(i) {
  return i.square();
};

export function isZeroField(i) {
  return i.isZero()
};

export function toInputField(i) {
  return Field.toInput(i);
};

export function toJSONField(i) {
  return Field.toJSON(i);
};

export function fromJSONField(i) {
  return Field.toJSON(i);
};

export function divModField(self) {
  return function(other) {
    return self.divMod(other);
  };
};

export function divField(self) {
  return function(other) {
    return self.div(other);
  };
};

export function modField(self) {
  return function(other) {
    return self.mod(other);
  };
};

export function addField(self) {
  return function(other) {
    return self.add(other);
  };
};

export function subField(self) {
  return function(other) {
    return self.sub(other);
  };
};

export function mulField(self) {
  return function(other) {
    return self.mul(other);
  };
};

export function ltField(self) {
  return function (other) {
    return self.lessThan(other);
  };
};

export function gtField(self) {
  return function (other) {
    return self.greaterThan(other);
  };
};

export function lteField(self) {
  return function (other) {
    return self.lessThanOrEqual(other);
  };
};

export function gtField4(self) {
  return function (other) {
    return self.greaterThan(other);
  };
};

export function gteField (self) {
  return function (other) {
    return self.greaterThanOrEqual(other);
  };
};

export function assertLtField(msg) {
  return function (self) {
    return function (other) {
      return self.assertLessThan(other,msg);
    };
  };
};

export function assertLteField(msg) {
  return function (self) {
    return function (other) {
      return self.assertLessThanOrEqual(other,msg);
    };
  };
};

export function assertGtField(msg) {
  return function (self) {
    return function (other) {
      return self.assertGreaterThan(other,msg);
    };
  };
};

export function assertGteField(msg) {
  return function (self) {
    return function (other) {
      return self.assertGreaterThanOrEqual(other,msg);
    };
  };
};


// Bool  (Note that the repetitiveness is intentional, some similarly named functions don't return the same type. We don't *have* types here so duplicating the functions helps avoid mistakes.)

export function fromBoolean(b) {
  return new Bool(b);
};

// Bool -> ?Boolean?
export function isConstantBool(b) {
  return b.isConstant();
};

// Bool -> Field
export function toFieldBool(b) {
  return b.toField();
};

// Bool -> Bool
export function notBool(b) {
  return b.not();
};

// Bool -> Bool -> Bool
export function andBool(b1) {
  return function(b2) {
    return b1.and(b2);
  };
};

export function orBool(b1) {
  return function(b2) {
    return b1.or(b2);
  };
};

// String -> Bool -> Bool -> Bool
export function assertEqBool(msg) {
  return function(b1) {
    return function(b2) {
      return b1.assertEquals(b2);
    };
  };
};

// String -> Bool -> Bool
export function assertTrue(msg) {
  return function(b1) {
    return b1.assertTrue(msg);
  };
};


export function assertFalse(msg) {
  return function(b1) {
    return b1.assertFalse(msg);
  };
};

export function equalsBool(b1) {
  return function(b2) {
    return b1.equals(b2);
  };
};

// Bool -> Int
export function sizeInFieldsBool(b) {
  return b.sizeInFields()
};

// Bool -> Array Field
export function toFieldsBool(b) {
  return b.toFields()
};

// Bool -> String
export function toStringBool(b) {
  return b.toString()
};

export function toBoolean(b) {
  return b.toBoolean();
};

// Bool -> Bool (catch the error? can we?)
export function checkBool(b) {
  return function() {
     Bool.check(b);
  };
};

import { Field } from "../../snarkydist/node/lib/field.js";
import { Bool } from "../../snarkydist/node/lib/bool.js";
import { Bool as B } from "../../snarkydist/node/lib/bool.js";

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

export function unsafeShowField(f1) {
  return f1.toString()
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

export function checkField_(field) {
  return;
};

// Bool

export function toBoolean(b) {
  return b.toBoolean();
};

export function fromBoolean(b) {
  return new Bool(b);
};

// Bool -> Bool
export function notBool(b) {
  return B.b.not();
};

// Bool -> Bool -> Bool
export function andBool(b1) {
  return function(b2) {
    const f1 = b1.and;
    const f2 = f1.bind(b1,b2);
    return f2();
  };
};

export function orBool(b1) {
  return function(b2) {
    const f1 = b1.or;
    const f2 = f1.bind(b1,b2);
    return f2();
  };
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

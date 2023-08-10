import { Bool as B } from "../../snarkydist/node/lib/bool.js";


// Bool -> ?Boolean?
export function isConstantBool(b) {
  const f1 =  B.isConstant.bind(b);
  return f1();
};

// Bool -> Field
export function toFieldBool(b) {
  const f1 = b.toField;
  const f2 = f1.bind(b);
  return f2();
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

// String -> Bool -> Bool -> Bool
export function assertEqBool(msg) {
  return function(b1) {
    return function(b2) {
      const f1 = b1.assertEquals;
      const f2 = f1.bind(b2);
      return f2();
    };
  };
};

// String -> Bool -> Bool
export function assertTrue(msg)  {
  return function(b) {
    return B.assertEqual(b,true)
  };
};


export function assertFalse(msg) {
  return function(b) {
    const f1 = b.assertTrue;
    const f2 = f1.bind(b,msg)
    return f2();
  };
};

export function equalsBool(b1) {
  return function(b2) {
    return B.equal(b1,b2);
  };
};

// Bool -> Int
export function sizeInFieldsBool(b) {
  return b.sizeInFields()
};

// Bool -> Array Field
export function toFieldsBool(b) {
  const f1 = b.toFields;
  const f2 = f1.bind(b);
  return f2()
};

// Bool -> String
export function toStringBool(b) {
  const f1 = b.toString;
  const f2 = f1.bind(b);
  return f2()
};

// Bool -> Bool (catch the error? can we?)
export function checkBool(b) {
     return B.check(b);
};

export function fromFieldBool(fields) {
  return B.fromFields([fields]);
};

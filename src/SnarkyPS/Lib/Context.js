export function fmapContext(f) {
  return function(ta) {
    return f(ta)
  };
};

export function pureContext(a) {
  return a
};

export function lift2Context(f) {
  return function(ca) {
    return function(cb) {
      return f(ca)(cb)
    };
  };
};

export function applyContext(fab) {
  return function(fa) {
    return fab(fa)
  };
};

export function bindContext(ca) {
  return function(f) {
    return f(ca)
  };
};

export function assertMany(xs) {
  return Array.forEach((x) => x);
};

export function assertAndThen(assertion) {
  return function(andThen) {
    assertion
    return andThen;
  }
}

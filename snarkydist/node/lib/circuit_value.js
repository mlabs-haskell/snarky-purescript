import 'reflect-metadata';
import { Field, Bool, Scalar, Group } from './core.js';
import { provable, provablePure, HashInput, } from '../bindings/lib/provable-snarky.js';
import { Provable } from './provable.js';
// external API
export { CircuitValue, prop, arrayProp, matrixProp, provable, provablePure, Struct, };
// internal API
export { cloneCircuitValue, circuitValueEquals, toConstant, isConstant, HashInput, };
/**
 * @deprecated `CircuitValue` is deprecated in favor of {@link Struct}, which features a simpler API and better typing.
 */
class CircuitValue {
    constructor(...props) {
        // if this is called with no arguments, do nothing, to support simple super() calls
        if (props.length === 0)
            return;
        let fields = this.constructor.prototype._fields;
        if (fields === undefined)
            return;
        if (props.length !== fields.length) {
            throw Error(`${this.constructor.name} constructor called with ${props.length} arguments, but expected ${fields.length}`);
        }
        for (let i = 0; i < fields.length; ++i) {
            let [key] = fields[i];
            this[key] = props[i];
        }
    }
    static fromObject(value) {
        return Object.assign(Object.create(this.prototype), value);
    }
    static sizeInFields() {
        const fields = this.prototype._fields;
        return fields.reduce((acc, [_, typ]) => acc + typ.sizeInFields(), 0);
    }
    static toFields(v) {
        const res = [];
        const fields = this.prototype._fields;
        if (fields === undefined || fields === null) {
            return res;
        }
        for (let i = 0, n = fields.length; i < n; ++i) {
            const [key, propType] = fields[i];
            const subElts = propType.toFields(v[key]);
            subElts.forEach((x) => res.push(x));
        }
        return res;
    }
    static toAuxiliary() {
        return [];
    }
    static toInput(v) {
        let input = { fields: [], packed: [] };
        let fields = this.prototype._fields;
        if (fields === undefined)
            return input;
        for (let i = 0, n = fields.length; i < n; ++i) {
            let [key, type] = fields[i];
            if ('toInput' in type) {
                input = HashInput.append(input, type.toInput(v[key]));
                continue;
            }
            // as a fallback, use toFields on the type
            // TODO: this is problematic -- ignores if there's a toInput on a nested type
            // so, remove this? should every provable define toInput?
            let xs = type.toFields(v[key]);
            input.fields.push(...xs);
        }
        return input;
    }
    toFields() {
        return this.constructor.toFields(this);
    }
    toJSON() {
        return this.constructor.toJSON(this);
    }
    toConstant() {
        return this.constructor.toConstant(this);
    }
    equals(x) {
        return Provable.equal(this, x);
    }
    assertEquals(x) {
        Provable.assertEqual(this, x);
    }
    isConstant() {
        return this.toFields().every((x) => x.isConstant());
    }
    static fromFields(xs) {
        const fields = this.prototype._fields;
        if (xs.length < fields.length) {
            throw Error(`${this.name}.fromFields: Expected ${fields.length} field elements, got ${xs?.length}`);
        }
        let offset = 0;
        const props = {};
        for (let i = 0; i < fields.length; ++i) {
            const [key, propType] = fields[i];
            const propSize = propType.sizeInFields();
            const propVal = propType.fromFields(xs.slice(offset, offset + propSize), []);
            props[key] = propVal;
            offset += propSize;
        }
        return Object.assign(Object.create(this.prototype), props);
    }
    static check(v) {
        const fields = this.prototype._fields;
        if (fields === undefined || fields === null) {
            return;
        }
        for (let i = 0; i < fields.length; ++i) {
            const [key, propType] = fields[i];
            const value = v[key];
            if (propType.check === undefined)
                throw Error('bug: CircuitValue without .check()');
            propType.check(value);
        }
    }
    static toConstant(t) {
        const xs = this.toFields(t);
        return this.fromFields(xs.map((x) => x.toConstant()));
    }
    static toJSON(v) {
        const res = {};
        if (this.prototype._fields !== undefined) {
            const fields = this.prototype._fields;
            fields.forEach(([key, propType]) => {
                res[key] = propType.toJSON(v[key]);
            });
        }
        return res;
    }
    static fromJSON(value) {
        let props = {};
        let fields = this.prototype._fields;
        if (typeof value !== 'object' || value === null || Array.isArray(value)) {
            throw Error(`${this.name}.fromJSON(): invalid input ${value}`);
        }
        if (fields !== undefined) {
            for (let i = 0; i < fields.length; ++i) {
                let [key, propType] = fields[i];
                if (value[key] === undefined) {
                    throw Error(`${this.name}.fromJSON(): invalid input ${value}`);
                }
                else {
                    props[key] = propType.fromJSON(value[key]);
                }
            }
        }
        return Object.assign(Object.create(this.prototype), props);
    }
}
function prop(target, key) {
    const fieldType = Reflect.getMetadata('design:type', target, key);
    if (!target.hasOwnProperty('_fields')) {
        target._fields = [];
    }
    if (fieldType === undefined) {
    }
    else if (fieldType.toFields && fieldType.fromFields) {
        target._fields.push([key, fieldType]);
    }
    else {
        console.log(`warning: property ${key} missing field element conversion methods`);
    }
}
function arrayProp(elementType, length) {
    return function (target, key) {
        if (!target.hasOwnProperty('_fields')) {
            target._fields = [];
        }
        target._fields.push([key, Provable.Array(elementType, length)]);
    };
}
function matrixProp(elementType, nRows, nColumns) {
    return function (target, key) {
        if (!target.hasOwnProperty('_fields')) {
            target._fields = [];
        }
        target._fields.push([
            key,
            Provable.Array(Provable.Array(elementType, nColumns), nRows),
        ]);
    };
}
/**
 * `Struct` lets you declare composite types for use in snarkyjs circuits.
 *
 * These composite types can be passed in as arguments to smart contract methods, used for on-chain state variables
 * or as event / action types.
 *
 * Here's an example of creating a "Voter" struct, which holds a public key and a collection of votes on 3 different proposals:
 * ```ts
 * let Vote = { hasVoted: Bool, inFavor: Bool };
 *
 * class Voter extends Struct({
 *   publicKey: PublicKey,
 *   votes: [Vote, Vote, Vote]
 * }) {}
 *
 * // use Voter as SmartContract input:
 * class VoterContract extends SmartContract {
 *   \@method register(voter: Voter) {
 *     // ...
 *   }
 * }
 * ```
 * In this example, there are no instance methods on the class. This makes `Voter` type-compatible with an anonymous object of the form
 * `{ publicKey: PublicKey, votes: Vote[] }`.
 * This mean you don't have to create instances by using `new Voter(...)`, you can operate with plain objects:
 * ```ts
 * voterContract.register({ publicKey, votes });
 * ```
 *
 * On the other hand, you can also add your own methods:
 * ```ts
 * class Voter extends Struct({
 *   publicKey: PublicKey,
 *   votes: [Vote, Vote, Vote]
 * }) {
 *   vote(index: number, inFavor: Bool) {
 *     let vote = this.votes[i];
 *     vote.hasVoted = Bool(true);
 *     vote.inFavor = inFavor;
 *   }
 * }
 * ```
 *
 * In this case, you'll need the constructor to create instances of `Voter`. It always takes as input the plain object:
 * ```ts
 * let emptyVote = { hasVoted: Bool(false), inFavor: Bool(false) };
 * let voter = new Voter({ publicKey, votes: Array(3).fill(emptyVote) });
 * voter.vote(1, Bool(true));
 * ```
 *
 * In addition to creating types composed of Field elements, you can also include auxiliary data which does not become part of the proof.
 * This, for example, allows you to re-use the same type outside snarkyjs methods, where you might want to store additional metadata.
 *
 * To declare non-proof values of type `string`, `number`, etc, you can use the built-in objects `String`, `Number`, etc.
 * Here's how we could add the voter's name (a string) as auxiliary data:
 * ```ts
 * class Voter extends Struct({
 *   publicKey: PublicKey,
 *   votes: [Vote, Vote, Vote],
 *   fullName: String
 * }) {}
 * ```
 *
 * Again, it's important to note that this doesn't enable you to prove anything about the `fullName` string.
 * From the circuit point of view, it simply doesn't exist!
 *
 * @param type Object specifying the layout of the `Struct`
 * @param options Advanced option which allows you to force a certain order of object keys
 * @returns Class which you can extend
 */
function Struct(type, options = {}) {
    class Struct_ {
        constructor(value) {
            Object.assign(this, value);
        }
        /**
         * This method is for internal use, you will probably not need it.
         * @returns the size of this struct in field elements
         */
        static sizeInFields() {
            return this.type.sizeInFields();
        }
        /**
         * This method is for internal use, you will probably not need it.
         * @param value
         * @returns the raw list of field elements that represent this struct inside the proof
         */
        static toFields(value) {
            return this.type.toFields(value);
        }
        /**
         * This method is for internal use, you will probably not need it.
         * @param value
         * @returns the raw non-field element data contained in the struct
         */
        static toAuxiliary(value) {
            return this.type.toAuxiliary(value);
        }
        /**
         * This method is for internal use, you will probably not need it.
         * @param value
         * @returns a representation of this struct as field elements, which can be hashed efficiently
         */
        static toInput(value) {
            return this.type.toInput(value);
        }
        /**
         * Convert this struct to a JSON object, consisting only of numbers, strings, booleans, arrays and plain objects.
         * @param value
         * @returns a JSON representation of this struct
         */
        static toJSON(value) {
            return this.type.toJSON(value);
        }
        /**
         * Convert from a JSON object to an instance of this struct.
         * @param json
         * @returns a JSON representation of this struct
         */
        static fromJSON(json) {
            let value = this.type.fromJSON(json);
            let struct = Object.create(this.prototype);
            return Object.assign(struct, value);
        }
        /**
         * This method is for internal use, you will probably not need it.
         * Method to make assertions which should be always made whenever a struct of this type is created in a proof.
         * @param value
         */
        static check(value) {
            return this.type.check(value);
        }
        /**
         * This method is for internal use, you will probably not need it.
         * Recover a struct from its raw field elements and auxiliary data.
         * @param fields the raw fields elements
         * @param aux the raw non-field element data
         */
        static fromFields(fields, aux) {
            let value = this.type.fromFields(fields, aux);
            let struct = Object.create(this.prototype);
            return Object.assign(struct, value);
        }
    }
    Struct_.type = provable(type, options);
    return Struct_;
}
let primitives = new Set([Field, Bool, Scalar, Group]);
function isPrimitive(obj) {
    for (let P of primitives) {
        if (obj instanceof P)
            return true;
    }
    return false;
}
function cloneCircuitValue(obj) {
    // primitive JS types and functions aren't cloned
    if (typeof obj !== 'object' || obj === null)
        return obj;
    // HACK: callbacks, account udpates
    if (obj.constructor?.name.includes('GenericArgument') ||
        obj.constructor?.name.includes('Callback')) {
        return obj;
    }
    if (obj.constructor?.name.includes('AccountUpdate')) {
        return obj.constructor.clone(obj);
    }
    // built-in JS datatypes with custom cloning strategies
    if (Array.isArray(obj))
        return obj.map(cloneCircuitValue);
    if (obj instanceof Set)
        return new Set([...obj].map(cloneCircuitValue));
    if (obj instanceof Map)
        return new Map([...obj].map(([k, v]) => [k, cloneCircuitValue(v)]));
    if (ArrayBuffer.isView(obj))
        return new obj.constructor(obj);
    // snarkyjs primitives aren't cloned
    if (isPrimitive(obj)) {
        return obj;
    }
    // cloning strategy that works for plain objects AND classes whose constructor only assigns properties
    let propertyDescriptors = {};
    for (let [key, value] of Object.entries(obj)) {
        propertyDescriptors[key] = {
            value: cloneCircuitValue(value),
            writable: true,
            enumerable: true,
            configurable: true,
        };
    }
    return Object.create(Object.getPrototypeOf(obj), propertyDescriptors);
}
function circuitValueEquals(a, b) {
    // primitive JS types and functions are checked for exact equality
    if (typeof a !== 'object' ||
        a === null ||
        typeof b !== 'object' ||
        b === null)
        return a === b;
    // built-in JS datatypes with custom equality checks
    if (Array.isArray(a)) {
        return (Array.isArray(b) &&
            a.length === b.length &&
            a.every((a_, i) => circuitValueEquals(a_, b[i])));
    }
    if (a instanceof Set) {
        return (b instanceof Set && a.size === b.size && [...a].every((a_) => b.has(a_)));
    }
    if (a instanceof Map) {
        return (b instanceof Map &&
            a.size === b.size &&
            [...a].every(([k, v]) => circuitValueEquals(v, b.get(k))));
    }
    if (ArrayBuffer.isView(a) && !(a instanceof DataView)) {
        // typed array
        return (ArrayBuffer.isView(b) &&
            !(b instanceof DataView) &&
            circuitValueEquals([...a], [...b]));
    }
    // the two checks below cover snarkyjs primitives and CircuitValues
    // if we have an .equals method, try to use it
    if ('equals' in a && typeof a.equals === 'function') {
        let isEqual = a.equals(b).toBoolean();
        if (typeof isEqual === 'boolean')
            return isEqual;
        if (isEqual instanceof Bool)
            return isEqual.toBoolean();
    }
    // if we have a .toFields method, try to use it
    if ('toFields' in a &&
        typeof a.toFields === 'function' &&
        'toFields' in b &&
        typeof b.toFields === 'function') {
        let aFields = a.toFields();
        let bFields = b.toFields();
        return aFields.every((a, i) => a.equals(bFields[i]).toBoolean());
    }
    // equality test that works for plain objects AND classes whose constructor only assigns properties
    let aEntries = Object.entries(a).filter(([, v]) => v !== undefined);
    let bEntries = Object.entries(b).filter(([, v]) => v !== undefined);
    if (aEntries.length !== bEntries.length)
        return false;
    return aEntries.every(([key, value]) => key in b && circuitValueEquals(b[key], value));
}
function toConstant(type, value) {
    return type.fromFields(type.toFields(value).map((x) => x.toConstant()), type.toAuxiliary(value));
}
function isConstant(type, value) {
    return type.toFields(value).every((x) => x.isConstant());
}
//# sourceMappingURL=circuit_value.js.map
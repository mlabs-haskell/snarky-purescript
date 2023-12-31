/**
 * {@link Provable} is
 * - a namespace with tools for writing provable code
 * - the main interface for types that can be used in provable code
 */
import { Field, Bool } from './core.js';
import { Provable as Provable_ } from '../snarky.js';
import type { FlexibleProvable } from './circuit_value.js';
import { Context } from './global-context.js';
import { InferredProvable } from '../bindings/lib/provable-snarky.js';
import { inCheckedComputation, inProver, asProver, runAndCheck, runUnchecked, constraintSystem } from './provable-context.js';
export { Provable };
export { memoizationContext, MemoizationContext, memoizeWitness, getBlindingValue, };
/**
 * `Provable<T>` is the general circuit type interface. It describes how a type `T` is made up of field elements and auxiliary (non-field element) data.
 *
 * You will find this as the required input type in a few places in SnarkyJS. One convenient way to create a `Provable<T>` is using `Struct`.
 */
type Provable<T> = Provable_<T>;
declare const Provable: {
    /**
     * Create a new witness. A witness, or variable, is a value that is provided as input
     * by the prover. This provides a flexible way to introduce values from outside into the circuit.
     * However, note that nothing about how the value was created is part of the proof - `Provable.witness`
     * behaves exactly like user input. So, make sure that after receiving the witness you make any assertions
     * that you want to associate with it.
     * @example
     * Example for re-implementing `Field.inv` with the help of `witness`:
     * ```ts
     * let invX = Provable.witness(Field, () => {
     *   // compute the inverse of `x` outside the circuit, however you like!
     *   return Field.inv(x));
     * }
     * // prove that `invX` is really the inverse of `x`:
     * invX.mul(x).assertEquals(1);
     * ```
     */
    witness: typeof witness;
    /**
     * Proof-compatible if-statement.
     * This behaves like a ternary conditional statement in JS.
     *
     * **Warning**: Since `Provable.if()` is a normal JS function call, both the if and the else branch
     * are evaluated before calling it. Therefore, you can't use this function
     * to guard against execution of one of the branches. It only allows you to pick one of two values.
     *
     * @example
     * ```ts
     * const condition = Bool(true);
     * const result = Provable.if(condition, Field(1), Field(2)); // returns Field(1)
     * ```
     */
    if: typeof if_;
    /**
     * Generalization of {@link Provable.if} for choosing between more than two different cases.
     * It takes a "mask", which is an array of `Bool`s that contains only one `true` element, a type/constructor, and an array of values of that type.
     * The result is that value which corresponds to the true element of the mask.
     * @example
     * ```ts
     * let x = Provable.switch([Bool(false), Bool(true)], Field, [Field(1), Field(2)]);
     * x.assertEquals(2);
     * ```
     */
    switch: typeof switch_;
    /**
     * Asserts that two values are equal.
     * @example
     * ```ts
     * class MyStruct extends Struct({ a: Field, b: Bool }) {};
     * const a: MyStruct = { a: Field(0), b: Bool(false) };
     * const b: MyStruct = { a: Field(1), b: Bool(true) };
     * Provable.assertEqual(MyStruct, a, b);
     * ```
     */
    assertEqual: typeof assertEqual;
    /**
     * Checks if two elements are equal.
     * @example
     * ```ts
     * class MyStruct extends Struct({ a: Field, b: Bool }) {};
     * const a: MyStruct = { a: Field(0), b: Bool(false) };
     * const b: MyStruct = { a: Field(1), b: Bool(true) };
     * const isEqual = Provable.equal(MyStruct, a, b);
     * ```
     */
    equal: typeof equal;
    /**
     * Creates a {@link Provable} for a generic array.
     * @example
     * ```ts
     * const ProvableArray = Provable.Array(Field, 5);
     * ```
     */
    Array: typeof provableArray;
    /**
     * Interface to log elements within a circuit. Similar to `console.log()`.
     * @example
     * ```ts
     * const element = Field(42);
     * Provable.log(element);
     * ```
     */
    log: typeof log;
    /**
     * Runs code as a prover.
     * @example
     * ```ts
     * Provable.asProver(() => {
     *   // Your prover code here
     * });
     * ```
     */
    asProver: typeof asProver;
    /**
     * Runs provable code quickly, without creating a proof, but still checking whether constraints are satisfied.
     * @example
     * ```ts
     * Provable.runAndCheck(() => {
     *   // Your code to check here
     * });
     * ```
     */
    runAndCheck: typeof runAndCheck;
    /**
     * Runs provable code quickly, without creating a proof, and not checking whether constraints are satisfied.
     * @example
     * ```ts
     * Provable.runUnchecked(() => {
     *   // Your code to run here
     * });
     * ```
     */
    runUnchecked: typeof runUnchecked;
    /**
     * Returns information about the constraints created by the callback function.
     * @example
     * ```ts
     * const result = Provable.constraintSystem(circuit);
     * console.log(result);
     * ```
     */
    constraintSystem: typeof constraintSystem;
    /**
     * Checks if the code is run in prover mode.
     * @example
     * ```ts
     * if (Provable.inProver()) {
     *   // Prover-specific code
     * }
     * ```
     */
    inProver: typeof inProver;
    /**
     * Checks if the code is run in checked computation mode.
     * @example
     * ```ts
     * if (Provable.inCheckedComputation()) {
     *   // Checked computation-specific code
     * }
     * ```
     */
    inCheckedComputation: typeof inCheckedComputation;
};
declare function witness<T, S extends FlexibleProvable<T> = FlexibleProvable<T>>(type: S, compute: () => T): T;
type ToFieldable = {
    toFields(): Field[];
};
declare function assertEqual<T>(type: FlexibleProvable<T>, x: T, y: T): void;
declare function assertEqual<T extends ToFieldable>(x: T, y: T): void;
declare function equal<T>(type: FlexibleProvable<T>, x: T, y: T): Bool;
declare function equal<T extends ToFieldable>(x: T, y: T): Bool;
declare function if_<T>(condition: Bool, type: FlexibleProvable<T>, x: T, y: T): T;
declare function if_<T extends ToFieldable>(condition: Bool, x: T, y: T): T;
declare function switch_<T, A extends FlexibleProvable<T>>(mask: Bool[], type: A, values: T[]): T;
declare function log(...args: any): void;
type MemoizationContext = {
    memoized: {
        fields: Field[];
        aux: any[];
    }[];
    currentIndex: number;
    blindingValue: Field;
};
declare let memoizationContext: Context.t<MemoizationContext>;
/**
 * Like Provable.witness, but memoizes the witness during transaction construction
 * for reuse by the prover. This is needed to witness non-deterministic values.
 */
declare function memoizeWitness<T>(type: FlexibleProvable<T>, compute: () => T): T;
declare function getBlindingValue(): import("./field.js").Field;
declare function provableArray<A extends FlexibleProvable<any>>(elementType: A, length: number): InferredProvable<A[]>;

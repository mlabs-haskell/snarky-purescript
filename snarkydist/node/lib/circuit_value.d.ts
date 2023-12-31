import 'reflect-metadata';
import { ProvablePure } from '../snarky.js';
import { Field } from './core.js';
import { provable, provablePure, HashInput, NonMethods } from '../bindings/lib/provable-snarky.js';
import type { InferJson, InferProvable, InferredProvable, IsPure } from '../bindings/lib/provable-snarky.js';
import { Provable } from './provable.js';
export { CircuitValue, ProvableExtended, ProvablePureExtended, prop, arrayProp, matrixProp, provable, provablePure, Struct, FlexibleProvable, FlexibleProvablePure, };
export { AnyConstructor, cloneCircuitValue, circuitValueEquals, toConstant, isConstant, InferProvable, HashInput, InferJson, InferredProvable, };
type ProvableExtension<T, TJson = any> = {
    toInput: (x: T) => {
        fields?: Field[];
        packed?: [Field, number][];
    };
    toJSON: (x: T) => TJson;
    fromJSON: (x: TJson) => T;
};
type ProvableExtended<T, TJson = any> = Provable<T> & ProvableExtension<T, TJson>;
type ProvablePureExtended<T, TJson = any> = ProvablePure<T> & ProvableExtension<T, TJson>;
type Struct<T> = ProvableExtended<NonMethods<T>> & Constructor<T> & {
    _isStruct: true;
};
type StructPure<T> = ProvablePure<NonMethods<T>> & ProvableExtension<NonMethods<T>> & Constructor<T> & {
    _isStruct: true;
};
type FlexibleProvable<T> = Provable<T> | Struct<T>;
type FlexibleProvablePure<T> = ProvablePure<T> | StructPure<T>;
type Constructor<T> = new (...args: any) => T;
type AnyConstructor = Constructor<any>;
/**
 * @deprecated `CircuitValue` is deprecated in favor of {@link Struct}, which features a simpler API and better typing.
 */
declare abstract class CircuitValue {
    constructor(...props: any[]);
    static fromObject<T extends AnyConstructor>(this: T, value: NonMethods<InstanceType<T>>): InstanceType<T>;
    static sizeInFields(): number;
    static toFields<T extends AnyConstructor>(this: T, v: InstanceType<T>): Field[];
    static toAuxiliary(): [];
    static toInput<T extends AnyConstructor>(this: T, v: InstanceType<T>): HashInput;
    toFields(): Field[];
    toJSON(): any;
    toConstant(): this;
    equals(x: this): import("./bool.js").Bool;
    assertEquals(x: this): void;
    isConstant(): boolean;
    static fromFields<T extends AnyConstructor>(this: T, xs: Field[]): InstanceType<T>;
    static check<T extends AnyConstructor>(this: T, v: InstanceType<T>): void;
    static toConstant<T extends AnyConstructor>(this: T, t: InstanceType<T>): InstanceType<T>;
    static toJSON<T extends AnyConstructor>(this: T, v: InstanceType<T>): any;
    static fromJSON<T extends AnyConstructor>(this: T, value: any): InstanceType<T>;
}
declare function prop(this: any, target: any, key: string): void;
declare function arrayProp<T>(elementType: FlexibleProvable<T>, length: number): (target: any, key: string) => void;
declare function matrixProp<T>(elementType: FlexibleProvable<T>, nRows: number, nColumns: number): (target: any, key: string) => void;
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
declare function Struct<A, T extends InferProvable<A> = InferProvable<A>, J extends InferJson<A> = InferJson<A>, Pure extends boolean = IsPure<A>>(type: A, options?: {
    customObjectKeys?: string[];
}): (new (value: T) => T) & {
    _isStruct: true;
} & (Pure extends true ? ProvablePure<T> : Provable<T>) & {
    toInput: (x: T) => {
        fields?: Field[] | undefined;
        packed?: [Field, number][] | undefined;
    };
    toJSON: (x: T) => J;
    fromJSON: (x: J) => T;
};
declare function cloneCircuitValue<T>(obj: T): T;
declare function circuitValueEquals<T>(a: T, b: T): boolean;
declare function toConstant<T>(type: FlexibleProvable<T>, value: T): T;
declare function isConstant<T>(type: FlexibleProvable<T>, value: T): boolean;

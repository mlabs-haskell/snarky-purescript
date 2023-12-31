import { Field } from './core.js';
import { AnyConstructor, CircuitValue } from './circuit_value.js';
import { Types } from '../bindings/mina-transaction/types.js';
import { HashInput } from './hash.js';
export { UInt32, UInt64, Int64, Sign };
/**
 * A 64 bit unsigned integer with values ranging from 0 to 18,446,744,073,709,551,615.
 */
declare class UInt64 extends CircuitValue {
    value: Field;
    static NUM_BITS: number;
    /**
     * Static method to create a {@link UInt64} with value `0`.
     */
    static get zero(): UInt64;
    /**
     * Static method to create a {@link UInt64} with value `1`.
     */
    static get one(): UInt64;
    /**
     * Turns the {@link UInt64} into a string.
     * @returns
     */
    toString(): string;
    /**
     * Turns the {@link UInt64} into a {@link BigInt}.
     * @returns
     */
    toBigInt(): bigint;
    /**
     * Turns the {@link UInt64} into a {@link UInt32}, asserting that it fits in 32 bits.
     */
    toUInt32(): UInt32;
    /**
     * Turns the {@link UInt64} into a {@link UInt32}, clamping to the 32 bits range if it's too large.
     * ```ts
     * UInt64.from(4294967296).toUInt32Clamped().toString(); // "4294967295"
     * ```
     */
    toUInt32Clamped(): UInt32;
    static check(x: UInt64): void;
    static toInput(x: UInt64): HashInput;
    /**
     * Encodes this structure into a JSON-like object.
     */
    static toJSON(x: UInt64): string;
    /**
     * Decodes a JSON-like object into this structure.
     */
    static fromJSON<T extends AnyConstructor>(x: string): InstanceType<T>;
    private static checkConstant;
    /**
     * Creates a new {@link UInt64}.
     */
    static from(x: UInt64 | UInt32 | Field | number | string | bigint): UInt64;
    /**
     * Creates a {@link UInt64} with a value of 18,446,744,073,709,551,615.
     */
    static MAXINT(): UInt64;
    /**
     * Integer division with remainder.
     *
     * `x.divMod(y)` returns the quotient and the remainder.
     */
    divMod(y: UInt64 | number | string): {
        quotient: UInt64;
        rest: UInt64;
    };
    /**
     * Integer division.
     *
     * `x.div(y)` returns the floor of `x / y`, that is, the greatest
     * `z` such that `z * y <= x`.
     *
     */
    div(y: UInt64 | number): UInt64;
    /**
     * Integer remainder.
     *
     * `x.mod(y)` returns the value `z` such that `0 <= z < y` and
     * `x - z` is divisble by `y`.
     */
    mod(y: UInt64 | number): UInt64;
    /**
     * Multiplication with overflow checking.
     */
    mul(y: UInt64 | number): UInt64;
    /**
     * Addition with overflow checking.
     */
    add(y: UInt64 | number): UInt64;
    /**
     * Subtraction with underflow checking.
     */
    sub(y: UInt64 | number): UInt64;
    /**
     * @deprecated Use {@link lessThanOrEqual} instead.
     *
     * Checks if a {@link UInt64} is less than or equal to another one.
     */
    lte(y: UInt64): import("./bool.js").Bool;
    /**
     * Checks if a {@link UInt64} is less than or equal to another one.
     */
    lessThanOrEqual(y: UInt64): import("./bool.js").Bool;
    /**
     * @deprecated Use {@link assertLessThanOrEqual} instead.
     *
     * Asserts that a {@link UInt64} is less than or equal to another one.
     */
    assertLte(y: UInt64, message?: string): void;
    /**
     * Asserts that a {@link UInt64} is less than or equal to another one.
     */
    assertLessThanOrEqual(y: UInt64, message?: string): void;
    /**
     * @deprecated Use {@link lessThan} instead.
     *
     * Checks if a {@link UInt64} is less than another one.
     */
    lt(y: UInt64): import("./bool.js").Bool;
    /**
     *
     * Checks if a {@link UInt64} is less than another one.
     */
    lessThan(y: UInt64): import("./bool.js").Bool;
    /**
     *
     * @deprecated Use {@link assertLessThan} instead.
     *
     * Asserts that a {@link UInt64} is less than another one.
     */
    assertLt(y: UInt64, message?: string): void;
    /**
     * Asserts that a {@link UInt64} is less than another one.
     */
    assertLessThan(y: UInt64, message?: string): void;
    /**
     * @deprecated Use {@link greaterThan} instead.
     *
     * Checks if a {@link UInt64} is greater than another one.
     */
    gt(y: UInt64): import("./bool.js").Bool;
    /**
     * Checks if a {@link UInt64} is greater than another one.
     */
    greaterThan(y: UInt64): import("./bool.js").Bool;
    /**
     * @deprecated Use {@link assertGreaterThan} instead.
     *
     * Asserts that a {@link UInt64} is greater than another one.
     */
    assertGt(y: UInt64, message?: string): void;
    /**
     * Asserts that a {@link UInt64} is greater than another one.
     */
    assertGreaterThan(y: UInt64, message?: string): void;
    /**
     * @deprecated Use {@link greaterThanOrEqual} instead.
     *
     * Checks if a {@link UInt64} is greater than or equal to another one.
     */
    gte(y: UInt64): import("./bool.js").Bool;
    /**
     * Checks if a {@link UInt64} is greater than or equal to another one.
     */
    greaterThanOrEqual(y: UInt64): import("./bool.js").Bool;
    /**
     * @deprecated Use {@link assertGreaterThanOrEqual} instead.
     *
     * Asserts that a {@link UInt64} is greater than or equal to another one.
     */
    assertGte(y: UInt64, message?: string): void;
    /**
     * Asserts that a {@link UInt64} is greater than or equal to another one.
     */
    assertGreaterThanOrEqual(y: UInt64, message?: string): void;
}
/**
 * A 32 bit unsigned integer with values ranging from 0 to 4,294,967,295.
 */
declare class UInt32 extends CircuitValue {
    value: Field;
    static NUM_BITS: number;
    /**
     * Static method to create a {@link UInt32} with value `0`.
     */
    static get zero(): UInt32;
    /**
     * Static method to create a {@link UInt32} with value `0`.
     */
    static get one(): UInt32;
    /**
     * Turns the {@link UInt32} into a string.
     */
    toString(): string;
    /**
     * Turns the {@link UInt32} into a {@link BigInt}.
     */
    toBigint(): bigint;
    /**
     * Turns the {@link UInt32} into a {@link UInt64}.
     */
    toUInt64(): UInt64;
    static check(x: UInt32): void;
    static toInput(x: UInt32): HashInput;
    /**
     * Encodes this structure into a JSON-like object.
     */
    static toJSON(x: UInt32): string;
    /**
     * Decodes a JSON-like object into this structure.
     */
    static fromJSON<T extends AnyConstructor>(x: string): InstanceType<T>;
    private static checkConstant;
    /**
     * Creates a new {@link UInt32}.
     */
    static from(x: UInt32 | Field | number | string | bigint): UInt32;
    /**
     * Creates a {@link UInt32} with a value of 4,294,967,295.
     */
    static MAXINT(): UInt32;
    /**
     * Integer division with remainder.
     *
     * `x.divMod(y)` returns the quotient and the remainder.
     */
    divMod(y: UInt32 | number | string): {
        quotient: UInt32;
        rest: UInt32;
    };
    /**
     * Integer division.
     *
     * `x.div(y)` returns the floor of `x / y`, that is, the greatest
     * `z` such that `x * y <= x`.
     *
     */
    div(y: UInt32 | number): UInt32;
    /**
     * Integer remainder.
     *
     * `x.mod(y)` returns the value `z` such that `0 <= z < y` and
     * `x - z` is divisble by `y`.
     */
    mod(y: UInt32 | number): UInt32;
    /**
     * Multiplication with overflow checking.
     */
    mul(y: UInt32 | number): UInt32;
    /**
     * Addition with overflow checking.
     */
    add(y: UInt32 | number): UInt32;
    /**
     * Subtraction with underflow checking.
     */
    sub(y: UInt32 | number): UInt32;
    /**
     * @deprecated Use {@link lessThanOrEqual} instead.
     *
     * Checks if a {@link UInt32} is less than or equal to another one.
     */
    lte(y: UInt32): import("./bool.js").Bool;
    /**
     * Checks if a {@link UInt32} is less than or equal to another one.
     */
    lessThanOrEqual(y: UInt32): import("./bool.js").Bool;
    /**
     * @deprecated Use {@link assertLessThanOrEqual} instead.
     *
     * Asserts that a {@link UInt32} is less than or equal to another one.
     */
    assertLte(y: UInt32, message?: string): void;
    /**
     * Asserts that a {@link UInt32} is less than or equal to another one.
     */
    assertLessThanOrEqual(y: UInt32, message?: string): void;
    /**
     * @deprecated Use {@link lessThan} instead.
     *
     * Checks if a {@link UInt32} is less than another one.
     */
    lt(y: UInt32): import("./bool.js").Bool;
    /**
     * Checks if a {@link UInt32} is less than another one.
     */
    lessThan(y: UInt32): import("./bool.js").Bool;
    /**
     * @deprecated Use {@link assertLessThan} instead.
     *
     * Asserts that a {@link UInt32} is less than another one.
     */
    assertLt(y: UInt32, message?: string): void;
    /**
     * Asserts that a {@link UInt32} is less than another one.
     */
    assertLessThan(y: UInt32, message?: string): void;
    /**
     * @deprecated Use {@link greaterThan} instead.
     *
     * Checks if a {@link UInt32} is greater than another one.
     */
    gt(y: UInt32): import("./bool.js").Bool;
    /**
     * Checks if a {@link UInt32} is greater than another one.
     */
    greaterThan(y: UInt32): import("./bool.js").Bool;
    /**
     * @deprecated Use {@link assertGreaterThan} instead.
     *
     * Asserts that a {@link UInt32} is greater than another one.
     */
    assertGt(y: UInt32, message?: string): void;
    /**
     * Asserts that a {@link UInt32} is greater than another one.
     */
    assertGreaterThan(y: UInt32, message?: string): void;
    /**
     * @deprecated Use {@link greaterThanOrEqual} instead.
     *
     * Checks if a {@link UInt32} is greater than or equal to another one.
     */
    gte(y: UInt32): import("./bool.js").Bool;
    /**
     * Checks if a {@link UInt32} is greater than or equal to another one.
     */
    greaterThanOrEqual(y: UInt32): import("./bool.js").Bool;
    /**
     * @deprecated Use {@link assertGreaterThanOrEqual} instead.
  
     *
     * Asserts that a {@link UInt32} is greater than or equal to another one.
     */
    assertGte(y: UInt32, message?: string): void;
    /**
     * Asserts that a {@link UInt32} is greater than or equal to another one.
     */
    assertGreaterThanOrEqual(y: UInt32, message?: string): void;
}
declare class Sign extends CircuitValue {
    value: Field;
    static get one(): Sign;
    static get minusOne(): Sign;
    static check(x: Sign): void;
    static emptyValue(): Sign;
    static toInput(x: Sign): HashInput;
    static toJSON(x: Sign): "Positive" | "Negative";
    static fromJSON<T extends AnyConstructor>(x: 'Positive' | 'Negative'): InstanceType<T>;
    neg(): Sign;
    mul(y: Sign): Sign;
    isPositive(): import("./bool.js").Bool;
    toString(): string;
}
type BalanceChange = Types.AccountUpdate['body']['balanceChange'];
/**
 * A 64 bit signed integer with values ranging from -18,446,744,073,709,551,615 to 18,446,744,073,709,551,615.
 */
declare class Int64 extends CircuitValue implements BalanceChange {
    magnitude: UInt64;
    sgn: Sign;
    constructor(magnitude: UInt64, sgn?: Sign);
    /**
     * Creates a new {@link Int64} from a {@link Field}.
     *
     * Does check if the {@link Field} is within range.
     */
    private static fromFieldUnchecked;
    /**
     * Creates a new {@link Int64} from a {@link Field}.
     *
     * **Does not** check if the {@link Field} is within range.
     */
    static fromUnsigned(x: UInt64 | UInt32): Int64;
    /**
     * Creates a new {@link Int64}.
     *
     * Check the range if the argument is a constant.
     */
    static from(x: Int64 | UInt32 | UInt64 | Field | number | string | bigint): Int64;
    /**
     * Turns the {@link Int64} into a string.
     */
    toString(): string;
    isConstant(): boolean;
    /**
     * Static method to create a {@link Int64} with value `0`.
     */
    static get zero(): Int64;
    /**
     * Static method to create a {@link Int64} with value `1`.
     */
    static get one(): Int64;
    /**
     * Static method to create a {@link Int64} with value `-1`.
     */
    static get minusOne(): Int64;
    /**
     * Returns the {@link Field} value.
     */
    toField(): import("./field.js").Field;
    /**
     * Static method to create a {@link Int64} from a {@link Field}.
     */
    static fromField(x: Field): Int64;
    /**
     * Negates the value.
     *
     * `Int64.from(5).neg()` will turn into `Int64.from(-5)`
     */
    neg(): Int64;
    /**
     * Addition with overflow checking.
     */
    add(y: Int64 | number | string | bigint | UInt64 | UInt32): Int64;
    /**
     * Subtraction with underflow checking.
     */
    sub(y: Int64 | number | string | bigint | UInt64 | UInt32): Int64;
    /**
     * Multiplication with overflow checking.
     */
    mul(y: Int64 | number | string | bigint | UInt64 | UInt32): Int64;
    /**
     * Integer division.
     *
     * `x.div(y)` returns the floor of `x / y`, that is, the greatest
     * `z` such that `z * y <= x`.
     *
     */
    div(y: Int64 | number | string | bigint | UInt64 | UInt32): Int64;
    /**
     * Integer remainder.
     *
     * `x.mod(y)` returns the value `z` such that `0 <= z < y` and
     * `x - z` is divisble by `y`.
     */
    mod(y: UInt64 | number | string | bigint | UInt32): Int64;
    /**
     * Checks if two values are equal.
     */
    equals(y: Int64 | number | string | bigint | UInt64 | UInt32): import("./bool.js").Bool;
    /**
     * Asserts that two values are equal.
     */
    assertEquals(y: Int64 | number | string | bigint | UInt64 | UInt32, message?: string): void;
    /**
     * Checks if the value is postive.
     */
    isPositive(): import("./bool.js").Bool;
}
